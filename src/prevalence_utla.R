library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)

get_data_from_gov_api <- function(request_url) {
  res = GET(request_url)
  data <- res$content %>%
    rawToChar() %>%
    fread %>%
    as_tibble()
  return(data)
}


get_adjusted_prevalence <- function(data) {
  # Estimate true cases using method from COVID-19 Projections
  # https://covid19-projections.com/estimating-true-infections-revisited/
  
  # Day 0 is the 12th Feb 2020, 14 days prior to known community transmission in Edinburgh on 26th Feb
  MAX_DELAY_FACTOR = 2
  
  df <- data %>%
    merge(df_all_population, by="areaCode") %>%
    group_by(areaCode) %>%
    arrange(date) %>%
    mutate(
      day_i = as.numeric(as.Date(date) - as.Date("2020-02-12")),
      newCases7da = rollmean(newCasesBySpecimenDate, k=7, align="right", fill=NA),
      newTests7da = rollmean(newVirusTestsBySpecimenDate, k=7, align="right", fill=NA),
      newCasesWeekPercentage = (
        rollsum(newCasesBySpecimenDate, k=7, align="right", fill=NA) -
          rollsum(lag(newCasesBySpecimenDate, n=7), k=7, align="right", fill=NA)) /
        rollsum(lag(newCasesBySpecimenDate, n=7), k=7, align="right", fill=NA)
    ) %>%
    ungroup() %>%
    
    mutate(
      positivityRate = ifelse(newTests7da>0, newCases7da/newTests7da, NA),
      underreportingFactor = (1500/(day_i+10)) * positivityRate**0.5 +2,
      delayFactor = 1+ ifelse(newCasesWeekPercentage < 0, 0,
                              ifelse(newCasesWeekPercentage > MAX_DELAY_FACTOR, MAX_DELAY_FACTOR,
                                     newCasesWeekPercentage)),
      trueNewCases = ifelse(as.numeric(Sys.Date() - as.Date(date)) <= 6,
                            newCases7da * underreportingFactor * delayFactor,
                            newCases7da * underreportingFactor)
    ) %>%
    
    group_by(areaCode) %>%
    arrange(date) %>%
    mutate(
      prevalenceRate = rollsum(trueNewCases, k=7, fill=NA, align="right") /population
    ) %>%
    ungroup()
  
  return(df)
}


get_most_recent <- function(df) {
  df_most_recent <- df %>%
    filter(!is.na(prevalenceRate)) %>%
    group_by(areaName) %>%
    slice(which.max(date)) %>%
    as_tibble()
  return(df_most_recent)
}


get_top_N <- function(df_most_recent, n) {
  df_top_N <- df_most_recent %>%
    slice_max(prevalenceRate, n=n) %>%
    select(areaName)
  return(df_top_N$areaName)
}



get_plot_order <- function(df_most_recent) {
  order <- df_most_recent %>%
    arrange(desc(prevalenceRate)) %>%
    select(areaName)
  return(order$areaName)
}

# Import data

data_utla = get_data_from_gov_api(
  paste(
    "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla",
    "&metric=newCasesBySpecimenDate",
    "&metric=newVirusTestsBySpecimenDate",
    "&format=csv",
    sep="")
)

data_population <- "https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv" %>%
  fread %>%
  as_tibble()

# Get population sizes
df_all_population <- data_population %>%
  filter(category=="ALL") %>%
  select(areaCode, population)

# Arrange data

utla <- get_adjusted_prevalence(data_utla)
utla_most_recent <- get_most_recent(utla)
utla_top_N <- get_top_N(utla_most_recent, n=25)
utla_order <- get_plot_order(utla_most_recent)

utla_plot <- utla
utla_plot$areaNameOrdered <- factor(utla_plot$areaName, levels=utla_order)
utla_plot <- utla_plot %>%
  filter(areaName %in% utla_top_N)

utla_most_recent$areaNameOrdered <- factor(utla_most_recent$areaName, levels=utla_order)
utla_most_recent <- utla_most_recent %>%
  filter(areaName %in% utla_top_N)

latest_date <- max(utla$date)



color_code = "#0070C0"

ggplot(utla_plot, aes(x=date, y=prevalenceRate, color=areaNameOrdered)) +
  geom_line(
    size=1,
    color=color_code
  ) +
  
  geom_point(
    data=utla_most_recent,
    size=3,
    color=color_code,
  ) +
  
  gghighlight(
    unhighlighted_params = list(
      size = 1,
      alpha=0.5
    )
  ) +
  
  geom_text(
    data=utla_most_recent, 
    aes(label=scales::percent(prevalenceRate,0.01)),
    hjust=1,
    vjust=0.5,
    #nudge_y=0.0025,
    nudge_x=-3,
    color=color_code,
    fontface="bold"
  ) +
  
  facet_wrap(~areaNameOrdered) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%d %b"),
    limits = as.Date(c('2021-11-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1)),
    rect = element_rect(fill = "transparent")
  ) +
  
  labs(
    title="Estimated prevalence of COVID-19 in the UK, by local authority",
    subtitle="Using adjustments to compensate for delays and underreporting",
    caption=paste(
      "Data from UK Health Security Agency (coronavirus.data.gov.uk) and ONS; adjustment method from microcovid.org\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )

ggsave(paste("plots/prevalence_by_utla.png", sep=""), width=10,height=8)


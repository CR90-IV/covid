library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)
library(plotly)
library(gganimate)

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

data_region = get_data_from_gov_api(
  paste(
    "https://api.coronavirus.data.gov.uk/v2/data?areaType=region",
    "&metric=newCasesBySpecimenDate",
    "&metric=newVirusTestsBySpecimenDate",
    "&format=csv",
    sep="")
)

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

region <- get_adjusted_prevalence(data_region)
region_most_recent <- get_most_recent(region)
region_order <- get_plot_order(region_most_recent)
region$areaNameOrdered <- factor(region$areaName, levels=region_order)
region_most_recent$areaNameOrdered <- factor(region_most_recent$areaName, levels=region_order)


color_code = "#0070C0"

ggplot(region, aes(x=date, y=prevalenceRate, color=areaNameOrdered)) +
  geom_line(
    size=1,
    color=color_code
  ) +
  
  geom_point(
    data=region_most_recent,
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
    data=region_most_recent, 
    aes(label=scales::percent(prevalenceRate,0.01)),
    hjust=0.5,
    vjust=0,
    nudge_y=0.005,
    color=color_code,
    fontface="bold"
  ) +
  
  facet_wrap(~areaNameOrdered) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%b %Y"),
    limits = as.Date(c('2021-10-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1))
    
  ) +
  
  labs(
    title="Estimated prevalence of COVID-19 in the UK",
    subtitle="Using adjustments to compensate for delays and underreporting",
    caption=paste(
      "Data from UK Health Security Agency (coronavirus.data.gov.uk) and ONS; adjustment method from microcovid.org\nProduced",
      Sys.Date()
    )
  )

ggsave(paste("plots/prevalence_by_region_", Sys.Date(), ".png", sep=""), width=10,height=8)


utla <- get_adjusted_prevalence(data_utla)
utla_most_recent <- get_most_recent(utla)
utla_top_N <- get_top_N(utla_most_recent, n=50)
utla_order <- get_plot_order(utla_most_recent)

utla_plot <- utla
utla_plot$areaNameOrdered <- factor(utla_plot$areaName, levels=utla_order)
utla_plot <- utla_plot %>%
  filter(areaName %in% utla_top_N)

utla_most_recent$areaNameOrdered <- factor(utla_most_recent$areaName, levels=utla_order)
utla_most_recent <- utla_most_recent %>%
  filter(areaName %in% utla_top_N)


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
    vjust=0,
    nudge_x=-2,
    color=color_code,
    fontface="bold"
  ) +
  
  facet_wrap(~areaNameOrdered) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%b %Y"),
    limits = as.Date(c('2021-10-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1))
    
  ) +
  
  labs(
    title="Estimated prevalence of COVID-19 in the UK",
    subtitle="Using adjustments to compensate for delays and underreporting",
    caption=paste(
      "Data from UK Health Security Agency (coronavirus.data.gov.uk) and ONS; adjustment method from microcovid.org\nProduced",
      Sys.Date()
    )
  )
ggsave(paste("plots/prevalence_by_utla_", Sys.Date(), ".png", sep=""), width=10,height=8)


## Animation

region_animate <- region %>%
  #filter(date > as.Date("2021-09-01")) %>%
  group_by(date) %>%
  mutate(rank = min_rank(-prevalenceRate) *1) %>%
  ungroup()


anim <- ggplot(
  region_animate,
  aes(rank,
      group=areaName,
      fill=as.factor(areaName))
) +
  
  geom_tile(
    aes(y=prevalenceRate/2,
        height=prevalenceRate,
        width=0.9),
    alpha=0.8
  ) +
  
  geom_text(aes(y = 0, label = paste(areaName, " ")), vjust = 0.2, hjust = 1) +
  
  scale_x_continuous(labels = scales::percent) +
  
  theme_fivethirtyeight() +
  
  theme(
    legend.position = "none"
  ) +
  
  labs(
    title = "Prevalence of COVID-19 in the UK",
    subtitle = '{frame_time}',
    caption=paste(
      "Data from UK Health Security Agency (coronavirus.data.gov.uk) and ONS; adjustment method from microcovid.org\nProduced",
      Sys.Date()
    )
  ) +
  
  transition_time(date) +
  view_follow(fixed_x=TRUE) +
  ease_aes('linear')

animate(anim, nframes = 100, fps=12,
        renderer = av_renderer())


anim_save("plots/prevalence_over_time_by_region")



anim <- ggplot(mtcars, aes(mpg, disp)) +
  geom_point(aes(color = gear)) +
  transition_states(gear, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

animate(anim)

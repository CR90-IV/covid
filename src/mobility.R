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

res = GET("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
data <- res$content %>%
  rawToChar() %>%
  fread %>%
  as_tibble()

GB <- data %>%
  filter(country_region == "United Kingdom")

plot <- GB %>%
  pivot_longer(
    cols=ends_with("percent_change_from_baseline"),
    names_to="place_category",
    values_to="change_from_baseline"
  ) %>%
  mutate_at("place_category", str_replace, "_percent_change_from_baseline", "") %>%
  mutate(change_from_baseline = change_from_baseline/100)

latest_date <- max(plot$date)

plot %>% filter(sub_region_1 == "") %>%

ggplot(aes(x=date, y=change_from_baseline, color=place_category)) +
  geom_line(
    size=1,
  ) +
  
  gghighlight(
    unhighlighted_params = list(
      size = 1,
      alpha=0.5),
    use_direct_label = FALSE
  ) +
  
  facet_wrap(~place_category) +
  
  scale_y_continuous(labels = scales::percent,
                     limits=c(-1,1)) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%d %b"),
    limits = as.Date(c('2021-12-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=0, face="bold", size=rel(1)),
    rect = element_rect(fill = "transparent"),
    legend.position = "none"
  ) +
  
  #coord_cartesian(clip = "off") +
  
  labs(
    title="Are people going outside?",
    subtitle="Community mobility in the UK",
    caption=paste(
      "Data: Google COVID-19 Community Mobility Reports\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )

ggsave(paste("plots/community_mobility.png", sep=""), width=10,height=8)

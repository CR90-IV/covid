library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)
library(lubridate)
library(RColorBrewer)

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
  mutate(place_category = gsub("_", " ", place_category)) %>%
  mutate(change_from_baseline = change_from_baseline/100)

latest_date <- max(plot$date)


ggplot(plot, aes(x=date, y=change_from_baseline, color=place_category)) +
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


calendar <- plot %>%
  
  mutate(
    wday = wday(date, week_start = 1, label=TRUE),
    day = day(date),
    month = month(date, label=TRUE),
    week = strftime(date, format = "%W"),#if_else(isoweek(date)<=52, isoweek(date), isoweek(date)-52),
    year = year(date),
    year_month = format(date, "%Y-%m")
    ) %>%
  
  mutate(
    place_category = str_wrap(place_category, 12),
    change_factor = cut(
      change_from_baseline,
      breaks=c(-1, -0.75, -0.5, -0.1, 0.1, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2),
      #labels=c("-100% to -50%", "-50% to 0%", "0% to 50%", "50% to 100%", "100% to 150%", "150% to 200%")
      )
  ) %>%
  
  filter(
    #date > as.Date("2021-01-01"),
    #date < as.Date("2021-12-30"),
    sub_region_1 == "",
  )
  


newPalette <- c(
  #rev(brewer.pal(3, "Oranges")),
  "#FDAE6B","#FDD0A2", "#FEEDDE",
  "grey90",
  brewer.pal(7, "Greens"))


ggplot(calendar, aes(x=week, y=wday)) +
  
  geom_tile(
    aes(fill=change_factor),
    color="white",
    size=0.25
    ) +
  
  scale_fill_manual(
    values=newPalette,
    drop=FALSE
    ) +
  
  # scale_fill_gradient2(
  #   low="red",
  #   mid="grey90",
  #   high="green",
  #   #mid="#F0F0F0"
  #   #limits = c(-1, 1)
  #   )+
  
  #geom_text(aes(label=day))+
  
  facet_grid(
    cols=vars(year_month),
    rows=vars(place_category),
    scales="free",
    space="free",
    switch = "y"
    ) +
  
  theme_fivethirtyeight() +
  
  scale_y_discrete(limits = rev)+
  
  labs(
    x="", y="",
    title="Are people going outside?",
    subtitle="Community mobility in the UK compared to Jan-Feb 2020 baseline",
    caption=paste(
      "Data: Google COVID-19 Community Mobility Reports\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )+
  
  theme(
    legend.title=element_blank(),
    panel.grid=element_blank(),
    panel.grid.major = element_blank(),
    panel.border=element_blank(),
    strip.background=element_blank(),
    legend.position="top",
    legend.justification="left",
    legend.direction="horizontal",
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    rect = element_rect(fill = "white")
    ) 

ggsave("plots/mobility_over_time.png", width=17, height=8)    


source("src/functions.R")

library(zoo)
library(ggthemes)
library(scales)


data <- url_to_data_table("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&metric=newReinfectionsBySpecimenDate&format=csv")

df <- data %>%
  #filter(areaName=="London") %>%
  group_by(areaName) %>%
  arrange(date) %>%
  mutate(
    newCases7da = rollmean(newCasesBySpecimenDate, k=7, align="right", fill=NA),
    newReinfections7da = rollmean(newReinfectionsBySpecimenDate, k=7, align="right", fill=NA),
    reinfectionProportion = newReinfectionsBySpecimenDate/newCasesBySpecimenDate,
    reinfectionProportion7da = newReinfections7da/newCases7da
  )

latest_date <- max(df$date)

color_code = "#0070C0"

ggplot(df, aes(x=date)) +
  geom_point(
    aes(y=reinfectionProportion),
    color=color_code,
    alpha=0.5
    ) +
  
  geom_line(
    aes(y=reinfectionProportion7da),
    color=color_code,
    size=1
    ) +
  
  facet_wrap(~areaName) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%d %b"),
    limits = as.Date(c('2021-01-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    text = element_text(family = "IBM Plex Sans"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1)),
    rect = element_rect(fill = "#FFFFFF"),
    axis.text.x=element_text(angle = 90, vjust=0.5)
  ) +
  
  labs(
    title="Proportion of COVID cases that are reinfections",
    subtitle="Reinfection episodes counted as positive tests more than 90 days after the first positive test",
    caption=paste(
      "Data: UKHSA and ONS (via coronavirus.data.gov.uk)\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )

ggsave(paste("plots/reinfections_by_region.png", sep=""), width=10,height=10)

source("src/01_prevalence/prevalence_core.R")

# Import data

data_region = get_data_from_gov_api(
  paste(
    "https://api.coronavirus.data.gov.uk/v2/data?areaType=region",
    "&metric=newCasesBySpecimenDate",
    "&metric=newVirusTestsBySpecimenDate",
    "&format=csv",
    sep="")
)

# Arrange data

region <- get_adjusted_prevalence(data_region)
region_most_recent <- get_most_recent(region)
region_order <- get_plot_order(region_most_recent)
region$areaNameOrdered <- factor(region$areaName, levels=region_order)
region_most_recent$areaNameOrdered <- factor(region_most_recent$areaName, levels=region_order)

latest_date <- max(region$date)


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
    use_direct_label = FALSE,
    unhighlighted_params = list(
      size = 1,
      alpha=0.5
    )
  ) +
  
  geom_text(
    data=region_most_recent, 
    aes(label=scales::percent(prevalenceRate,0.01)),
    hjust=0.75,
    vjust=1,
    nudge_y=-0.0025,
    #nudge_x=0,
    color=color_code,
    fontface="bold"
  ) +
  
  facet_wrap(~areaNameOrdered) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_date(
    #date_breaks = "2 month",
    labels=date_format("%d %b"),
    limits = as.Date(c('2021-12-01',Sys.Date()))
  ) +
  
  theme_fivethirtyeight() +
  
  theme(
    text = element_text(family = "IBM Plex Sans"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1)),
    rect = element_rect(fill = "#FFFFFF")
  ) +
  
  #coord_cartesian(clip = "off") +
  
  labs(
    title="Estimated prevalence of COVID-19 in the UK, by region",
    subtitle="Using adjustments to compensate for delays and underreporting",
    caption=paste(
      "Data: UKHSA and ONS (via coronavirus.data.gov.uk) | Adjustment method: microCOVID Project\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )

ggsave(paste("plots/prevalence_by_region.png", sep=""), width=10,height=8)

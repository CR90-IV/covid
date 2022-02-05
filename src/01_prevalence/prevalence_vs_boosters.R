source("src/01_prevalence/prevalence_core.R")
library(ggrepel)

# Import data

data_vax = get_data_from_gov_api(
  paste(
    "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla",
    "&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage",
    "&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage",
    "&metric=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage",
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

# Arrange data

vax <- data_vax %>%
  rename(
    dose_1_uptake = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
    dose_2_uptake = cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
    dose_3_uptake = cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage
  ) %>%
  mutate(
    dose_1_uptake = dose_1_uptake/100,
    dose_2_uptake = dose_2_uptake/100,
    dose_3_uptake = dose_3_uptake/100
  )


utla <- get_adjusted_prevalence(data_utla)

utla_peak <- utla %>%
  group_by(areaName) %>%
  filter(date > "2021-11-01") %>%
  slice_max(date, n=1) %>%
  mutate(vax_date = as.Date(date) - 10) %>%
  merge(y=vax, by.x=c("vax_date", "areaName"), by.y=c("date", "areaName"))

latest_date <- max(utla$date)



color_code = "#0070C0"

ggplot(utla_peak, aes(x=dose_3_uptake, y=prevalenceRate)) +

  geom_point(
    size=3,
    color=color_code,
  ) +
  
  geom_label_repel(
    aes(label = areaName),
    size=2,
    family = "IBM Plex Sans",
    min.segment.length = 0,
    point.padding = 0.5,
    #arrow = arrow(length = unit(0.005, "npc")),
    label.size = NA,
    alpha = 0.8,
    fill = NA,
    segment.color = 'grey50'
  ) +
  
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  
  theme_fivethirtyeight() +
  
  theme(
    text = element_text(family = "IBM Plex Sans"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color="#C0C0C0"),
    axis.ticks = element_blank(),          #strip axis ticks
    strip.text.x = element_text(hjust=1, face="bold", size=rel(1)),
    rect = element_rect(fill = "#FFFFFF"),
    axis.title.x = element_text(),
    axis.title.y = element_text()
  ) +
  
  labs(
    title="Omicron prevalence compared with vaccine coverage",
    subtitle="Using adjustments to compensate for testing delays and underreporting",
    x="Vaccine coverage",
    y="Prevalence of COVID-19",
    caption=paste(
      "Data: UKHSA and ONS (via coronavirus.data.gov.uk) | Adjustment method: microCOVID Project\nProduced",
      Sys.time(),
      "with most recent data up to",
      latest_date
    )
  )

ggsave(paste("plots/prevalence_vs_boosters.png", sep=""), width=10,height=10)


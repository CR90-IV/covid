source("src/functions.R")
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)
library(lubridate)
library(stringr)
library(extrafont)

url_to_data_table <- function(request_url) {
  res = GET(request_url)
  data <- res$content %>%
    rawToChar() %>%
    fread %>%
    as_tibble()
  return(data)
}

# Get most up to date version of each edition
dataset_url = "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions"
editions = jsonlite::fromJSON(dataset_url)$items

covid_url = editions[which(editions$edition == "covid-19"),]$links$latest_version$href
precovid_url = editions[which(editions$edition == "2010-19"),]$links$latest_version$href

covid_data <- jsonlite::fromJSON(covid_url)$downloads$csv$href %>%
  url_to_data_table()

precovid_data <- jsonlite::fromJSON(precovid_url)$downloads$csv$href %>%
  url_to_data_table()


precovid <- precovid_data %>%
  rename(num_deaths = V4_1) %>%
  mutate(weeknum = as.numeric(gsub("[^0-9.]", "",  `week-number`))) %>%
  mutate(date = lubridate::ymd(paste0(Time, "-01-01")) + lubridate::weeks(weeknum-1)) %>%
  filter(Sex == "All") %>%
  select(Time, weeknum, date, AgeGroups, num_deaths)

avgdeaths <- precovid %>%
  group_by(weeknum, AgeGroups) %>%
  summarise(avg_week_deaths = mean(num_deaths))

covid <- covid_data %>%
  rename(num_deaths = V4_1) %>%
  mutate(
    weeknum = as.numeric(gsub("[^0-9.]", "",  `week-number`)),
    date = lubridate::ymd(paste0(Time, "-01-01")) + lubridate::weeks(weeknum-1),
    AgeGroupsLarge = if_else(AgeGroups == "1-4" | AgeGroups == "10-14", "1-14",
                     if_else(AgeGroups == "15-19" | AgeGroups == "20-24" | AgeGroups == "25-29" | AgeGroups == "30-34" | AgeGroups == "35-39" | AgeGroups == "40-44", "15-44",
                     if_else(AgeGroups == "45-49" | AgeGroups == "50-54" | AgeGroups == "55-59" | AgeGroups == "60-64", "45-64",
                     if_else(AgeGroups == "65-69" | AgeGroups == "70-74", "65-74",
                     if_else(AgeGroups == "75-79" | AgeGroups == "80-84", "75-84",
                     if_else(AgeGroups == "85-89" | AgeGroups == "90+", "85+",
                     if_else(AgeGroups == "All ages", "All ages", AgeGroups)
                     ))))))
    ) %>%
  filter(
    Sex == "All",
    Deaths == "Deaths involving COVID-19: occurrences" | Deaths == "Total registered deaths"
  ) %>%
  group_by(Time, weeknum, date, AgeGroupsLarge, Deaths) %>%
  summarise(num_deaths = sum(num_deaths)) %>%
  merge(
    y=avgdeaths,
    by.x=c("weeknum", "AgeGroupsLarge"),
    by.y=c("weeknum", "AgeGroups")
  ) %>%
  mutate(
    Deaths = str_replace(Deaths, "Deaths involving COVID-19: occurrences", "Deaths_involving_COVID"),
    Deaths = str_replace_all(Deaths, " ", "_")
  ) %>%
  pivot_wider(
    names_from = Deaths,
    values_from = num_deaths
  ) %>%
  mutate(
    excess_deaths = Total_registered_deaths - avg_week_deaths,
    excess_deaths_pc = Total_registered_deaths/avg_week_deaths -1,
    excess_deaths_not_COVID = excess_deaths - Deaths_involving_COVID,
    excess_deaths_not_COVID_pc = (Total_registered_deaths - Deaths_involving_COVID)/avg_week_deaths -1
    )


ggplot(covid, aes(x=date)) +
  
  ## This is scuffed, needs redo
  geom_line(aes(y=avg_week_deaths), color="grey") +
  geom_ribbon(aes(ymin=0, ymax=avg_week_deaths), fill="grey", alpha=0.5) +
  
  geom_line(aes(y=avg_week_deaths + Deaths_involving_COVID), color="orange") +
  geom_ribbon(aes(ymin=avg_week_deaths, ymax=avg_week_deaths + Deaths_involving_COVID), fill="orange", alpha=0.5) +
  
  geom_line(aes(y=Total_registered_deaths)) +
  #geom_line(aes(y=Deaths_involving_COVID), color="red") +
  facet_wrap(~AgeGroupsLarge, scales="free", ncol=2) +
  theme_fivethirtyeight() +
  theme(
    text = element_text(family = "IBM Plex Sans")
  )
         
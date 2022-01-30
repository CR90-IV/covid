library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)
library(extrafont)

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



data_population <- "https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv" %>%
  fread %>%
  as_tibble()

# Get population sizes
df_all_population <- data_population %>%
  filter(category=="ALL") %>%
  select(areaCode, population)

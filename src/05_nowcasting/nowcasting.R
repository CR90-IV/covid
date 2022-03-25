library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(gghighlight)
library(zoo)
library(ggthemes)
library(scales)
library(extrafont)
library(lubridate)
library(janitor)
library(xml2)
library(rvest)


# Parse ONS for latest data

ons_is_url = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata"

ons_is_data_url <- rvest::read_html(ons_is_url) %>% 
  html_nodes(".show-hide") %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  extract2(1)
  
data_is <- paste0("https://www.ons.gov.uk", ons_is_data_url) %>%
  readxl::read_excel(sheet="1p") %>%
  clean_names()


url_to_data_table <- function(request_url) {
  res = GET(request_url)
  data <- res$content %>%
    rawToChar() %>%
    fread %>%
    as_tibble()
  return(data)
}


# COvID reported infections
data_testing = url_to_data_table(
  paste(
    "https://api.coronavirus.data.gov.uk/v2/data?areaType=region",
    "&metric=newCasesBySpecimenDate",
    "&metric=newVirusTestsBySpecimenDate",
    "&format=csv",
    sep="")
) %>% clean_names() %>%
  rename(cases = new_cases_by_specimen_date, tests = new_virus_tests_by_specimen_date)

# Population data
data_population <- "https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv" %>%
  fread %>%
  clean_names() %>%
  filter(category=="ALL") %>%
  select(area_code, population) %>%
  as_tibble()

area_lookup <- data_testing %>%
  select(area_code, area_name) %>%
  unique()



# ONS infection survey
data_is <- readxl::read_excel("src/05_nowcasting/20220318covid19infectionsurveydatasetsengland1.xlsx", sheet="1p") %>%
  clean_names()

region_labels <- data_is %>%
  filter(contents == "Date") %>%
  first() %>%
  transpose(keep.names = "col") %>%
  filter(V1 != "Date") %>%
  fill(V1) %>%
  transpose(make.names="col")


data <- data_is%>%
  mutate(group_id = cumsum(coalesce(str_starts(contents, "Publication Date"), 0))) %>%
  filter(group_id > 0) %>%
  group_by(group_id) %>%
  mutate(
    publication_date = first(contents) %>% str_extract("(?<=:)[^\\]]+") %>% str_trim() %>% as.Date("%d %b %y")
  ) %>%
  filter(contents != "Date", !is.na(x2)) %>%
  ungroup %>%
  select(-group_id)

headings <- paste(rep(c("median", "lower", "upper"), times=10), rep(region_labels, each=3), sep=";") %>% append("publication_date") %>% prepend("date")

infection_survey <- data %>% set_names(headings) %>%
  pivot_longer(
    cols=-c("date", "publication_date"),
    names_to = "observation",
    values_to = "prevalence"
  ) %>%
  # separate the region and observations
  separate("observation", into=c("observation", "region"), sep=";") %>%
  # standardise dates and make prevalence numeric
  mutate(
    date = if_else(
      as.numeric(date) %>% is.na(),
      as.Date(date, "%d %b %y"),
      as.Date(as.numeric(date), origin = "1899-12-30")
      ),
    prevalence_rate = as.numeric(prevalence)/100
  ) %>%
  
  # Get the most recent estimate for each given date and observation
  group_by(date, region, observation) %>%
  slice_max(order_by = "publication_date") %>%
  ungroup() %>%
  
  # Add on population data to calculate numbers from rates
  merge(area_lookup, by.x="region", by.y="area_name") %>%
  merge(data_population, by="area_code") %>%
  mutate(infected=prevalence_rate*population) %>%
  
  # Pivot so each row is one day's record
  select(date, publication_date, area_code, region, observation, prevalence_rate,
         infected) %>%
  pivot_wider(
    id_cols=date:region,
    names_from="observation",
    values_from = c("prevalence_rate", "infected"),
    names_sep="_"
  )
  
  

df <- merge(infection_survey, data_testing, by=c("area_code", "date"), all.x=TRUE) %>%
  select(-area_name, -area_type) %>%
  mutate(
    positivity_rate = cases/tests,
    cases_sum = rollsum(cases, k=7, fill=NA, align="right"),
    report_rate = cases_sum/infected_median
    ) %>%
  #Data looks weird before this
  filter(date > "2020-09-11")


plot <- df %>% 
  #filter(region=="London") %>%
  mutate(period = if_else(date < "2022-01-11", "PCR testing is required", "PCR testing not required"))

ggplot(plot, aes(x=date)) + 
  geom_line(aes(y=cases_sum)) +
  geom_ribbon(aes(ymin=infected_lower, ymax=infected_upper), fill="gray", color=NA, alpha=0.5)

ggplot(plot, aes(x=date, y=report_rate, color=period)) + geom_line() + facet_wrap(~region)# + geom_line(aes(y=positivity_rate))

ggplot(plot, aes(x=(date-as.Date("2020-02-28"))/(1500*positivity_rate**2), y=(report_rate), color=period)) +
  geom_point() +
  theme(legend.position="NA")

ggplot(plot, aes(x=date, y=positivity_rate, color=period)) +
  geom_point()


ggplot(plot, aes(
    y=1/(positivity_rate**0.5*50),
    x=date)
  ) +
  geom_point() +
  geom_point(aes(y=report_rate), color="green") +
  theme(legend.position="NA")


nls(report_rate ~ I(1/(positivity_rate**A*B)), data=plot, start=c(A=0.5, B=50))

nls(infected_median ~ I(cases_sum*A), data=plot, start=list(A=1))

ggplot(plot, aes(x=date, y=infected_median-cases_sum*5)) +
  geom_point()

ggplot(plot, aes(x=cases_sum, y=infected_median, color=period)) +
  geom_pointrange(aes(ymin=infected_lower, ymax=infected_upper))# +
  facet_grid(rows=vars(period), cols=vars(region))

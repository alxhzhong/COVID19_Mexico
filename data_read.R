# Title: Loading + Cleaning Country Data

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
library(tidyverse)


# confirmed
case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed = read_csv(case_url)

# recovered
recovered_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recoveries = read_csv(recovered_url)

# died
death_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths = read_csv(death_url)

# clean data function
clean_data <- function(data, country, col_name){
  data %>%
    filter(`Country/Region` == country) %>%
    filter(is.na(`Province/State`)) %>%
    dplyr::select( -Lat, -Long, -`Country/Region`) %>%
    pivot_longer(cols = !`Province/State`, names_to = "date", values_to = "count") %>%
    mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
    group_by(date) %>%
    summarize({{col_name}} := sum(count)) %>%
    return()
}

country = "Mexico"

confirmed <- clean_data(confirmed, country, "cases_total")
deaths <- clean_data(deaths, country, "deaths_total")
recoveries <- clean_data(recoveries, country, "recoveries_total")

# combine all conf/recov/deaths
cl_all <- confirmed %>%
  left_join(deaths, by = "date") %>%
  left_join(recoveries, by = "date")


mexico =
  cl_all %>%
  mutate(total_removed = deaths_total + recoveries_total,
         total_active_infected = cases_total - total_removed,
         day = 1:n(),
         daily_removed = total_removed - lag(total_removed),
         daily_infected = cases_total - lag(cases_total),
         daily_deaths = deaths_total - lag(deaths_total),
         daily_recoveries = recoveries_total - lag(recoveries_total),
         I = total_active_infected,
         R = total_removed)

# remove outlier/impossible data
mexico = mexico %>% 
  filter(R > -1e4 & I < 8e5) %>% 
  filter(daily_recoveries > -1e4)

# remove useless vars
rm(case_url, death_url, recovered_url)

# hi
# hello!



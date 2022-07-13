# Title: Loading + Cleaning Country Data

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
librarian::shelf(readr, tidyr, dplyr)


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
    dplyr::select(-Lat, -Long, -`Country/Region`) %>%
    pivot_longer(cols = !`Province/State`, names_to = "date", values_to = "count") %>%
    dplyr::select(-`Province/State`) %>%
    mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
    rename({{col_name}} := count) %>%
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
  mutate(daily_infected = cases_total - lag(cases_total),
         daily_deaths = deaths_total - lag(deaths_total),
         daily_recoveries = recoveries_total - lag(recoveries_total))

# fix decreases in cumulative recoveries----

# days with decr. in cumulative recoveries
problem_dates = mexico %>% 
  dplyr::filter(daily_recoveries < 0,
                date < as.Date("2021-08-01"))

mxfixed = mexico

for(i in 1:nrow(problem_dates)){
  date_i = problem_dates[i,]
  pdate = date_i$date
  target = mxfixed[mxfixed$date == pdate - 1,]$recoveries_total #count before drop
  count = mxfixed[mxfixed$date == pdate,]$recoveries_total
  
  while(count < target){
    mxfixed[mxfixed$date == pdate,]$recoveries_total = target
    pdate = pdate + 1
    count = mxfixed[mxfixed$date == pdate,]$recoveries_total
  }
  
}

# recalculate daily num, I, R on fixed values
mexico =
  mxfixed %>%
  mutate(total_removed = deaths_total + recoveries_total,
         total_active_infected = cases_total - total_removed,
         day = 1:n(),
         daily_removed = total_removed - lag(total_removed),
         daily_infected = cases_total - lag(cases_total),
         daily_deaths = deaths_total - lag(deaths_total),
         daily_recoveries = recoveries_total - lag(recoveries_total),
         I = total_active_infected,
         R = total_removed)

# remove useless vars
rm(case_url, death_url, recovered_url, mxfixed, problem_dates, date_i, count, i, pdate, target)

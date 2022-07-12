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

# replace decr. with 6-day rolling avg (3 days before/after)
for(i in 1:nrow(problem_dates)){
  date_i = problem_dates[i,]
  pdate = date_i$date
  
  mean_val = mexico %>%
    filter(date > pdate - 4, 
           date < pdate + 4,
           date != pdate) %>% 
    dplyr::select(daily_recoveries) %>%
    pull(daily_recoveries) %>% 
    mean() %>% 
    round()
    
  mxfixed[mxfixed$date == pdate,]$daily_recoveries = mean_val
}

mxfixed = mxfixed %>% 
  replace(is.na(.), 0)


# recalculate daily num, I, R on fixed values
mexico =
  mxfixed %>%
  mutate(recoveries_total = cumsum(daily_recoveries),
         total_removed = deaths_total + recoveries_total,
         total_active_infected = cases_total - total_removed,
         day = 1:n(),
         daily_removed = total_removed - lag(total_removed),
         I = total_active_infected,
         R = total_removed)

# remove useless vars
rm(case_url, death_url, recovered_url, mxfixed, problem_dates, date_i, i, pdate, mean_val)

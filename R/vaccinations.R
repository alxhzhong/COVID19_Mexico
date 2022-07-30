

source("estimate_tvr.R")
source("mexicocity_dataread.R")

vaccinations <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Mexico.csv")

plot_ly(vaccinations, x = ~date, y = ~people_vaccinated, type = "scatter", mode = "line") %>% 
  add_trace(y = ~vaccinations$total_vaccinations, type = "scatter", mode = "line") %>% 
  add_trace(y = ~vaccinations$people_fully_vaccinated, type = "scatter", mode = "line")

vaccinations <- mxgov %>% 
  left_join(vaccinations, by = "date") %>% 
  mutate(prop_fullyvax = people_fully_vaccinated / 128.9e6) %>% 
  mutate(prop_vax = people_vaccinated / 128.9e6)

plot_ly(vaccinations, x = ~date, y = ~tpr_rolavg, type = "scatter", mode = "line", name = "TPR") %>% 
  add_trace(y = ~vaccinations$prop_fullyvax, type = "scatter", mode = "line", name = "Prop. fully vaccinated") %>% 
  add_trace(y = ~vaccinations$prop_vax, type = "scatter", mode = "line", name = "Prop. vaccinated") %>% 
  layout(
    yaxis = list(range = c(0,1.0)),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)'
  ) 

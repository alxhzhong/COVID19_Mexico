library(tidyverse)
library(lubridate)
library(plotly)


mexicoCity <- read_csv("/Users/laurenhe/Downloads/Casos_Diarios_Estado_Nacional_Confirmados_20220705.csv")

mexicoCity <- mexicoCity %>% 
  filter(nombre == "DISTRITO FEDERAL") %>% 
  dplyr::select(-`cve_ent`, -`poblacion`) %>% 
  pivot_longer(cols = !`nombre`, names_to = "date", values_to = "count") %>% 
  mutate(date_format = dmy(date))



plot_ly(mexicoCity, x = ~date_format, y = ~count, type = "bar", hovermode = "x unified") %>%
  layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16))) 


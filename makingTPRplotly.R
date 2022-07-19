# Title: Loading + Cleaning Mexico Govt Data

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
library(readr)
library(tidyr)
library(dplyr)
library(zoo)
# librarian::shelf(readr, tidyr, dplyr, zoo)
library(plotly)

# Data source ---
mxcase_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_20220712.csv"
mx_confirmed = read_csv(mxcase_url)

mxneg_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Negativos_20220712.csv"
mx_neg = read_csv(mxneg_url)

mxdeaths_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_20220712.csv"
mx_deaths = read_csv(mxdeaths_url)


clean_mxgov = function(data, prov_name, col_name){
  data %>% 
    filter(nombre == prov_name) %>% 
    dplyr::select(-cve_ent, -poblacion, -nombre) %>% 
    pivot_longer(cols = everything(), names_to = "date", values_to = {{col_name}}) %>% 
    mutate(date = as.Date(date, format = "%d-%m-%Y")) %>% 
    return()
}

prov_name = "Nacional"

mx_confirmed_cl <- clean_mxgov(mx_confirmed, prov_name, "daily_infected")
mx_neg_cl <- clean_mxgov(mx_neg, prov_name, "daily_negatives")
mx_deaths_cl <- clean_mxgov(mx_deaths, prov_name, "daily_deaths")

mx_cl_all <- mx_neg_cl %>% 
  left_join(mx_confirmed_cl, by = "date") %>% 
  left_join(mx_deaths_cl, by = "date")

mxgov <- mx_cl_all %>% 
  mutate(
    daily_tests = daily_infected + daily_negatives,
    tpr = daily_infected / daily_tests,
    tpr_rolavg = rollapply(tpr, 7, mean, fill = NA)
  )

# ggplot

# mxgov %>% 
#  ggplot(mapping = aes(x = date, y = tpr_rolavg)) +
#  geom_line()

# plotly


mxgov = mxgov %>% mutate(text = paste0("TPR: ", tpr_rolavg))
fig <- plot_ly(mxgov, type = 'scatter', mode = 'lines', hoverlabel = list(align = "left"))%>%
  add_trace(x = ~date, y = ~tpr_rolavg, name = "test", text = ~text, hoverinfo = 'text') %>%
  layout(showlegend = F) %>% 
  layout(
    xaxis = list(title = "Date",
                zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                gridcolor = '#ffff'),
    yaxis = list(title = "Test Positivity Rate (7-day Ave.)",
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = '#ffff'),
    plot_bgcolor='#e5ecf6', width = 900)
fig

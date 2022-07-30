# Title: Loading + Cleaning Mexico Govt Data

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
library(readr)
library(httr)
library(tidyr)
library(dplyr)
library(zoo)
# librarian::shelf(readr, httr, tidyr, dplyr, zoo)

# find date of last-published data on CONACYT; load data
# Note: dependent on CONACYT using a consistent url format
find_mxdata = function(case_type){
  file_base = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional"
  file_date = Sys.Date() #today's date
  
  # look for file from last 60 days
  for(i in 1:60){
    date_text = format(file_date, "%Y%m%d")
    url_test = paste(file_base, case_type, date_text, sep = "_") %>% 
      paste(".csv", sep = "")
    
    # file not found
    if(http_error(url_test)){
      # print(date_text)
      file_date = file_date - 1
    }
    
    # file found (success!)
    else if(!http_error(url_test)){
      # print("file found!")
      return(read_csv(url_test))
      break
    }
  }
}


# Data source ---
mx_confirmed = find_mxdata("Confirmados")
mx_neg = find_mxdata("Negativos")
mx_deaths = find_mxdata("Defunciones")


# mxcase_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_20220717.csv"
# mx_confirmed = read_csv(mxcase_url)
# 
# mxneg_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Negativos_20220717.csv"
# mx_neg = read_csv(mxneg_url)
# 
# mxdeaths_url = "https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_20220717.csv"
# mx_deaths = read_csv(mxdeaths_url)


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


# mexico city
mxc_confirmed <- clean_mxgov(mx_confirmed, "DISTRITO FEDERAL", "daily_infected")
mxc_neg <- clean_mxgov(mx_neg, "DISTRITO FEDERAL", "daily_negatives")
mxc_deaths <- clean_mxgov(mx_deaths, "DISTRITO FEDERAL", "daily_deaths")

mxc_cl_all <- mxc_neg %>% 
  left_join(mxc_confirmed, by = "date") %>% 
  left_join(mxc_deaths, by = "date")

# rm(mxcase_url, mxdeaths_url, mxneg_url)
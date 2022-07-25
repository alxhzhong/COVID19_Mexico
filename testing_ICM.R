library(epidemia)
data("EuropeCovid")

if(!exists("mxgov")){
  source("mexicocity_dataread.R")
}

options(mc.cores = parallel::detectCores())



data <- mxgov %>% 
  mutate(country = "Mexico") %>% 
  rename(deaths = daily_deaths)
##R0=1.92,IFR= 0.3%

rt <- epirt(formula = R(country, date) ~ 0+rw(time=week,prior_scale = 0.1) ,
            link = scaled_logit(7))



# go back and think about running sensitivity analysis for these priors?
inf <- epiinf(gen = EuropeCovid$si)


deaths <- epiobs(formula = deaths ~ 1, i2o = EuropeCovid$inf2death,
                 link = scaled_logit(0.006))

fm <- epim(rt = rt, inf = inf, obs = deaths, data = data,
           group_subset = "Mexico", algorithm = "sampling", iter = 100,
           seed = 12345, refresh = 0)
plot_obs(fm,type="deaths")

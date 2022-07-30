# Title: SIR/SEIR predictions

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
source("R/SIR_function.R")
source("R/SIR_intervals.R")
source("R/SIR_SEIR_equations.R")

# Optimize beta, gamma for training interval
pred_SIR = sir_intervals("SIR")
pred_I_SIR = pred_SIR[[1]]
pred_R_SIR = pred_SIR[[2]]

last_pars = pred_SIR[[3]]
beta = last_pars[1]
gamma = last_pars[2]

num_days = 15
first_day = pred_I_SIR %>% 
  dplyr::select(date) %>%
  pull() %>% 
  tail(n=1)

last_day = first_day + num_days - 1

date = seq(first_day, last_day, by = 1)

last_IR = mexico %>% 
  filter(date == first_day) %>% 
  dplyr::select(date, I, R)

N = 128900000
lambda = mu = 0


# SIR function
predictions = sir_1(beta = exp(last_pars[1]), gamma = exp(last_pars[2]), I0 = last_IR$I,
                    R0 = last_IR$R, times = c(1:num_days), N = N, lambda = lambda,
                    mu = mu)

pred_I_med = round(predictions$I)
pred_R_med = round(predictions$R)

# CI for SIR
cl = 0.95
cl = (1 - cl) / 2
lwrI = qpois(p = cl, lambda = pred_I_med)
uprI = qpois(p = 1 - cl, lambda = pred_I_med)
pred_I=data.frame(date,pred_I_med,lwrI,uprI)

lwrR = qpois(p = cl, lambda = pred_R_med)
uprR = qpois(p = 1 - cl, lambda = pred_R_med)
pred_R=data.frame(date,pred_R_med,lwrR,uprR)

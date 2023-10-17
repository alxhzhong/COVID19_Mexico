# Title: SIR/SEIR predictions

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
source("R/SEIR_function.R")
source("R/SIR_intervals.R")
source("R/SIR_SEIR_equations.R")
source("R/predict_CI.R")

# Optimize beta, gamma for training interval
pred_SEIR = sir_intervals("SEIR")
pred_I_SEIR = pred_SEIR[[1]]
pred_R_SEIR = pred_SEIR[[2]]

last_pars = pred_SEIR[[3]]
beta = last_pars[1]
gamma = last_pars[2]

num_days = 15
first_day = pred_I_SEIR %>% 
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

predictions = seir_1(beta = exp(last_pars[1]), gamma = exp(last_pars[2]), sigma = 1/5.1,
                     I0 = last_IR$I,R0 = last_IR$R, times = c(1:num_days), N = N, 
                     lambda = lambda, mu = mu, k = best_k[length(best_k)])

sigma_l = 1/4.1
sigma_u = 1/5.8
sigma_m = 1/5.1
rep = 500

pred_I_med = tibble(pred_I_med = round(predictions$I), date)
pred_R_med = tibble(pred_R_med = round(predictions$R), date)

pred_SEIR_test = list(pred_I_med, pred_R_med)

ci_pred_seir = ciband(pred_SEIR_test, sigma_l, sigma_u, sigma_m, last_pars[1], last_pars[2], mexico, rep)

cl = 0.95
cl = (1 - cl) / 2
lwrI = qpois(p = cl, lambda = pred_I_med$pred_I_med)
uprI = qpois(p = 1 - cl, lambda = pred_I_med$pred_I_med)
SEIR_pred_I=data.frame(date,pred_I_med,lwrI,uprI)

lwrR = qpois(p = cl, lambda = pred_R_med$pred_R_med)
uprR = qpois(p = 1 - cl, lambda = pred_R_med$pred_R_med)
SEIR_pred_R=data.frame(date,pred_R_med,lwrR,uprR)

SEIR_pred_I=data.frame(date,pred_I_med, pred_lwrI = ci_pred_seir[[1]]$lwrI, pred_uprI = ci_pred_seir[[1]]$uprI)
SEIR_pred_R=data.frame(date,pred_R_med, pred_lwrR = ci_pred_seir[[2]]$lwrR, pred_uprR = ci_pred_seir[[2]]$uprR)

# ggplot() +
#   geom_bar(data = mexico, mapping = aes(x = date, y = I), stat = "identity") +
#   geom_line(data = pred_I, mapping = aes(x = date, y = pred_I_med)) +
#   xlim(first_day, last_day)



# changing things
# actual_train = mexico %>% 
#   filter(date >= "2020-10-07" & date <= "2021-01-13")

# SEIR_I_smape_train <- smape(actual_train$I, pred_I_SEIR$pred_I_med)
# SEIR_R_smape_train <- smape(actual_train$R, pred_R_SEIR$pred_R_med)
# 
# SEIR_I_smape_train
# SEIR_R_smape_train

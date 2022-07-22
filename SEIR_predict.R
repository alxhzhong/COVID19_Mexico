# Title: SIR/SEIR predictions

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
source("SEIR_function.R")
source("SIR_intervals.R")
source("SIR_SEIR_equations.R")

date_initial = as.Date("2020-11-24")
date_final = as.Date("2021-01-13")
f_days = as.Date(c("2020-11-24", "2020-12-15", "2021-01-04", "2021-01-13"))


sir_intervals_predict = function(method){
  # run SIR fitting for 4 specified periods
  starting_param_val = c(-2.5, -3)
  date_initial = as.Date("2020-11-24")
  date_final = as.Date("2021-01-13")
  f_days = as.Date(c("2020-11-24", "2020-12-15", "2021-01-04", "2021-01-13"))
  
  # choose method
  if(method == "SIR"){
    t1 <- sir_all(mexico, date_initial, f_days[2]-1, starting_param_val)
    t2 <- sir_all(mexico, f_days[2], f_days[3]-1, t1[[3]]) 
    t3 <- sir_all(mexico, f_days[3], f_days[4]-1, t2[[3]])
    
    pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]]) ## bind by row
    pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]])
    
    return(list(pred_I, pred_R, t3[[3]]))
  }
  
  if(method == "SEIR"){
    N = 128900000
    lambda = mu = 0
    
    # best_k = optimize_k(f_days, date_final)
    
    # incubation period (median + 95% CI) for original variant
    sigma_l = 1/4.1
    sigma_u = 1/5.8
    sigma_m = 1/5.1
    rep = 500
    
    t1 <- seir_all(mexico, date_initial, f_days[2]-1, starting_param_val, k = best_k[1])
    t2 <- seir_all(mexico, f_days[2], f_days[3]-1, t1[[4]], k = best_k[2])
    t3 <- seir_all(mexico, f_days[3], f_days[4]-1, t2[[4]], k = best_k[3])
    
    # for reference: ciband(pred, sigma_l, sigma_u, sigma_m, beta, gamma, data, rep)
    ci1 <- ciband(t1, sigma_l, sigma_u, sigma_m, t1[[4]][1], t1[[4]][2], mexico, rep)
    ci2 <- ciband(t2, sigma_l, sigma_u, sigma_m, t2[[4]][1], t2[[4]][2], mexico, rep)
    ci3 <- ciband(t3, sigma_l, sigma_u, sigma_m, t3[[4]][1], t3[[4]][2], mexico, rep)
    
    pred_I <- rbind(ci1[[1]], ci2[[1]], ci3[[1]]) ## bind by row
    pred_R <- rbind(ci1[[2]], ci2[[2]], ci3[[2]])
    
    return(list(pred_I, pred_R, t3[[4]]))
  }
  
  
}



pred_SEIR = sir_intervals_predict("SEIR")
pred_I_SEIR = pred_SEIR[[1]]
pred_R_SEIR = pred_SEIR[[2]]

last_pars = pred_SEIR[[3]]
beta = last_pars[1]
gamma = last_pars[2]

num_days = 30
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

pred_I_med = round(predictions$I)
pred_R_med = round(predictions$R)

cl = 0.95
cl = (1 - cl) / 2
lwrI = qpois(p = cl, lambda = pred_I_med)
uprI = qpois(p = 1 - cl, lambda = pred_I_med)
SEIR_pred_I=data.frame(date,pred_I_med,lwrI,uprI)

lwrR = qpois(p = cl, lambda = pred_R_med)
uprR = qpois(p = 1 - cl, lambda = pred_R_med)
SEIR_pred_R=data.frame(date,pred_R_med,lwrR,uprR)

SEIR_pred_I=data.frame(date,pred_I_med)
SEIR_pred_R=data.frame(date,pred_R_med)

# ggplot() +
#   geom_bar(data = mexico, mapping = aes(x = date, y = I), stat = "identity") +
#   geom_line(data = pred_I, mapping = aes(x = date, y = pred_I_med)) +
#   xlim(first_day, last_day)


source("SIR_function.R")
source("SEIR_function.R")
source("optimize_k.R")
source("predict_CI.R")
# test

date_initial = as.Date("2020-11-22")
date_final = as.Date("2021-03-01")
f_days = as.Date(c("2020-11-22", "2020-12-15", "2021-01-06", "2021-01-23"))

if(!exists("best_k")){
  best_k = optimize_k(f_days, date_final)
}

sir_intervals = function(method){
  # run SIR fitting for 4 specified periods
  starting_param_val = c(-2.5, -3)
  date_initial = as.Date("2020-11-22")
  date_final = as.Date("2021-03-01")
  
  # choose method
  if(method == "SIR"){
    t1 <- sir_all(mexico, date_initial, f_days[2]-1, starting_param_val)
    t2 <- sir_all(mexico, f_days[2], f_days[3]-1, t1[[3]]) 
    t3 <- sir_all(mexico, f_days[3], f_days[4]-1, t2[[3]])
    t4 <- sir_all(mexico, f_days[4], date_final, t3[[3]])
    
    pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]], t4[[1]]) ## bind by row
    pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]], t4[[2]])
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
    t4 <- seir_all(mexico, f_days[4], date_final, t3[[4]], k = best_k[4])
    
    # for reference: ciband(pred, sigma_l, sigma_u, sigma_m, beta, gamma, data, rep)
    ci1 <- ciband(t1, sigma_l, sigma_u, sigma_m, t1[[4]][1], t1[[4]][2], mexico, rep)
    ci2 <- ciband(t2, sigma_l, sigma_u, sigma_m, t2[[4]][1], t2[[4]][2], mexico, rep)
    ci3 <- ciband(t3, sigma_l, sigma_u, sigma_m, t3[[4]][1], t3[[4]][2], mexico, rep)
    ci4 <- ciband(t4, sigma_l, sigma_u, sigma_m, t4[[4]][1], t4[[4]][2], mexico, rep)
    
    pred_I <- rbind(ci1[[1]], ci2[[1]], ci3[[1]], ci4[[1]]) ## bind by row
    pred_R <- rbind(ci1[[2]], ci2[[2]], ci3[[2]], ci4[[2]])
  }
  
  return(list(pred_I, pred_R, t4[[3]]))
  
}

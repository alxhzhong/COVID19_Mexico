
source("SIR_function2.R")

# run SIR fitting for 1-month periods
starting_param_val = log(c(1e-2,1e-5))
date_initial = as.Date("2020-11-22")
date_final = as.Date("2021-03-01")

t1 <- sir_all(mexico, "2020-11-22", "2020-12-22", starting_param_val)
t2 <- sir_all(mexico, "2020-12-23", "2021-01-11", t1[[3]]) 
t3 <- sir_all(mexico, "2021-01-12", "2021-01-22", t2[[3]])
t4 <- sir_all(mexico, "2021-01-23", "2021-03-01", t3[[3]])

pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]], t4[[1]]) ## bind by row
pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]], t4[[2]])

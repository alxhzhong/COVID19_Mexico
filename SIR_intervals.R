
source("SIR_function.R")
source("SEIR_function.R")
# test

sir_intervals = function(method){
  # run SIR fitting for 4 specified periods
  starting_param_val = c(-2.5, -3)
  date_initial = as.Date("2020-11-22")
  date_final = as.Date("2021-03-01")
  
  # choose method
  if(method == "SIR"){
    t1 <- sir_all(mexico, "2020-11-22", "2020-12-14", starting_param_val)
    t2 <- sir_all(mexico, "2020-12-15", "2021-01-05", t1[[3]]) 
    t3 <- sir_all(mexico, "2021-01-06", "2021-01-22", t2[[3]])
    t4 <- sir_all(mexico, "2021-01-23", "2021-03-01", t3[[3]])
    
    pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]], t4[[1]]) ## bind by row
    pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]], t4[[2]])
  }
  
  if(method == "SEIR"){
    t1 <- seir_all(mexico, "2020-11-22", "2020-11-30", starting_param_val)
    t2 <- seir_all(mexico, "2020-12-01", "2021-01-05", t1[[4]]) 
    t3 <- seir_all(mexico, "2021-01-06", "2021-01-22", t2[[4]])
    t4 <- seir_all(mexico, "2021-01-23", "2021-03-01", t3[[4]])
    
    pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]], t4[[1]]) ## bind by row
    pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]], t4[[2]])
  }
  
  return(list(pred_I, pred_R))
  
}






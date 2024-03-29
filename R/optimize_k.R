# Title: Optimizing k (E0/I0)

# Authors: Emily Bach, Lauren He, Alex Zhong

source("R/SEIR_function.R")

if(!exists("mexico")){
  source("R/data_read.R")
}

optimize_k = function(f_days, date_final){
  # setting up k value vector
  k_choices = seq(0.01, 2, by = 0.01)
  best_k = c()
  starting_param_val = c(-2.5, -3)
  
  for(i in 1:length(f_days)){
    sse_vec = rep(NA, length(k_choices))
    
    if(i != length(f_days)){
      end_day = f_days[i + 1] - 1
    }
    if(i == length(f_days)){
      end_day = date_final
    }
    
    # loop for one interval
    for(j in 1:length(k_choices)){
      k = k_choices[j]
      # evaluate error using k
      error_val = seir_all(mexico, f_days[i], end_day, starting_param_val, k = k)[[5]]
      sse_vec[j] = error_val
    }
    
    # add to best k for that interval
    best_k <- append(best_k, k_choices[which.min(sse_vec)])
  }
  
  return(best_k)
}

date_initial = as.Date("2020-10-07")
date_final = as.Date("2021-01-13")
f_days = as.Date(c(date_initial, "2020-11-07", "2020-12-15", "2021-01-05"))

optimize_k(f_days, date_final) %>% 
  saveRDS("best_k.rds")


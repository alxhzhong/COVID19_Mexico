# Title: Loading + Cleaning Country Data

# Authors: Ritoban Kundu, Emily Bach, Lauren He, Alex Zhong

# source SEIR function
source("SIR_SEIR_equations.R")


# confidence interval estimation
ciband <- function(pred, sigma_l, sigma_u, sigma_m, beta, gamma, data, rep){
  # modifying pred to fit SIR_intervals/SEIR_all formatting
  date <- pred[[1]]$date
  I <- pred[[1]]$pred_I_med
  R <- pred[[2]]$pred_R_med
  pred <- data.frame(date, I, R) 
  
  N = 128900000
  lambda = mu = 0
  
  beta = exp(beta)
  gamma = exp(gamma)
  
  # filter mexico data to be within specified interval
  days = pred$date
  data = data %>% dplyr::filter(date %in% days)
  
  pred_I_med=pred$I
  pred_R_med=pred$R
  sd=abs(1/sigma_u - 1/sigma_l)/(2*1.96)
  pred_I=matrix(0,nrow=nrow(pred),ncol=rep)
  pred_R=matrix(0,nrow=nrow(pred),ncol=rep)

  for(i in 1:rep){
    De=rnorm(1,mean=1/sigma_m, sd=sd)
    sigma=1/De
    predictions = seir_1(beta = beta, gamma = gamma, sigma=sigma, I0 = data$I[1],
                         R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                         mu = mu, k = best_k[1])
    p_I=rpois(nrow(predictions),lambda=predictions$I)
    p_R=rpois(nrow(predictions),predictions$R)
    pred_I[,i]=p_I
    pred_R[,i]=p_R
  }
  lwrI = round(apply(pred_I,1,quantile, probs=0.025))
  uprI = round(apply(pred_I,1,quantile, probs=0.975))
  sd=(uprI-lwrI)/1.96*2
  lwrI=pred_I_med-1.96*sd
  uprI=pred_I_med+1.96*sd
  pred_I=data.frame(date,pred_I_med,lwrI,uprI)
  lwrR = round(apply(pred_R,1,quantile, probs=0.025))
  uprR = round(apply(pred_R,1,quantile, probs=0.975))
  sd=(uprR-lwrR)/1.96*2
  lwrR=pred_R_med-1.96*sd
  uprR=pred_R_med+1.96*sd
  pred_R=data.frame(date,pred_R_med,lwrR,uprR)
  return(list(pred_I,pred_R))
}

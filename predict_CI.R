# Title: Loading + Cleaning Country Data

# Authors: Ritoban Kundu, Emily Bach, Lauren He, Alex Zhong


ciband<-function(pred,sigma_l,sigma_u,sigma_m,beta,gamma,data,rep){
  pred_I_med=pred$I
  pred_R_med=pred$R
  sd=abs(1/sigma_l+1/sigma_u-2*(1/sigma_m))/(2*1.96)
  pred_I=matrix(0,nrow=nrow(pred),ncol=rep)
  pred_R=matrix(0,nrow=nrow(pred),ncol=rep)
  for(i in 1:rep){
    De=rnorm(1,mean=1/sigma_m, sd=sd)
    sigma=1/De
    predictions = seir_1(beta = beta, gamma = gamma, I0 = data$I[1],
                         R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                         mu = mu, sigma=sigma)
    p_I=rpois(nrow(predictions),lambda=predictions$I)
    p_R=rpois(nrow(predictions),predictions$R)
    pred_I[,i]=p_I
    pred_R[,i]=p_R
  }
  lwrI = round(apply(pred_I,1,quantile, probs=0.025))
  uprI = round(apply(pred_I,1,quantile, probs=0.975))
  pred_I=data.frame(date,pred_I_med,lwrI,uprI)
  lwrR = round(apply(pred_R,1,quantile, probs=0.025))
  uprR = round(apply(pred_R,1,quantile, probs=0.975))
  pred_R=data.frame(date,pred_R_med,lwrR,uprR)
  return(list(pred_I,pred_R))
}
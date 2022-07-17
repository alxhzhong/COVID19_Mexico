# Title: Loading + Cleaning Country Data

# Authors: Ritoban Kundu, Emily Bach, Lauren He, Alex Zhong

seir_1 = function(beta, gamma, sigma, I0, R0, times, N, lambda, mu, k) {
  # define SIR equations
  seir_equations = function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS = -beta * I * S/N + lambda * N - mu * S
      dE = beta * I * S/N - sigma * E - mu * E
      dI = sigma * E - gamma * I - mu * I
      dR =  gamma * I - mu*R
      return(list(c(dS, dE, dI, dR)))
    })
  }
  # prepare input for ODE solver
  parameters_values = c(beta = beta, gamma = gamma)
  E0 = k*I0
  S0 = N - E0 - I0 - R0
  initial_values = c(S = S0, E = E0, I = I0, R = R0)
  # solve system of ODEs
  out = ode(initial_values, times, seir_equations, parameters_values, method = "rk4")
  return(as.data.frame(out))
}


ciband<-function(pred,sigma_l,sigma_u,sigma_m,beta,gamma,data,rep){
  date <- pred[[1]]$date
  I <- pred[[1]]$pred_I_med
  R <- pred[[2]]$pred_R_med
  pred <- data.frame(date, I, R) 
  
  beta = exp(beta)
  gamma = exp(gamma)
  
  days = pred$date
  data = data %>% dplyr::filter(date %in% days)
  
  pred_I_med=pred$I
  pred_R_med=pred$R
  sd=abs(1/sigma_l+1/sigma_u-2*(1/sigma_m))/(2*1.96)
  pred_I=matrix(0,nrow=nrow(pred),ncol=rep)
  pred_R=matrix(0,nrow=nrow(pred),ncol=rep)
  
  # N = 128900000
  # lambda = mu = 0
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
  pred_I=data.frame(date,pred_I_med,lwrI,uprI)
  lwrR = round(apply(pred_R,1,quantile, probs=0.025))
  uprR = round(apply(pred_R,1,quantile, probs=0.975))
  pred_R=data.frame(date,pred_R_med,lwrR,uprR)
  return(list(pred_I,pred_R))
}

# ci_results <- ciband(t1, sigma_l = 1/4.1, sigma_u = 1/5.8, sigma_m = 1/5.1, beta = beta, gamma = gamma, data = mexico, 500)
# # pred_I_SEIR <- ci_results[[1]]
# 
# pred_I_SEIR_graph <- pred_I_SEIR %>% 
#   mutate(index = 1:n()) %>% 
#   mutate(loess = fitted(loess(pred_I_med ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
#   mutate(upper = fitted(loess(uprI ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
#   mutate(lower = fitted(loess(lwrI ~ index, data = pred_I_SEIR, span = 0.3)))
# 
# 
# plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual",
#         color = I("#60A5E8")) %>% 
#   add_trace(y = ~pred_I_SEIR$pred_I_med, type = 'scatter', mode = 'lines', name = "Predicted", line = list(color = 'rgba(245, 121, 58,, 1)')) %>%
#   add_trace(y = ~pred_I_SEIR$uprI, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE) %>% 
#   add_trace(y = ~pred_I_SEIR$lwrI, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.5)'), name = "Lower", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c(date_initial,date_final)),
#     yaxis = list(title = 'Active Infections'),
#     xaxis = list(title = 'Date'),
#     hovermode = "x unified")
# 

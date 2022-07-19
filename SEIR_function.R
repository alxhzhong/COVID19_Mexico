# Title: SEIR function

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
library(deSolve)
library(outbreaks)
library(gridExtra)
library(arm)
library(tidyverse)
library(bbmle)
# librarian::shelf(deSolve, outbreaks, gridExtra, arm, tidyverse, bbmle)


# Start of function!

seir_all <- function(data, date_initial, date_final, starting_param_val, k){
  # start/end dates can be string
  date_initial = as.Date(date_initial)
  date_final = as.Date(date_final)
  
  data =
    data %>%
    filter(date >= date_initial, date <= date_final)
  
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
  
  # starting_param_val = log(c(1e-2,1e-5))      # starting param as argument of fxn
  N = 128900000                                 # population size
  lambda = mu = 0                            # birth/death rate
  
  ss = function(beta, gamma, sigma, N, data, lambda, mu,k) {
    # starting cases and removals on day 1
    I0 = data$I[1]
    R0 = data$R[1]
    times = data$day
    # transform parameters so they are non-negative
    beta = exp(beta)
    gamma = exp(gamma)
    # generate predictions using parameters, starting values
    predictions = seir_1(beta = beta, gamma = gamma, sigma = sigma,       # parameters
                         I0 = I0, R0 = R0,                        # variables' intial values
                         times = times, N = N, lambda = lambda, mu = mu,k=k)    # time points
    # compute the sums of squares
    sum((predictions$I[-1] - data$I[-1])^2 + (predictions$R[-1] - data$R[-1])^2)
    #sum((predictions$I[-1] - data$I[-1])^2 )
  }
  
  # convenient wrapper to return sums of squares ----
  ss2 = function(x, N, data, lambda, mu,sigma,k) {
    ss(beta = x[1], gamma = x[2], N = N, data = data, lambda = lambda, mu = mu,sigma=sigma,k=k)
  }
  
  logli = function(beta, gamma, sigma, N, dat, lambda, mu) {
    I0 = dat$I[1]
    E0 = I0 * k
    R0 = dat$R[1]
    times = dat$day
    beta = exp(beta)
    gamma = exp(gamma)
    predictions = seir_1(beta = beta, gamma = gamma, sigma = sigma,  # parameters
                         I0 = I0, R0 = R0, # variables' intial values
                         times = times, N = N, lambda = lambda, mu = mu)
    ## negative of log likelihood
    -sum(dpois(x = dat$I, lambda = predictions$I, log = TRUE)) - sum(dpois(x = dat$R, lambda = predictions$R, log = TRUE))
  }
  
  
  method = "ls" # choose method
  
  if(method == "ls"){
    # set starting values ----
    starting_param_val = starting_param_val                               ## why need starting_param here when also above in function
    N = 128900000                                 # population size
    lambda = mu = 1/(75.05*365)                            # birth/death rate
    # set the data set
    sigma = 1/5.1
    
    # Optimization result ----
    ss_optim = optim(starting_param_val, ss2, N = N, data = data, lambda = lambda,
                     mu = mu, sigma=sigma,k=k)
    
    # Obtain beta, gamma
    pars = ss_optim$par
    sse = ss_optim$value
  }
  
  if(method == "mle"){
    N=128900000
    lambda=mu= 1/(75.05*365)  
    sigma = 1/5.1
    
    starting_param_val = starting_param_val
    estimates_pois = mle2(minuslogl = logli,
                          start = lapply(starting_param_val, log), method = "Nelder-Mead",
                          data=list(dat = data, N = N, lambda = lambda, mu = mu, sigma = sigma))
    pars = as.numeric(coef(estimates_pois))
    sse = 0 # CHANGE WHEN MLE WORKING
  }
  
  R = as.numeric((exp(pars[1])*(1/5.1)) / ((1/5.1)+mu)*(exp(pars[2])+mu)) # does this look ok?
  
  predictions = seir_1(beta = exp(pars[1]), gamma = exp(pars[2]), sigma = sigma, I0 = data$I[1],
                       R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                       mu = mu,k=k)              # generate predictions from the least
  # squares solution
  
  date = seq(date_initial, date_final, by = 1)
  pred_I_med = round(predictions$I)
  pred_R_med = round(predictions$R)
  pred_E_med = round(predictions$E)
  
  cl = 0.95
  cl = (1 - cl) / 2
  lwrI = qpois(p = cl, lambda = pred_I_med)
  uprI = qpois(p = 1 - cl, lambda = pred_I_med)
  pred_I=data.frame(date,pred_I_med,lwrI,uprI)
  
  lwrR = qpois(p = cl, lambda = pred_R_med)
  uprR = qpois(p = 1 - cl, lambda = pred_R_med)
  pred_R=data.frame(date,pred_R_med,lwrR,uprR)
  
  lwrE = qpois(p = cl, lambda = pred_E_med)
  uprE = qpois(p = 1 - cl, lambda = pred_E_med)
  pred_E=data.frame(date,pred_E_med,lwrE,uprE)
  
  return(list(pred_I, pred_R, pred_E, pars, sse, method))
  
}

# starting_param_val = log(c(1e-2,1e-5, 1/5.1))
# test <- sir_all(mexico, "2020-11-22", "2021-03-01", starting_param_val)
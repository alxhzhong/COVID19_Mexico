# Title: SIR function 2 (no CI)

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
librarian::shelf(deSolve, outbreaks, gridExtra, arm, tidyverse)


# Import data ----

source("data_read.R")

# Start of function!

sir_all <- function(data, date_initial, date_final, starting_param_val){
  # start/end dates can be string
  date_initial = as.Date(date_initial)
  date_final = as.Date(date_final)

  data =
    data %>%
    filter(date >= date_initial, date <= date_final)

  sir_1 = function(beta, gamma, I0, R0, times, N, lambda, mu) {
    # define SIR equations
    sir_equations = function(time, variables, parameters) {
      with(as.list(c(variables, parameters)), {
        dS = -beta * I * S/N + lambda * N - mu * S
        dI =  beta * I * S/N - gamma * I -mu * I
        dR =  gamma * I - mu*R
        return(list(c(dS, dI, dR)))
      })
    }
    # prepare input for ODE solver
    parameters_values = c(beta = beta, gamma = gamma)
    S0 = N - I0 - R0
    initial_values = c(S = S0, I = I0, R = R0)
    # solve system of ODEs
    out = ode(initial_values, times, sir_equations, parameters_values, method = "rk4")
    return(as.data.frame(out))
  }

  # starting_param_val = log(c(1e-2,1e-5))      # starting param as argument of fxn
  N = 128900000                                 # population size
  lambda = mu = 0                            # birth/death rate

  ss = function(beta, gamma, N, data, lambda, mu) {
    # starting cases and removals on day 1
    I0 = data$I[1]
    R0 = data$R[1]
    times = data$day
    # transform parameters so they are non-negative
    beta = exp(beta)
    gamma = exp(gamma)
    # generate predictions using parameters, starting values
    predictions = sir_1(beta = beta, gamma = gamma,                        # parameters
                        I0 = I0, R0 = R0,                                  # variables' intial values
                        times = times, N = N, lambda = lambda, mu = mu)    # time points
    # compute the sums of squares
    sum((predictions$I[-1] - data$I[-1])^2 + (predictions$R[-1] - data$R[-1])^2)
    #sum((predictions$I[-1] - data$I[-1])^2 )
  }

  # convenient wrapper to return sums of squares ----
  ss2 = function(x, N, data, lambda, mu) {
    ss(beta = x[1], gamma = x[2], N = N, data = data, lambda = lambda, mu = mu)
  }

  ss_optim = optim(starting_param_val, ss2, N = N, data = data, lambda = lambda,
                   mu = mu)

  pars = ss_optim$par

  R = as.numeric(exp(pars[1]) / exp(pars[2]))

  predictions = sir_1(beta = exp(pars[1]), gamma = exp(pars[2]), I0 = data$I[1],
                      R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                      mu = mu)              # generate predictions from the least
  # squares solution

  date = seq(date_initial, date_final, by = 1)
  pred_I_med = round(predictions$I)
  pred_R_med = round(predictions$R)

  pred_I=data.frame(date,pred_I_med)
  pred_R=data.frame(date,pred_R_med)

  return(list(pred_I, pred_R, pars))

}

#starting_param_val = log(c(1e-2,1e-5))
#test <- sir_all(mexico, "2020-11-22", "2021-03-01", starting_param_val)

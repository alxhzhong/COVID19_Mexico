# Title: Loading + Cleaning Country Data

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
source("SIR_function.R")
source("SIR_intervals.R")

pred_SIR = sir_intervals("SIR")
pred_I_SIR = pred_SIR[[1]]
pred_R_SIR = pred_SIR[[2]]

last_pars = pred_SIR[[3]]
beta = last_pars[1]
gamma = last_pars[2]

num_days = 14
first_day = pred_I_SIR %>% 
  dplyr::select(date) %>%
  pull() %>% 
  tail(n=1)

last_day = first_day + num_days - 1

date = seq(first_day, last_day, by = 1)

last_IR = mexico %>% 
  filter(date == last_day) %>% 
  dplyr::select(date, I, R)

N = 128900000
lambda = mu = 0


# SIR function
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

predictions = sir_1(beta = exp(last_pars[1]), gamma = exp(last_pars[2]), I0 = last_IR$I,
                    R0 = last_IR$R, times = c(1:num_days), N = N, lambda = lambda,
                    mu = mu)

pred_I_med = round(predictions$I)
pred_R_med = round(predictions$R)

cl = 0.95
cl = (1 - cl) / 2
lwrI = qpois(p = cl, lambda = pred_I_med)
uprI = qpois(p = 1 - cl, lambda = pred_I_med)
pred_I=data.frame(date,pred_I_med,lwrI,uprI)

lwrR = qpois(p = cl, lambda = pred_R_med)
uprR = qpois(p = 1 - cl, lambda = pred_R_med)
pred_R=data.frame(date,pred_R_med,lwrR,uprR)

ggplot() +
  geom_bar(data = mexico, mapping = aes(x = date, y = I), stat = "identity") +
  geom_line(data = pred_I, mapping = aes(x = date, y = pred_I_med)) +
  xlim(first_day, last_day)


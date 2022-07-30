# Title: SIR/SEIR predictions

# Authors: Emily Bach, Lauren He, Alex Zhong

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

# SEIR function
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

# calculate SMAPE


# SIR
smape <- function(actual, pred){
  return (1/length(actual) * sum(2*abs(pred-actual) / (abs(actual) + abs(pred)) *100))
}
actual <- mexico %>%
  filter(date >= "2021-01-14" & date <= "2021-01-26")
SIR_I_smape_est <- SIR_int %>%
  filter(date >= "2021-01-14" & date <= "2021-01-26")

SIR_I_smape <- smape(actual$I, SIR_I_smape_est$pred_I_med.y)
SIR_R_smape <- smape(actual$R, SIR_I_smape_est$pred_R_med.y)



# SEIR
# calculate SMAPE
smape <- function(actual, pred){  
  return (1/length(actual) * sum(2*abs(pred-actual) / (abs(actual) + abs(pred)) *100))
}

actual <- mexico %>% 
  filter(date >= "2021-01-14" & date <= "2021-01-26")
SEIR_I_smape_est <- SEIR_int %>% 
  filter(date >= "2021-01-14" & date <= "2021-01-26")

SEIR_I_smape <- smape(actual$I, SEIR_I_smape_est$pred_I_med.y)
SEIR_R_smape <- smape(actual$R, SEIR_I_smape_est$pred_R_med.y)


# eSIR
eSIR_smape_est <- esir_graph %>% 
  filter(date >= "2021-01-14" & date <= "2021-01-26")
eSIR_smape_est_R <- esir_graph_R %>% 
  filter(date >= "2021-01-14" & date <= "2021-01-26")

eSIR_I_smape <- smape(actual$I, eSIR_smape_est$median.y)
eSIR_R_smape <- smape(actual$R, eSIR_smape_est_R$median.y)


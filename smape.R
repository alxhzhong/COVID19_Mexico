# Title: SMAPE error calculation

# Authors: Emily Bach, Lauren He, Alex Zhong

train_days = seq(as.Date("2020-10-07"), as.Date("2021-01-13"), by = 1)
test_days = seq(as.Date("2021-01-14"), as.Date("2021-01-26"), by = 1)

# observed data
actual_train <- mexico %>%
  filter(date %in% train_days) %>% 
  dplyr::select(I, R)
actual_test <- mexico %>%
  filter(date %in% test_days) %>% 
  dplyr::select(I, R)

# calculate SMAPE
smape <- function(actual, pred){
  return (1/length(actual) * sum(2*abs(pred-actual) / (abs(actual) + abs(pred)) *100))
}

# SIR
SIR_est_train <- SIR_int %>%
  filter(date %in% train_days) %>% 
  select(date, pred_I_med.x, pred_R_med.x)
SIR_est_test <- SIR_int %>%
  filter(date %in% test_days) %>% 
  select(date, pred_I_med.y, pred_R_med.y)

SIR_I_smape_train <- smape(actual_train$I, SIR_est_train$pred_I_med.x)
SIR_R_smape_train <- smape(actual_train$R, SIR_est_train$pred_R_med.x)

SIR_I_smape_test <- smape(actual_test$I, SIR_est_test$pred_I_med.y)
SIR_R_smape_test <- smape(actual_test$R, SIR_est_test$pred_R_med.y)



# SEIR
# calculate SMAPE

SEIR_est_train <- SEIR_int %>%
  filter(date %in% train_days) %>% 
  select(date, pred_I_med.x, pred_R_med.x)
SEIR_est_test <- SEIR_int %>%
  filter(date %in% test_days) %>% 
  select(date, pred_I_med.y, pred_R_med.y)

SEIR_I_smape_train <- smape(actual_train$I, SEIR_est_train$pred_I_med.x)
SEIR_R_smape_train <- smape(actual_train$R, SEIR_est_train$pred_R_med.x)

SEIR_I_smape_test <- smape(actual_test$I, SEIR_est_test$pred_I_med.y)
SEIR_R_smape_test <- smape(actual_test$R, SEIR_est_test$pred_R_med.y)

# eSIR

eSIR_I_smape_train <- smape(actual_train$I, esir_pre$median)
eSIR_R_smape_train <- smape(actual_train$R, esir_pre_R$median)

eSIR_I_smape_test <- smape(actual_test$I, esir_post$median)
eSIR_R_smape_test <- smape(actual_test$R, esir_post_R$median)


library(devtools)
install("R/eSIR")
library(eSIR)
library(tibble)

if(!exists("plt_data")){
  source("R/estimate_tvr.R")
}
rm(plt_data)

plt_data <- tibble(
  date_num = res$dates
) %>% left_join(
  res$R, by = c("date_num" = "t_end")
) %>%
  dplyr::select(
    date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
  ) %>%
  mutate(date = mexico_filt$date) %>%
  filter(!is.na(r))

# baseline_dates = plt_data %>% 
#   filter(date >= as.Date("2020-11-18") & date <= as.Date("2020-11-24"))
# 
# baseline_r = baseline_dates %>% 
#   dplyr::select(r) %>% 
#   summarize(mean = mean(r)) %>% 
#   pull()

baseline_r = plt_data %>% 
  filter(date >= as.Date("2020-10-07") & date <= as.Date("2021-01-13")) %>% 
  arrange(desc(r)) %>% 
  head(n = 1) %>% 
  select(r) %>% 
  pull()
  

pi = plt_data %>% 
  dplyr::select(r, date) %>% 
  mutate(pi_t = r / baseline_r) %>% 
  filter(date >= as.Date("2020-10-07") & date <= as.Date("2021-01-13"))

pi_2 = pi %>% 
  left_join(dplyr::select(mexico, date, I, R), by = "date") %>% 
  mutate(
    I = I / 128.9e6,
    R = R / 128.9e6
  )
pi_2[pi_2$pi_t > 1,]$pi_t = 1

# quar_dates = plt_data %>% 
#   dplyr::select(date) %>% 
#   filter(date >= as.Date("2020-03-30") & date < as.Date("2020-05-31")) %>% 
#   pull() %>% 
#   format(format = "%m/%d/%Y")
# 
# quar_pi = pi_2 %>% 
#   filter(date >= as.Date("2020-03-30") & date < as.Date("2020-05-31")) %>% 
#   dplyr::select(pi_t) %>% 
#   pull()
# quar_pi = c(1, quar_pi)
# quar_pi[length(quar_pi)] = 1

change_time = pi %>% 
  dplyr::select(date) %>% 
  pull() %>% 
  format(format = "%m/%d/%Y")
p0 <- c(1, pi$pi_t)
# p0[length(p0)] = 1

res.step <- tvt.eSIR(
  Y = pi_2$I, R = pi_2$R, pi0 = p0, change_time = change_time, begin_str = "10/07/2020", death_in_R = 0.101,
  beta0 = 0.1065, gamma0 = 0.0707, T_fin = 112, casename = "Mexico", save_files = F, M = 5e4, nburnin = 2e4
)

res.step_final <- tvt.eSIR(
  Y = pi_2$I, R = pi_2$R, pi0 = p0, change_time = change_time, begin_str = "10/07/2020", death_in_R = 0.101,
  beta0 = 0.1065, gamma0 = 0.0707, T_fin = 112, casename = "Mexico", save_files = F, M = 5e5, nburnin = 2e5
)

res.step %>% 
  saveRDS(file = "eSIR_results.rds")

res.step_final %>% 
  saveRDS(file = "eSIR_results_final.rds")

rds_output_final = res.step_final[c("data_comp", "data_comp_R")]

rds_output_final %>% 
  saveRDS(file = "eSIR_results_final.rds")




# no pi(t)
# res.step <- tvt.eSIR(
# Y = pi_2$I, R = pi_2$R, begin_str = "10/07/2020", death_in_R = 0.03,
# beta0 = 0.1065, gamma0 = 0.0707, T_fin = 112, casename = "Mexico", save_files = F, M = 500, nburnin = 200
# )


# res.step$plot_infection
# res.step$plot_removed
# res.step$spaghetti_plot


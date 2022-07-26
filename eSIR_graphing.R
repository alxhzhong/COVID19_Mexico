esir_res = readRDS("eSIR_results.rds")

all_dates = seq(as.Date("2020-10-07"), as.Date("2021-01-26"), by = 1)
data_comp = res.step$data_comp
data_comp_R = res.step$data_comp_R
data_poly = res.step$data_poly

# jags_sample = res.step$jags_sample
# theta_pp = res.step$theta_pp
Y_mean = res.step$Y_mean
Y_band = res.step$Y_band
theta_p_mean = res.step$theta_p_mean
theta_p_ci = res.step$theta_p_ci
  

data_comp %>% 
  ggplot(mapping = aes(x = time, y = median)) +
  geom_polygon(data = data_poly, mapping = aes(x = x, y = y, fill = value, group = phase)) +
  geom_point()

# observed data to compare to eSIR fit
mexico_esir = mexico_filt %>% 
  filter(date %in% all_dates) %>% 
  select(date, I, R) %>% 
  mutate(I = I / 128.9e6,
         R = R / 128.9e6)

data_comp %>% 
  ggplot(mapping = aes(x = all_dates)) +
  geom_col(data = mexico_esir, mapping = aes(x = date, y = I), alpha = 0.5) +
  geom_line(mapping = aes(y = upper)) +
  geom_line(mapping = aes(y = lower)) +
  geom_point(mapping = aes(y = median)) +
  geom_vline(xintercept = as.Date("2021-01-13"))
  
  


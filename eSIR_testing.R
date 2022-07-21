
library(eSIR)

plt_data <- tibble(
  date_num = res$dates
) %>% left_join(
  res$R, by = c("date_num" = "t_end")
) %>%
  dplyr::select(
    date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
  ) %>%
  add_column(date = mexico_filt$date) %>%
  mutate(
    text = paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                  format(round(r, 2), nsmall = 2), "<br>CI: ",
                  paste0("[", format(round(lower, 2), nsmall = 2), ", ",
                         format(round(upper, 2), nsmall = 2), "]"))
  ) %>%
  filter(!is.na(r))

test = plt_data %>% 
  filter(date >= as.Date("2020-03-23") & date < as.Date("2020-03-31"))

baseline_r = test %>% dplyr::select(r) %>%  summarize(mean = mean(r)) %>% pull()

pi = plt_data %>% 
  dplyr::select(r, date) %>% 
  mutate(pi_t = r / baseline_r) %>% 
  filter(date >= as.Date("2020-03-23") & date < as.Date("2020-05-31"))

pi_2 = pi %>% 
  left_join(dplyr::select(mexico, date, I, R), by = "date") %>% 
  mutate(
    I = I / 128.9e6,
    R = R / 128.9e6
  )

change_time <- format(pi$date, format = "%m/%d/%Y")
p0 <- pi$pi_t
res.step <- tvt.eSIR(
  pi_2$I, pi_2$R, begin_str = "03/14/2020", death_in_R = 0.05,
  T_fin = 300, pi0 = p0, casename = "Mexico", save_files = F, M = 5000, nburnin = 2000
)

res.step$plot_infection
res.step$plot_removed






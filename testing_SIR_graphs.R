# Title: SIR function
# hi
# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----
librarian::shelf(deSolve, outbreaks, gridExtra, arm, tidyverse, bbmle)

source("SIR_function2.R")

# run SIR fitting for 1-month periods
starting_param_val = log(c(1e-2,1e-5))
date_initial = as.Date("2020-11-22")
date_final = as.Date("2021-03-01")

t1 <- sir_all(mexico, "2020-11-22", "2020-12-22", starting_param_val)
t2 <- sir_all(mexico, "2021-01-01", "2021-01-31", t1[[3]]) ## starting param is 3rd index of t1 ?
t3 <- sir_all(mexico, "2021-02-01", "2021-03-01", t2[[3]])

pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]]) ## bind by row
pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]])


# Plot results ----
# ci = c("#C79999")
mn = c("#7C0000")
date_breaks = "1 month"

base = ggplot() +
  xlab("") +
  scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  theme(legend.position = "right")

p1 = base +
  geom_smooth(mapping = aes(x = date, y = pred_I_med, color = colour),
            data = pred_I, size = 0.5, color = mn, span = 0.3) +
  # geom_ribbon(
  #   mapping = aes(x = date, ymin = lwrI, ymax = uprI),
  #   data = pred_I,
  #   size = 1, fill = ci, alpha = 0.8,
  # ) +
  geom_bar(mapping = aes(x = date, y = I), stat = "identity",
           data = mexico, width = 0.5, fill = 'steelblue', alpha = 0.7,
  ) +
  xlim(date_initial, date_final)

p1 = p1 + labs(y = "Active Cases")

p2 = base +
  geom_line(mapping = aes(x = date, y = pred_R_med, color = colour),
            data = pred_R, size = 1,color=mn) +
  # ggplot2::geom_ribbon(
  #   mapping = ggplot2::aes(x = date, ymin = lwrR, ymax=uprR),
  #   data = pred_R,
  #   size = 1,fill=ci,alpha=0.8,
  # ) +
  geom_bar(mapping = aes(x = date, y = R), stat = "identity",
           data = mexico, width = 0.5, fill = 'steelblue', alpha = 0.7,
  ) +
  xlim(date_initial, date_final)
p2 = p2 + labs(y = "Removed")


p = grid.arrange(p1, p2)

# testing change to document :-) hjvjhj

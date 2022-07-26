esir_res = readRDS("eSIR_results_final.rds")

all_dates = seq(as.Date("2020-10-07"), as.Date("2021-01-26"), by = 1)
data_comp = esir_res$data_comp
data_comp_R = esir_res$data_comp_R
data_poly = esir_res$data_poly

# jags_sample = esir_res$jags_sample
# theta_pp = esir_res$theta_pp
Y_mean = esir_res$Y_mean
Y_band = esir_res$Y_band
theta_p_mean = esir_res$theta_p_mean
theta_p_ci = esir_res$theta_p_ci
  

# data_comp %>% 
#   ggplot(mapping = aes(x = time, y = median)) +
#   geom_polygon(data = data_poly, mapping = aes(x = x, y = y, fill = value, group = phase)) +
#   geom_point()

# observed data to compare to eSIR fit
mexico_esir = mexico_filt %>% 
  filter(date %in% all_dates) %>% 
  dplyr::select(date, I, R)

# data for I graph
data_comp <- data_comp %>% 
  mutate(mean = mean * 128.9e6, upper = upper * 128.9e6, lower = lower * 128.9e6, median = median * 128.9e6) %>% 
  mutate(date = seq(from = as.Date("2020-10-07"), to = as.Date("2021-01-26"), by = 'day'))

esir_pre <- data_comp %>% 
  filter(phase == "pre")
esir_post <- data_comp %>% 
  filter(phase == "post")
esir_graph <- mexico_esir %>% 
  left_join(esir_pre, by = "date") %>% 
  left_join(esir_post, by = "date")

# data for R graph
data_comp_R <- data_comp_R %>% 
  mutate(mean = mean * 128.9e6, upper = upper * 128.9e6, lower = lower * 128.9e6, median = median * 128.9e6) %>% 
  mutate(date = seq(from = as.Date("2020-10-07"), to = as.Date("2021-01-26"), by = 'day'))

esir_pre_R <- data_comp_R %>% 
  filter(phase == "pre")
esir_post_R <- data_comp_R %>% 
  filter(phase == "post")
esir_graph_R <- mexico_esir %>% 
  left_join(esir_pre_R, by = "date") %>% 
  left_join(esir_post_R, by = "date")

# esir_graph_R[100,"lower.x"] = esir_graph_R[99,"lower.x"]
# esir_graph_R[100,"upper.x"] = esir_graph_R[99,"upper.x"]
# esir_graph_R[100,"median.x"] = esir_graph_R[99,"median.x"]




# data_comp %>% 
#   ggplot(mapping = aes(x = all_dates)) +
#   geom_col(data = mexico_esir, mapping = aes(x = date, y = I), alpha = 0.5) +
#   geom_line(mapping = aes(y = upper)) +
#   geom_line(mapping = aes(y = lower)) +
#   geom_point(mapping = aes(y = median)) +
#   geom_vline(xintercept = as.Date("2021-01-13"))


# # I prediction
# plot_ly(esir_graph, x = ~date, y = ~I, type = "bar", name = "Actual",
#         color = I("#A95AA1")) %>% 
#   add_trace(y = ~esir_graph$median.x, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
#   add_trace(y = ~esir_graph$upper.x, type = 'scatter', mode = 'lines', name = "Upper", color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE, hoverinfo = 'skip') %>%
#   add_trace(y = ~esir_graph$lower.x, type = 'scatter', mode = 'lines', fill = 'tonexty', color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE, hoverinfo = 'skip') %>%
#   add_trace(y = ~esir_graph$median.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   add_ribbons(ymin = ~esir_graph$lower.y,
#               ymax = ~esir_graph$upper.y,
#               line = list(color = 'rgb(100, 225, 0, 0.2)'),
#               fillcolor = 'rgba(100, 225, 0, 0.2)',
#               showlegend = FALSE,
#               hoverinfo = "skip") %>%
#   add_lines(
#     y = range(0:max(esir_graph$median.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-10-07", "2021-01-26"), title = 'Date'),
#     yaxis = list(title = 'Active Infections'),
#     #hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )
# 
# # R prediction
# 
# plot_ly(esir_graph_R, x = ~date, y = ~R, type = "bar", name = "Actual",
#         color = I("#A95AA1")) %>% 
#   add_trace(y = ~esir_graph_R$median.x, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
#   add_trace(y = ~esir_graph_R$upper.x, type = 'scatter', mode = 'lines', name = "Upper", color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE, hoverinfo = 'skip') %>%
#   add_trace(y = ~esir_graph_R$lower.x, type = 'scatter', mode = 'lines', fill = 'tonexty', color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE, hoverinfo = 'skip') %>%
#   add_trace(y = ~esir_graph_R$median.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   add_ribbons(ymin = ~esir_graph_R$lower.y,
#               ymax = ~esir_graph_R$upper.y,
#               line = list(color = 'rgb(100, 225, 0, 0.2)'),
#               fillcolor = 'rgba(100, 225, 0, 0.2)',
#               showlegend = FALSE,
#               hoverinfo = "skip") %>%
#   add_lines(
#     y = range(0:max(esir_graph_R$median.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-10-07", "2021-01-26"), title = 'Date'),
#     yaxis = list(title = 'Active Infections'),
#     #hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )  
#   
# 

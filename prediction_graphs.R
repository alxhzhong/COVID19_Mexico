source("SIR_predict.R")
source("SEIR_predict.R")


# SIR mutating
SIR_int <- mexico %>% 
  filter(date >= "2020-10-07" & date <= "2021-01-26") %>% 
  dplyr::select(date, I, R)
pred_I_SIR <- pred_I_SIR %>% 
  mutate(index = 1:n()) %>% 
  mutate(loess_m = fitted(loess(pred_I_med ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(upper_m = fitted(loess(uprI ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(lower_m = fitted(loess(lwrI ~ index, data = pred_I_SIR, span = 0.3))) 

SIR_int <- SIR_int%>% 
  left_join(pred_I_SIR, by = "date") %>% 
  left_join(pred_I, by = "date") %>% 
  left_join(pred_R_SIR, by = "date") %>% 
  left_join(pred_R, by = "date")
SIR_int <-SIR_int %>%   mutate(
    text = paste0("Date: ", format(date, format = '%b %d'), "<br>Actual: ",
                  format(round(I, 2)), "<br>Model: ",
                  paste0(format(round(SIR_int$loess_m, 2))))
  )

# SIR_I <- merge(pred_I, pred_I_SIR, by = "date", all = TRUE)
# SIR_I <-  merge(mexico_SIR_int, SIR_I, by = "date", all = TRUE)
# SIR_R <- merge(pred_R, pred_R_SIR, by = "date", all = TRUE)
# SIR_R <-  merge(mexico_SIR_int, SIR_R, by = "date", all = TRUE)



# SEIR mutating
SEIR_int <-mexico %>% 
  filter(date >= "2020-10-07" & date <= "2021-01-26")
pred_I_SEIR <- pred_I_SEIR %>% 
  mutate(index = 1:n()) %>% 
  mutate(loess = fitted(loess(pred_I_med ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
  mutate(upper = fitted(loess(uprI ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
  mutate(lower = fitted(loess(lwrI ~ index, data = pred_I_SEIR, span = 0.3)))

SEIR_int <- SEIR_int%>% 
  left_join(pred_I_SEIR, by = "date") %>% 
  left_join(SEIR_pred_I, by = "date") %>% 
  left_join(pred_R_SEIR, by = "date") %>% 
  left_join(SEIR_pred_R, by = "date")

# SEIR_I <- merge(SEIR_pred_I, pred_I_SEIR, by = "date", all = TRUE) 
# SEIR_I <- merge(mexico_SEIR_int, SEIR_I, by = "date", all = TRUE)
# SEIR_R <- merge(SEIR_pred_R, pred_R_SEIR, by = "date", all = TRUE) 
# SEIR_R <- merge(mexico_SEIR_int, SEIR_R, by = "date", all = TRUE)
  

# # SIR I prediction
# plot_ly(SIR_int, x = ~date, y = ~I, type = "bar", name = "Actual",
#         color = I("#60A5E8")) %>% 
#   add_trace(y = ~SIR_int$pred_I_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   add_trace(y = ~SIR_int$uprI.y, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(100, 225, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
#   add_trace(y = ~SIR_int$lwrI.y, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(100, 225, 0, 0.2)'), name = "Lower", line = list(color = "rgba(100, 225, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
#   add_trace(y = ~SIR_int$loess_m, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
#   add_trace(y = ~SIR_int$upper_m, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
#   add_trace(y = ~SIR_int$lower_m, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>%
#   add_lines(
#     y = range(0:max(SIR_int$pred_I_med.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "black"), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-11-24", "2021-01-26")),
#     yaxis = list(title = 'Active Infections'),
#     xaxis = list(title = 'Date'),
#     hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )
# 
# 
# 
# # SEIR I prediction
# plot_ly(SEIR_int, x = ~date, y = ~I, type = "bar", name = "Actual",
#         color = I("#60A5E8")) %>% 
#   add_trace(y = ~SEIR_int$pred_I_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   # add_trace(y = ~SEIR_I$uprI, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(44, 237, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
#   # add_trace(y = ~SEIR_I$lwrI, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(44, 237, 0, 0.2)'), name = "Lower", line = list(color = "rgba(44, 237, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
#   add_trace(y = ~SEIR_int$loess, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
#   add_trace(y = ~SEIR_int$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
#   add_trace(y = ~SEIR_int$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
#   add_lines(
#     y = range(0:max(SEIR_int$pred_I_med.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "black"), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-11-21", "2021-02-10")),
#     yaxis = list(title = 'Active Infections'),
#     xaxis = list(title = 'Date'),
#     hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )
# 
# 
# # SIR R prediction
# plot_ly(SIR_int, x = ~date, y = ~R, type = "bar", name = "Actual",
#         color = I("#A95AA1")) %>% 
#   add_trace(y = ~SIR_int$pred_R_med.x, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
#   add_trace(y = ~SIR_int$uprR.x, type = 'scatter', mode = 'lines', name = "Upper", color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE) %>% 
#   add_trace(y = ~SIR_int$lwrR.x, type = 'scatter', mode = 'lines', fill = 'tonexty', color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE) %>% 
#   # add_ribbons(ymin = ~SIR_int$lwrR.x,
#   #             ymax = ~SIR_int$uprR.x,
#   #             line = list(color = 'rgb(245, 121, 58, 0.2)'),
#   #             fillcolor = 'rgba(245, 121, 58, 0.2)',
#   #             showlegend = FALSE) %>% 
#   add_trace(y = ~SIR_int$pred_R_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   add_ribbons(ymin = ~SIR_int$lwrR.y,
#               ymax = ~SIR_int$uprR.y,
#               line = list(color = 'rgb(100, 225, 0, 0.2)'),
#               fillcolor = 'rgba(100, 225, 0, 0.2)',
#               showlegend = FALSE,
#               hoverinfo = "skip") %>% 
#   add_lines(
#     y = range(0:max(SIR_int$pred_R_med.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "black"), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-11-24", "2021-01-26")),
#     yaxis = list(title = 'Total Removed'),
#     xaxis = list(title = 'Date'),
#     hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )
# 
# 
# # SEIR R prediction
# plot_ly(SEIR_int, x = ~date, y = ~R, type = "bar", name = "Actual", color = I("#A95AA1")) %>% 
#   add_trace(y = ~SEIR_int$pred_R_med.x, type = 'scatter', mode = 'lines', name = "Model", color = I("rgba(245, 121, 58, 1.0)"), line = list(width = 3))%>%
#   add_trace(y = ~SEIR_int$uprR, type = 'scatter', mode = 'lines', name = "Upper",  color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE) %>%
#   add_trace(y = ~SEIR_int$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty',  color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE) %>%
#   # add_ribbons(ymin = ~SEIR_R$lwrR,
#   #             ymax = ~SEIR_R$uprR,
#   #             # line = list(color = 'rgba(245, 121, 58, 0.1)'),
#   #             # fillcolor = 'rgba(245, 121, 58, 0.3)',
#   #             color = I('rgba(245, 121, 58, 0.5)'),
#   #             showlegend = FALSE, hoverinfo = "none") %>% 
#   add_trace(y = ~SEIR_int$pred_R_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
#   add_lines(
#     y = range(0:max(SEIR_int$pred_R_med.y, na.rm = TRUE)), x = "2021-01-12",
#     line = list(color = "black"), hoverinfo = "skip", showlegend = FALSE) %>% 
#   layout(
#     xaxis = list(
#       range=c("2020-11-24", "2021-02-10")),
#     yaxis = list(title = 'Total Removed'),
#     xaxis = list(title = 'Date'),
#     hovermode = "x unified",
#     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
#     paper_bgcolor='rgba(0,0,0,0)',
#     plot_bgcolor='rgba(0,0,0,0)',
#     font = t
#   )


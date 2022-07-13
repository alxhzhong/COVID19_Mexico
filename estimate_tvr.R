# Title: SIR function

# Authors: Emily Bach, Lauren He, Alex Zhong

# Packages ----

if(!exists("mexico")){
  source("data_read.R")
}

# function
data=mexico
estimatetvr <- function(data, date_initial, date_final, mean_si, std_si){
  data <- data %>% 
    filter(date >= as.Date(date_initial),
           date <= as.Date(date_final))
  
  t_start = seq(2, nrow(data) - 4)
  t_end = t_start + 4
  
  res <- EpiEstim::estimate_R(
    incid = data$daily_infected,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      mean_si             = mean_si,
      std_si              = std_si,
      si_parametric_distr = "W",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 46342))
  )
  
  return(res)

}

mexico_filt <- mexico %>%
  filter(date >= as.Date(date_initial),
         date <= as.Date(date_final))

date_initial = as.Date("2020-04-01")
date_final = as.Date("2021-07-01")

res <- estimatetvr(mexico_filt, date_initial, date_final, 4.8, 2.3)

start_date = mexico_filt$date[1]

# fancy plot
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

cap <- paste0("Mexico. Last updated: ",
               format(Sys.Date(), format = "%b %e"), sep = ' ')
axis_title_font <- list(size = 16)
tickfont        <- list(size = 16)

plt_data = plt_data %>%
  filter(r < 10)

p <- plot_ly(plt_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
             line = list(color = "rgb(54, 163, 11)", width = 5),
             hoverinfo = "text",
             text   = ~text) %>%
  add_markers(data = plt_data, x = ~date, y = ~r, mode = "marker",
              marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%
  add_ribbons(ymin = ~lower,
              ymax = ~upper,
              line = list(color = 'rgba(54, 163, 11, 0.05)'),
              fillcolor = 'rgba(54, 163, 11, 0.2)',
              hoverinfo = "none") %>%
  layout(
    title = list(text = cap, xanchor = "left", x = 0),
    xaxis = list(title = "Date", titlefont = axis_title_font,
                 tickfont = tickfont, zeroline = T),
    yaxis = list(title = "R(t)", titlefont = axis_title_font,
                 tickfont = tickfont, zeroline = T),
    shapes = list(
      type = "line", xref = "paper", yref = "data",
      x0 = 0, x1 = 1, y0 = 1, y1 = 1,
      line = list(color = "rgba(255, 153, 51, 0.5)")
    ),
    showlegend = FALSE
  ) %>%
  plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

p


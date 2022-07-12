library(shiny)
library(plotly)
library(dplyr)

source("data_read.R")
source("SIR_intervals.R")
source("estimate_tvr.R")

pred_SIR = sir_intervals("SIR")
pred_I_SIR = pred_SIR[[1]]
pred_R_SIR = pred_SIR[[2]]

pred_SEIR = sir_intervals("SEIR")
pred_I_SEIR = pred_SEIR[[1]]
pred_R_SEIR = pred_SEIR[[2]]

# for descriptive plots, to truncate timeframe to where recoveries stop reporting
mexicoDescriptives <- mexico %>% 
  filter(date <= "2021-08-04")

# for SIR I graph, to smooth line
pred_I_graph <- pred_I_SIR %>% 
  mutate(index = 1:n()) %>% 
  mutate(loess = fitted(loess(pred_I_med ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(upper = fitted(loess(uprI ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(lower = fitted(loess(lwrI ~ index, data = pred_I_SIR, span = 0.3)))

# for cumulative plotly graph
stack = bind_rows(
  mexicoDescriptives %>% dplyr::select(date, val = daily_deaths) %>%
    mutate(name = "Deaths"),
  mexicoDescriptives %>% dplyr::select(date, val = daily_infected) %>%
    mutate(name = "Infected"),
  mexicoDescriptives %>% dplyr::select(date, val = daily_recoveries) %>%
    mutate(name = "Recovered")
)

# for SIR graphs, to plot in correct timeframe
mexicoSmall = mexico %>% 
  right_join(pred_I_SIR, by = "date")
date_initial = "2020-11-22"
date_final = "2021-03-01"


# start of app

ui <- fluidPage(
  
  # titlePanel("graphZ"),
  
  fluidRow(
    # mainPanel(
    #   plotlyOutput("graph"),
    #   plotlyOutput("graph2"),
    # ),
    navbarPage("Mexico",
               tabPanel("Stacked Plotly", 
                        mainPanel(
                          plotlyOutput("graphStacked")
                        ),
                        sidebarPanel(
                          # selectInput(inputId =  "y", label = "label",
                          #             choices = names(plotMexico)),
                          checkboxGroupInput("name", "data:",
                                             choices=unique(stack$name), selected = unique(stack$name)),
                          id = "sidebar"
                        )),
               
               navbarMenu("Cumulative",
                          tabPanel("Cumulative Infections", plotlyOutput("graphCumulativeI")),
                          tabPanel("Cumulative Recoveries", plotlyOutput("graphCumulativeR")),
                          tabPanel("Daily Active Cases", plotlyOutput("graphActiveI")),
               ),
               
               navbarMenu("SIR Estimations",
                          tabPanel("SIR Active", plotlyOutput("graphSIRActive")),
                          tabPanel("SIR Recoveries", plotlyOutput("graphSIRRecov"))
               ),
               
               tabPanel("R Estimation", plotlyOutput("graphR0")),
               
    )
    
  )
  
)



server <- function(input, output, session){
  
  dataplot <- eventReactive(input$name, {
    stack <- stack %>% filter(as.factor(name) %in% c(input$name))
  })
  output$graphStacked <- renderPlotly({
    plot_ly(dataplot(), x = ~date, y =~val, color = ~name,
            type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16)), hovermode = "x unified") %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
    
    
  })
  
  
  output$graphCumulativeI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~cases_total, type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified") 
  })
  
  output$graphCumulativeR <- renderPlotly({ 
    plot_ly(mexicoDescriptives, x = ~date, y = ~R, type = "bar", hovermode = "x unified") %>%
      layout(barmode = "stack", title = list(xanchor = "right", x = 0), legend =
               list(orientation = "h", font = list(size = 16)))
  })
  
  output$graphActiveI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~I, type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified") 
    
    
  })
  
  output$graphSIRActive <- renderPlotly({
    # date_breaks = "1 month"
    # 
    # base = ggplot() +
    #   xlab("") +
    #   scale_x_date(
    #     date_breaks = date_breaks,
    #     labels = scales::date_format("%e %b")
    #   ) +
    #   theme_bw() +
    #   theme(
    #     axis.text.x = element_text(angle = 45, hjust = 1),
    #     axis.text = element_text(size = 12),
    #     axis.title = element_text(size = 12)
    #   ) +
    #   theme(legend.position = "right")
    
    # p1 <- #base +
    #   ggplot() +
    #   geom_smooth(mapping = aes(x = date, y = pred_I_med),
    #               data = pred_I, size = 0.5, span = 0.3) +
    #   # geom_ribbon(
    #   #   aes(x = date, ymin = lwrI, ymax = uprI),
    #   # data = pred_I,
    #   #   #size = 1, fill = ci, alpha = 0.8,
    #   # ) +
    #   geom_bar(mapping = aes(x = date, y = I), stat = "identity",
    #            data = mexico, width = 0.5, fill = 'steelblue', alpha = 0.7,
    #   ) +
    #   xlim(date_initial, date_final)
    # 
    # p1 <- p1 + labs(y = "Active Cases")
    # ggplotly(p1) %>% 
    #   layout(
    #     hovermode = "x unified")
    
    plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual") %>% 
      add_trace(y = ~pred_I_graph$loess, type = 'scatter', mode = 'lines', name = "Predicted") %>%
      add_trace(y = ~pred_I_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>% 
      add_trace(y = ~pred_I_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c(date_initial,date_final)),
        hovermode = "x unified")
  })
  
  output$graphSIRRecov <- renderPlotly({
    
    # p = ggplot() +
    #   geom_line(mapping = aes(x = date, y = pred_R_med),
    #             data = pred_R, size = 1) +
    #   # ggplot2::geom_ribbon(
    #   #   mapping = ggplot2::aes(x = date, ymin = lwrR, ymax=uprR),
    #   #   data = pred_R,
    #   #   size = 1,fill=ci,alpha=0.8,
    #   # ) +
    #   geom_bar(mapping = aes(x = date, y = R), stat = "identity",
    #            data = mexico, width = 0.5, fill = 'steelblue', alpha = 0.7,
    #   ) +
    #   xlim(date_initial, date_final) +
    #   labs(y = "Removed")
    # ggplotly(p)
    
    plot_ly(mexicoSmall, x = ~date, y = ~R, type = "bar", name = "Actual") %>% 
      add_trace(y = ~pred_R_SIR$pred_R_med, type = 'scatter', mode = 'lines', name = "Predicted") %>%
      # add_trace(y = ~pred_R$uprR, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>% 
      # add_trace(y = ~pred_R$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE)
      add_ribbons(ymin = ~pred_R_SIR$lwrR,
                  ymax = ~pred_R_SIR$uprR,
                  line = list(color = 'rgba(54, 163, 11, 0.05)'),
                  fillcolor = 'rgba(54, 163, 11, 0.2)',
                  hoverinfo = "none") %>% 
      
      layout(
        xaxis = list(
          range=c(date_initial, date_final)),
        hovermode = "x unified")
    
  })
  
  output$graphR0 <- renderPlotly({
    plot_ly(plt_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
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
    
    
  })
  
}

shinyApp(ui=ui, server=server)

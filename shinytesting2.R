library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

source("data_read.R")
source("SIR_intervals.R")
source("estimate_tvr.R")


mexicoDescriptives <- mexico %>% 
  filter(date <= "2021-08-04")


stack = bind_rows(
  mexicoDescriptives %>% dplyr::select(date, val = daily_deaths) %>%
    mutate(name = "Deaths"),
  mexicoDescriptives %>% dplyr::select(date, val = daily_infected) %>%
    mutate(name = "Infected"),
  mexicoDescriptives %>% dplyr::select(date, val = daily_recoveries) %>%
    mutate(name = "Recovered")
)

mexicoSmall = mexico %>% 
  right_join(pred_I, by = "date")



ui <- fluidPage(theme = shinytheme("darkly"),
  

  # titlePanel("graphZ"),
  
  fluidRow(
      # mainPanel(
      #   plotlyOutput("graph"),
      #   plotlyOutput("graph2"),
      # ),
      navbarPage("Mexico",
                 tabPanel("Stacked Plotly", 
                          mainPanel(
                            plotlyOutput("graph")
                            ),
                          sidebarPanel(
                            # selectInput(inputId =  "y", label = "label",
                            #             choices = names(plotMexico)),
                            checkboxGroupInput("name", "data:",
                                               choices=unique(stack$name), selected = unique(stack$name)),
                            id = "sidebar"
                          )),
                
                  navbarMenu("Cumulative",
                            tabPanel("Cumulative Infections", plotlyOutput("graph3")),
                            tabPanel("Cumulative Recoveries", plotlyOutput("graph4")),
                            tabPanel("Daily Active Cases", plotlyOutput("graphInfections")),
                 ),
                 
                 navbarMenu("SIR Estimations",
                            tabPanel("SIR Active", plotlyOutput("graph5")),
                            tabPanel("SIR Recoveries", plotlyOutput("graphSIR2"))
                            ),
              
                 tabPanel("R Estimation", plotlyOutput("graph6")),
               
    )
   
  )
  
)



server <- function(input, output, session){
 
  dataplot <- eventReactive(input$name, {
    stack <- stack %>% filter(as.factor(name) %in% c(input$name))
  })
  output$graph <- renderPlotly({
    plot_ly(dataplot(), x = ~date, y =~val, color = ~name,
            type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16)), hovermode = "x unified") %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))


  })


  output$graph3 <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~cases_total, type = "bar") %>%
    layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
             list(font = list(size = 16)), hovermode = "x unified") 
  })
    
  output$graph4 <- renderPlotly({ 
    plot_ly(mexicoDescriptives, x = ~date, y = ~R, type = "bar", hovermode = "x unified") %>%
    layout(barmode = "stack", title = list(xanchor = "right", x = 0), legend =
             list(orientation = "h", font = list(size = 16)))
  })
  
  output$graphInfections <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~I, type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified") 
    
    
  })
  
  output$graph5 <- renderPlotly({
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
  
    p1 <- #base +
      ggplot() +
      geom_smooth(mapping = aes(x = date, y = pred_I_med),
                  data = pred_I, size = 0.5, span = 0.3) +
      geom_ribbon(
        aes(x = date, ymin = lwrI, ymax = uprI),
        data = pred_I,
        #size = 1, fill = ci, alpha = 0.8,
      ) +
      geom_bar(mapping = aes(x = date, y = I), stat = "identity",
               data = mexico, width = 0.5, fill = 'steelblue', alpha = 0.7,
      ) +
      xlim(date_initial, date_final)

    p1 <- p1 + labs(y = "Active Cases", x = "Date")
    ggplotly(p1) %>% 
      layout(
        hovermode = "x unified")

  })
  
  output$graphSIR2 <- renderPlotly({
    
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
      add_trace(y = ~pred_R$pred_R_med, type = 'scatter', mode = 'lines', name = "Predicted") %>% 
      layout(
        xaxis = list(
          range=c(date_initial,date_final)),
        hovermode = "x unified")
    
  })
  
  output$graph6 <- renderPlotly({
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



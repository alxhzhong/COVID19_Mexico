library(shiny)
library(plotly)
library(dplyr)

plotMexico <- mexico %>% 
  dplyr::select("I", "R")

stack = bind_rows(
  mexico %>% dplyr::select(date, val = daily_deaths) %>%
    mutate(name = "Deaths"),
  mexico %>% dplyr::select(date, val = daily_infected) %>%
    mutate(name = "Infected"),
  mexico %>% dplyr::select(date, val = daily_recoveries) %>%
    mutate(name = "Recovered")
)

source("SIR_intervals.R")

source("SIR_function2.R")

# run SIR fitting for 1-month periods
# starting_param_val = log(c(1e-2,1e-5))
# date_initial = as.Date("2020-11-22")
# date_final = as.Date("2021-03-01")
# 
# t1 <- sir_all(mexico, "2020-11-22", "2020-12-22", starting_param_val)
# t2 <- sir_all(mexico, "2020-12-22", "2021-01-11", t1[[3]]) 
# t3 <- sir_all(mexico, "2021-01-11", "2021-01-22", t2[[3]])
# t4 <- sir_all(mexico, "2021-01-22", "2021-03-01", t3[[3]])
# 
# pred_I <- rbind(t1[[1]], t2[[1]], t3[[1]], t4[[1]]) 
# pred_R <- rbind(t1[[2]], t2[[2]], t3[[2]], t4[[2]])



ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId =  "y", label = "label",
                choices = names(plotMexico)),
    checkboxGroupInput("name", "data:",
                       choices=unique(stack$name), selected = unique(stack$name)),
    id = "sidebar"
  ),

  titlePanel("graphZ"),
  
  mainPanel(
    # mainPanel(
    #   plotlyOutput("graph"),
    #   plotlyOutput("graph2"),
    # ),
    navbarPage("Mexico",
       tabPanel("Stacked Plotly 1", plotlyOutput("graph")),
       tabPanel("Stacked Plotly 2", plotlyOutput("graph2")),
       navbarMenu("Cumulative",
            tabPanel("Plotly Cumulative Infections", plotlyOutput("graph3")),
            tabPanel("Plotly Cumulative Recoveries", plotlyOutput("graph4")),
            ),
       tabPanel("SIR Graphs", plotOutput("graph5")),
       tabPanel("R Estimation", plotlyOutput("graph6")),
       id = "navbar"
       )
  )
    
  
  # navlistPanel(
  #   tabPanel("Stacked Plotly 1", plotlyOutput("graph")),
  #   tabPanel("Stacked Plotly 2", plotlyOutput("graph2")),
  #   tabPanel("Plotly Cumulative Recoveries", plotlyOutput(("graph3")),
  #   tabPanel("Plotly Cumulative Infections", plotlyOutput("graph4"))
  #   )
  # )

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

  output$graph2 <- renderPlotly({
    ggplot(dataplot(), aes(fill= name, y=val, x=date)) +
      geom_bar(position="stack", stat = "identity")

  })
  
  output$graph3 <- renderPlotly({
    plot_ly(mexico, x = ~date, y = ~I, type = "bar") %>%
    layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
             list(orientation = "h", font = list(size = 16)), hovermode = "x unified") 
  })
    
  output$graph4 <- renderPlotly({ 
    plot_ly(mexico, x = ~date, y = ~R, type = "bar", hovermode = "x unified") %>%
    layout(barmode = "stack", title = list(xanchor = "right", x = 0), legend =
             list(orientation = "h", font = list(size = 16)))
  })
  
  output$graph5 <- renderPlot({
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
    
    p
  })
 
  # observeEvent(input[["navbar"]], {
  #   if(input[["navbar"]] == "Stacked Plotly 1"){
  #     dashboardSidebar(collapsed = FALSE)
  #     removeCssClass("main", "col-sm-8")
  #     addCssClass("main", "col-sm-12")
  #   }else{
  #     hideElement(selector = "#sidebar")
  #     removeCssClass("main", "col-sm-12")
  #     addCssClass("main", "col-sm-8")
    # }
  # })
  
}

shinyApp(ui=ui, server=server)



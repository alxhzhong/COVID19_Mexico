library(shiny)
library(plotly)
library(dplyr)

plotMexico <- mexico %>% 
  select("I", "R")

stack = bind_rows(
  mexico %>% dplyr::select(date, val = daily_deaths) %>%
    mutate(name = "Deaths"),
  mexico %>% dplyr::select(date, val = daily_infected) %>%
    mutate(name = "Infected"),
  mexico %>% dplyr::select(date, val = daily_recoveries) %>%
    mutate(name = "Recovered")
)




ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId =  "y", label = "label",
                choices = names(plotMexico)),
    checkboxGroupInput("name", "data:",
                       choices=unique(stack$name), selected = unique(stack$name))
  ),

  titlePanel("graphZ"),
  
  mainPanel(
    # mainPanel(
    #   plotlyOutput("graph"),
    #   plotlyOutput("graph2"),
    # ),
    navbarPage("Main",
       tabPanel("Stacked Plotly 1", plotlyOutput("graph")),
       tabPanel("Stacked Plotly 2", plotlyOutput("graph2")),
       navbarMenu("Cumulative",
            tabPanel("Plotly Cumulative Infections", plotlyOutput("graph3")),
            tabPanel("Plotly Cumulative Recoveries", plotlyOutput("graph4"))
            )
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
            type = "bar", hoverinfo = "text") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16))) %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

  })

  output$graph2 <- renderPlotly({
    ggplot(dataplot(), aes(fill= name, y=val, x=date)) +
      geom_bar(position="stack", stat = "identity")

  })
  
  output$graph3 <- renderPlotly({
    plot_ly(mexico, x = ~date, y = ~I, type = "bar", hoverinfo = "text") %>%
    layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
             list(orientation = "h", font = list(size = 16))) 
  })
    
  output$graph4 <- renderPlotly({ 
    plot_ly(mexico, x = ~date, y = ~R, type = "bar", hoverinfo = "text") %>%
    layout(barmode = "stack", title = list(xanchor = "right", x = 0), legend =
             list(orientation = "h", font = list(size = 16)))
  })
 
}

shinyApp(ui=ui, server=server)



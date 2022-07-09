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
    checkboxGroupInput("data", "data:", 
                       choices=unique(stack$name), selected = unique(stack$name)),
    checkboxInput(inputId = "show_data", label = "Show data table", value = TRUE)
  ),
  
  titlePanel("graph"),
  
  mainPanel(
    mainPanel(
      plotlyOutput("graphR")
    )
  )
  
)

server <- function(input, output, session){
  
  stackPlot = reactive({
    filter(stack, name %in% input$data)
  })
  
  
  output$graphR <- renderPlotly({
    plot_ly(stack, x = ~date, y =~val, color = ~name,
            type = "bar", hoverinfo = "text") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16))) %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

  })

  output$graph2 <- renderPlot({
    ggplot(stackPlot(), aes(fill=as.factor(name), y=val, x=date)) + 
      geom_bar(position="stack")

  })
  
 
}

shinyApp(ui=ui, server=server)
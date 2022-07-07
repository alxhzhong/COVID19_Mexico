library(shiny)

source("data_read.R")
# source("SIR_function.R")
# source("estimate_tvr.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    ggplot(mexico) +
      aes(x = date, y = I) +
      geom_col()
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)


# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Distribution selection"),

  sidebarLayout(

    sidebarPanel(


      radioButtons(inputId="dist1",label="Function Type:",
                   choices=list("Normal" = "Normal",
                                "Exponential" = "Exponential",
                                "Uniform" = "Uniform"))

    ),




    # Show a plot of the generated regression
    mainPanel(
      plotOutput("distPlot")


    )
  )
)



# Define server logic
server <- function(input, output) {

  dataInput <- reactive({


    switch(input$dist1,
           Normal = return(rnorm(500)),
           Exponential = return(rexp(500)),
           Uniform = return(runif(500))

    )
  })


  output$distPlot <- renderPlot({

    ggplot(mexico) +
      aes(x = date, y = I) +
      geom_col()

  })

}

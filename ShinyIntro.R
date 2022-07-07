adder <- function(x) {
  y <- 2*x + 1
  return(y)
}

adder2 <- function(x) {
  y <- 2*x + 1
  z <- 1:5
  return(c(y,z))
}

library(shiny)

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
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkblue', border = 'white')
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

    hist(dataInput(),xlab = "x",ylab="Counts",main="")

    xtitle <- paste("Distribution: ",input$dist1,sep="")

    title(xtitle)

  })

}

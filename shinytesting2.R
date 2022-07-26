library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

source("data_read.R")
source("SIR_intervals.R")
source("estimate_tvr.R")
source("mexicocity_dataread.R")
source("prediction_graphs.R")

# pred_SIR = sir_intervals("SIR")
# pred_I_SIR = pred_SIR[[1]]
# pred_R_SIR = pred_SIR[[2]]
# 
# pred_SEIR = sir_intervals("SEIR")
# pred_I_SEIR = pred_SEIR[[1]]
# pred_R_SEIR = pred_SEIR[[2]]

# for descriptive plots, to truncate timeframe to where recoveries stop reporting
mexicoDescriptives <- mexico %>% 
  filter(date <= "2021-08-04") 

# mxcity vs mexico
mx_mxc <- mxgov %>% 
  left_join(mxc_cl_all, by = "date")

# for SIR and SEIR I graphs, to smooth line
pred_I_SIR_graph <- pred_I_SIR %>% 
  mutate(index = 1:n()) %>% 
  mutate(loess = fitted(loess(pred_I_med ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(upper = fitted(loess(uprI ~ index, data = pred_I_SIR, span = 0.3))) %>% 
  mutate(lower = fitted(loess(lwrI ~ index, data = pred_I_SIR, span = 0.3)))

pred_I_SEIR_graph <- pred_I_SEIR %>% 
  mutate(index = 1:n()) %>% 
  mutate(loess = fitted(loess(pred_I_med ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
  mutate(upper = fitted(loess(uprI ~ index, data = pred_I_SEIR, span = 0.3))) %>% 
  mutate(lower = fitted(loess(lwrI ~ index, data = pred_I_SEIR, span = 0.3)))

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


# for TPR graph
mxgov = mxgov %>% mutate(text = paste0("TPR: ", tpr_rolavg)) %>% 
  mutate(percent = tpr_rolavg * 100) 


# for vaccinations
vaccinations <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Mexico.csv")
vaccinations <- vaccinations %>% 
  mutate(prop_2doses = people_fully_vaccinated / 128.9e6 *100) %>% 
  mutate(prop_1dose = people_vaccinated / 128.9e6 *100)


# general formatting
base <- list(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')

t <- list(
  color = "white" #, size = 18
  #, family = ""
)


css <- HTML(" body {
    background-color: #2a2a2b;
}")



# start of app

ui <- fluidPage(tags$head(tags$style(css)), theme = shinytheme("darkly"),
                
                # titlePanel("graphZ"),
                
                fluidRow(
                  # mainPanel(
                  #   plotlyOutput("graph"),
                  #   plotlyOutput("graph2"),
                  # ),
                  navbarPage("COVID-19 in Mexico",
                             
                             tabPanel("Home", titlePanel("Tracking and Modeling SARS-CoV-2 in Mexico"),
                                      imageOutput("mexicoFlag"),
                                      p("Welcome to our web app for tracking and modeling the COVID-19 pandemic in Mexico. Our aim is to provide a resource for individuals to understand COVID-19 trends and potential public health threats in Mexico. To accomplish this aim, we provide a brief policy explanation and numerous relevant graphs with descriptions. For more information on our group and this project, see the “About Us” tab."),
                                      br(),
                                      h4("Policy Overview"),
                                      p("Mexico suspended all nonessential activities at the beginning of the pandemic, in March 2020. At the end of May 2020, the national government's sanitary emergency expired, and Mexico switched to a stoplight system that operated at the state level. This system remains in place today. Indicators of COVID-19 are assessed weekly, and each state is assigned a color–green, yellow, orange, or red–based on certain indicators. More information on the colors is given below."),
                                      br(),
                                      h4("Stoplight Policy Information"),
                                      p("Red: stay at home if possible; masking is mandatory in all public spaces; economic and social restrictions activities are dictated by local and federal authorities"),
                                      p("Orange: reduction in community movement; masking is mandatory in all public spaces; economic and social activities at 50% capacity"),
                                      p("Yellow: slight decrease in community movement; masking is mandatory in indoor public spaces and public transportation and recommended in outdoor spaces in which social distancing is not possible; economic and social activities at 75% capacity"),
                                      p("Green: no movement restrictions; masking is recommended in indoor public spaces and mandatory on public transportation"),
                                      br(),
                                      h4("Web app overview"),
                                      p("The descriptive plots and statistics are displayed from the beginning of the pandemic until 08/04/2021, when data from our primary source, JHU, stops reporting recovered individuals. The predictive models are performed on a specific timeframe: the training period is 11/24/2020 to 01/13/2021, and the testing period is 01/14/2021 to 01/26/2021. In the relevant plots, these time periods are separated by a vertical white bar."),
                                      p("We provide estimations using three different epidemiological models–SIR, SEIR, and eSIR. SIR divides the population into three compartments, namely “Susceptible” individuals (S), “Infectious” individuals (I), and “Removed” individuals (R). SEIR adds a fourth compartment for “Exposed” individuals (E), which in SIR is included in the I compartment. Lastly, eSIR divides the population into the same three compartments as SIR but introduces a modifier to account for policy effects on transmission rates. SIR, SEIR, and eSIR have long been used by epidemiologists to model virus transmissions, and this web app allows you to compare the model-based predictions with the actual values in the time period we selected."),
                                      
                             ),
                             
                             
                             
                             navbarMenu("Descriptive Graphs",
                                        tabPanel("Cumulative Cases", titlePanel("Cumulative Reported Cases"), plotlyOutput("graphCumulativeI"),
                                                 br(),
                                                 p("This graph displays the cumulative number of positive COVID-19 tests since the start of the pandemic. You can hover your cursor over the graph to see the exact count on a particular day.")),
                                        tabPanel("Cumulative Removed", titlePanel("Cumulative Removed"), plotlyOutput("graphCumulativeR"),
                                                 br(),
                                                 p("This graph displays the cumulative number of individuals who have either recovered from COVID-19 or died while infected with COVID-19. You can hover your cursor over the graph to see the exact count on a particular day.")),
                                        tabPanel("Daily Active Cases", titlePanel("Daily Active Cases"), plotlyOutput("graphActiveI"),
                                                 br(),
                                                 p("This graph displays the daily number of active COVID-19 cases in Mexico. You can hover your cursor over the graph to see the exact count on a particular day.")),
                                        tabPanel("Stacked", titlePanel("Daily Recoveries, Infections, and Deaths"),
                                                 plotlyOutput("graphStacked"),
                                                 br(),
                                                 p("This graph displays the daily numbers of COVID-19 infections (positive tests), deaths, and recoveries in Mexico. You can hover your cursor over the graph to see the exact counts on a particular day.")
                                                 
                                                 # sidebarPanel(
                                                 #   # selectInput(inputId =  "y", label = "label",
                                                 #   #             choices = names(plotMexico)),
                                                 #   checkboxGroupInput("name", "Data:",
                                                 #                      choices=unique(stack$name), selected = unique(stack$name)),
                                                 #   id = "sidebar"
                                                 # )
                                        ),
                                        tabPanel("Mexico City Comparison", titlePanel("National Active Cases vs. Mexico City’s Active Cases"),
                                                 plotlyOutput("graphMXC"),
                                                 br(),
                                                 p("This graph displays the daily numbers of COVID-19 infections (positive tests) in both Mexico City and Mexico as a whole. You can hover your cursor over the graph to see the exact counts on a particular day."))
                             ),
                             
                             navbarMenu("Descriptive Statistics",
                                        tabPanel("Test Positivity Rate",
                                                 titlePanel("Test Positivity Rate (TPR)"),
                                                 plotlyOutput("TPRgraph"),
                                                 br(),
                                                 p("This graph displays the TPR in Mexico over the course of the pandemic. TPR represents the percentage of daily reported COVID-19 tests that come back positive. You can hover your cursor over the graph to see the TPR on a particular day.")),
                                        tabPanel("Vaccinations", titlePanel("Percentage of Population Vaccinated for COVID-19"),
                                                 plotlyOutput("graphVax"),
                                                 br(),
                                                 p("This graph displays the vaccination rates in Mexico since the start of its COVID-19 vaccination campaign. The orange plot represents the percentage of the population that has received one dose of a two-dose vaccine. The blue plot represents the percentage of the population that is fully vaccinated (i.e. the percentage that has received either both doses of a two-dose vaccine or one dose of a single-dose vaccine). You can hover your cursor over the graph to see the percentages on a particular day.")),
                                        tabPanel("R Estimation", titlePanel("R Estimation"),
                                                 plotlyOutput("graphR0"),
                                                 br(),
                                                 p("This graph displays an estimation of the time varying reproduction number, R(t), calculated using EpiEstim. R(t) is a measure of the expected number of secondary cases produced by a single infection at a specific point in time. You can hover your cursor over the graph to see the R(t) value and 95% confidence interval on a particular day."))
                                        
                             ),
                             
                             navbarMenu("SIR Estimations",
                                        tabPanel("SIR Active", titlePanel("SIR Active Cases Estimation"),
                                                 plotlyOutput("graphSIRActive"),
                                                 br(),
                                                 p("This graph displays the SIR model for active cases (the 'I' compartment in SIR), which is displayed as the orange line. The blue bars represent the actual number of active cases. The green line after the horizontal white line displays the model's predictions.")),
                                        tabPanel("SIR Removed", titlePanel("SIR Removed Cases Estimation"),
                                                 plotlyOutput("graphSIRRem"),
                                                 br(),
                                                 p("This graph displays the SIR model for recovered cases (the 'R' compartment in SIR), which is displayed as the orange line. The purple bars represent the actual number of recovered cases. The green line after the horizontal white line displays the model's predictions."))
                             ),
                             
                             navbarMenu("SEIR Estimations",
                                        tabPanel("SEIR Active", titlePanel("SEIR Active Cases Estimation"),
                                                 plotlyOutput("graphSEIRActive"),
                                                 br(),
                                                 p("This graph displays the SEIR model for active cases (the 'I' compartment in SEIR), which is displayed as the orange line. The blue bars represent the actual number of active cases. The green line after the horizontal white line displays the model's predictions.")),
                                        tabPanel("SEIR Removed", titlePanel("SEIR Removed Cases Estimation"),
                                                 plotlyOutput("graphSEIRRem"),
                                                 br(),
                                                 p("This graph displays the SEIR model for recovered cases (the 'R' compartment in SIR), which is displayed as the orange line. The purple bars represent the actual number of recovered cases. The green line after the horizontal white line displays the model's predictions."))),
                             tabPanel("About Us",
                                      titlePanel("About Us"),
                                      br(),
                                      p("Emily Bach is a rising senior at Georgetown University, Lauren He is a rising sophomore at Stanford University, and Alex Zhong is a rising senior at Emory University. We all participated in the 2022 Big Data Summer Institute (BDSI) run by the University of Michigan’s Department of Biostatistics. As part of our BDSI research project on infectious diseases, we created these interactive plots modeling the pandemic in Mexico.")),
                             tabPanel("References",
                                      titlePanel("Data Sources:"),
                                      br(),
                                      p("HYPERLINKS TO DATA SOURCES!"))
                             
                  )
                  
                )
                
)
server <- function(input, output, session){
  
  output$mexicoFlag <- renderImage({
    
    filename <- "mexicoFlag.png"
    list(src = filename, contentType = 'image/png', alt = paste("Mexico Flag"), style="display: block; margin-left: auto; margin-right: auto;",
         width = 500
  
    )
  }, deleteFile = FALSE)
  
  
  
  output$graphStacked <- renderPlotly({
    plot_ly(stack, x = ~date, y =~val, color = ~name, colors = c("#F5793A", "#60A5E8", "#A95AA1"),
            type = "bar") %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), #titlefont = axis_title_font,
             xaxis = list(title = "Date"),
             yaxis = list(title = "Daily Counts", range = list(0, 60000)),
             legend = list(orientation = "v", font = list(size = 16)), hovermode = "x unified",
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             font = t,
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)')
      ) %>% 
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
    
    
  })
  
  output$TPRgraph <- renderPlotly({
    plot_ly(mxgov, type = 'scatter', mode = 'lines', hoverlabel = list(align = "left",
                                                                       color = I("#F5793A")))%>%
      add_trace(x = ~date, y = ~percent, name = "TPR", 
                hovertemplate = "%{y} %",
                line = list(width = 3)) %>%
      layout(
        xaxis = list(title = "Date",
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = '#ffff'),
        yaxis = list(title = "Test Positivity Rate (7-day Ave.) %",
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = '#ffff',
                     hoverformat = "0.2f",
                     range = c(0,100)
                     #tickformat = ', .0%'
                     ),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)', 
        font = t,
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.8)'),
        hovermode = "x unified",
        showlegend = F
      )
  })
  
  output$graphCumulativeI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~cases_total, type = "bar",
            color = I("#60A5E8")) %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Total Cases'), #, hoverformat = ".2f"
             xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
             font = t
      )
  })
  
  output$graphCumulativeR <- renderPlotly({ 
    plot_ly(mexicoDescriptives, x = ~date, y = ~R, type = "bar",
            color = I("#A95AA1")) %>%
      layout(barmode = "stack", title = list(xanchor = "right", x = 0), legend =
               list(orientation = "h", font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Total Removed', hoverformat = "2"), 
             xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
             font = t)
  })
  
  output$graphActiveI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~I, type = "bar",
            color = I("#F5793A")) %>% 
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Active Infections'), xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
             font = t
      )
  })
  
  output$graphMXC <- renderPlotly({
    plot_ly(mx_mxc, x = ~date, y = ~daily_deaths.x, type = "bar",
            color = I("#F5793A"), name = "National") %>% 
      add_trace(y = ~mx_mxc$daily_deaths.y, color = I("#60A5E8"), name = "Mexico City") %>% 
      layout(barmode = "group", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Active Infections'), xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
             font = t
      )
  })
  
  
  output$graphVax <- renderPlotly({
    plot_ly(vaccinations, x = ~date, y = ~prop_1dose, type = "scatter", mode = "line", name = "1 of 2 doses", color = I("#F5793A"), fill = 'tozeroy', line = list(width = 3), hovertemplate = "%{y} %") %>%
      add_trace(y = ~vaccinations$prop_2doses, type = "scatter", mode = "line", name = "fully vaccinated", color = I("#60A5E8"), line = list(width = 3), hovertemplate = "%{y} %") %>% 
      layout(
        yaxis = list(title = "Percentage of Vaccinated Population", range = c(0,100), hoverformat = "0.2f"),
        xaxis = list(title = "Date"),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        hovermode = "x unified",
        font = t
      ) 
    
  })
  
  
  output$graphSIRActive <- renderPlotly({
    # plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual",
    #         color = I("#60A5E8")) %>% 
    #   add_trace(y = ~pred_I_SIR_graph$loess, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
    #   add_trace(y = ~pred_I_SIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
    #   add_trace(y = ~pred_I_SIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
    #   layout(
    #     xaxis = list(
    #       range=c(date_initial, date_final)),
    #     yaxis = list(title = 'Active Infections'),
    #     xaxis = list(title = 'Date'),
    #     hovermode = "x unified",
    #     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
    #     paper_bgcolor='rgba(0,0,0,0)',
    #     plot_bgcolor='rgba(0,0,0,0)',
    #     font = t
    #   )
    
    plot_ly(SIR_int, x = ~date, y = ~I, type = "bar", name = "Actual",
            color = I("#60A5E8")) %>% 
      add_trace(y = ~SIR_int$pred_I_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
      add_trace(y = ~SIR_int$uprI.y, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(100, 225, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SIR_int$lwrI.y, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(100, 225, 0, 0.2)'), name = "Lower", line = list(color = "rgba(100, 225, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
      add_trace(y = ~SIR_int$loess_m, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
      add_trace(y = ~SIR_int$upper_m, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SIR_int$lower_m, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>%
      add_lines(
        y = range(0:max(SIR_int$pred_I_med.y, na.rm = TRUE)), x = "2021-01-12",
        line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c("2020-11-24", "2021-01-26"), title = 'Date'),
        yaxis = list(title = 'Active Infections'),
        #hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
  })
  
  output$graphSIRRem <- renderPlotly({
    
    # plot_ly(mexicoSmall, x = ~date, y = ~R, type = "bar", name = "Actual",
    #         color = I("#A95AA1")) %>% 
    #   add_trace(y = ~pred_R_SIR$pred_R_med, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
    #   # add_trace(y = ~pred_R$uprR, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>% 
    #   # add_trace(y = ~pred_R$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE)
    #   add_ribbons(ymin = ~pred_R_SIR$lwrR,
    #               ymax = ~pred_R_SIR$uprR,
    #               line = list(color = 'rgb(245, 121, 58, 0.2)'),
    #               fillcolor = 'rgba(245, 121, 58, 0.2)',
    #               showlegend = FALSE) %>% 
    #   layout(
    #     xaxis = list(
    #       range=c(date_initial,date_final)),
    #     yaxis = list(title = 'Total Removed'),
    #     xaxis = list(title = 'Date'),
    #     hovermode = "x unified",
    #     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
    #     paper_bgcolor='rgba(0,0,0,0)',
    #     plot_bgcolor='rgba(0,0,0,0)',
    #     font = t
    #   )
    
    plot_ly(SIR_int, x = ~date, y = ~R, type = "bar", name = "Actual",
            color = I("#A95AA1")) %>% 
      add_trace(y = ~SIR_int$pred_R_med.x, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
      add_trace(y = ~SIR_int$uprR.x, type = 'scatter', mode = 'lines', name = "Upper", color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SIR_int$lwrR.x, type = 'scatter', mode = 'lines', fill = 'tonexty', color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE, hoverinfo = 'skip') %>% 
      # add_ribbons(ymin = ~SIR_int$lwrR.x,
      #             ymax = ~SIR_int$uprR.x,
      #             line = list(color = 'rgb(245, 121, 58, 0.2)'),
      #             fillcolor = 'rgba(245, 121, 58, 0.2)',
      #             showlegend = FALSE) %>% 
      add_trace(y = ~SIR_int$pred_R_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
      add_ribbons(ymin = ~SIR_int$lwrR.y,
                  ymax = ~SIR_int$uprR.y,
                  line = list(color = 'rgb(100, 225, 0, 0.2)'),
                  fillcolor = 'rgba(100, 225, 0, 0.2)',
                  showlegend = FALSE,
                  hoverinfo = "skip") %>% 
      add_lines(
        y = range(0:max(SIR_int$pred_R_med.y, na.rm = TRUE)), x = "2021-01-12",
        line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c("2020-11-24", "2021-01-26"), title = 'Date'),
        yaxis = list(title = 'Total Removed'),
        #hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
    
  })
  
  
  output$graphSEIRActive <- renderPlotly({
    
    # plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual",
    #         color = I("#60A5E8")) %>% 
    #   add_trace(y = ~pred_I_SEIR_graph$loess, type = 'scatter', mode = 'lines', name = "Model", line = list(color = 'rgba(245, 121, 58,, 1)', width = 3)) %>%
    #   add_trace(y = ~pred_I_SEIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE, hoverinfo = "none") %>% 
    #   add_trace(y = ~pred_I_SEIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.5)'), name = "Lower", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE, hoverinfo = "none") %>% 
    #   layout(
    #     xaxis = list(
    #       range=c(date_initial,date_final)),
    #     yaxis = list(title = 'Active Infections'),
    #     xaxis = list(title = 'Date'),
    #     hovermode = "x unified",
    #     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
    #     paper_bgcolor='rgba(0,0,0,0)',
    #     plot_bgcolor='rgba(0,0,0,0)',
    #     font = t
    #   )
    
    plot_ly(SEIR_int, x = ~date, y = ~I, type = "bar", name = "Actual",
            color = I("#60A5E8")) %>% 
      add_trace(y = ~SEIR_int$pred_I_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
      # add_trace(y = ~SEIR_I$uprI, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(44, 237, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      # add_trace(y = ~SEIR_I$lwrI, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(44, 237, 0, 0.2)'), name = "Lower", line = list(color = "rgba(44, 237, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
      add_trace(y = ~SEIR_int$loess, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
      add_trace(y = ~SEIR_int$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SEIR_int$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
      
      add_trace(y = ~SEIR_int$pred_uprI, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(100, 225, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SEIR_int$pred_lwrI, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(100, 225, 0, 0.2)'), name = "Lower", line = list(color = "rgba(100, 225, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>%
      add_lines(
        y = range(0:max(SEIR_int$pred_I_med.y, na.rm = TRUE)), x = "2021-01-12",
        line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c("2020-11-21", "2021-01-26"), title = 'Date'),
        yaxis = list(title = 'Active Infections'),
        #hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
  })
  
  output$graphSEIRRem <- renderPlotly({
    
    # plot_ly(mexicoSmall, x = ~date, y = ~R, type = "bar", name = "Actual", color = I("#A95AA1")) %>% 
    #   add_trace(y = ~pred_R_SEIR$pred_R_med, type = 'scatter', mode = 'lines', name = "Model", color = I("rgba(245, 121, 58, 1.0)"), width = 3)%>%
    #   # add_trace(y = ~pred_R_SEIR$uprR, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>%
    #   # add_trace(y = ~pred_R_SEIR$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE) %>% 
    #   add_ribbons(ymin = ~pred_R_SEIR$lwrR,
    #               ymax = ~pred_R_SEIR$uprR,
    #               # line = list(color = 'rgba(245, 121, 58, 0.1)'),
    #               # fillcolor = 'rgba(245, 121, 58, 0.3)',
    #               color = I('rgba(245, 121, 58, 0.5)'),
    #               showlegend = FALSE, hoverinfo = "none") %>% 
    #   layout(
    #     xaxis = list(
    #       range=c(date_initial,date_final)),
    #     yaxis = list(title = 'Total Removed'),
    #     xaxis = list(title = 'Date'),
    #     hovermode = "x unified",
    #     hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
    #     paper_bgcolor='rgba(0,0,0,0)',
    #     plot_bgcolor='rgba(0,0,0,0)',
    #     font = t
    #   )
    
    plot_ly(SEIR_int, x = ~date, y = ~R, type = "bar", name = "Actual", color = I("#A95AA1")) %>% 
      add_trace(y = ~SEIR_int$pred_R_med.x, type = 'scatter', mode = 'lines', name = "Model", color = I("rgba(245, 121, 58, 1.0)"), line = list(width = 3))%>%
      add_trace(y = ~SEIR_int$uprR, type = 'scatter', mode = 'lines', name = "Upper",  color = I("rgba(245, 121, 58, 0.5)"), showlegend = FALSE, hoverinfo = 'skip') %>%
      add_trace(y = ~SEIR_int$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty',  color = I("rgba(245, 121, 58, 0.5)"), name = "Lower", showlegend = FALSE, hoverinfo = 'skip') %>%
      # add_ribbons(ymin = ~SEIR_R$lwrR,
      #             ymax = ~SEIR_R$uprR,
      #             # line = list(color = 'rgba(245, 121, 58, 0.1)'),
      #             # fillcolor = 'rgba(245, 121, 58, 0.3)',
      #             color = I('rgba(245, 121, 58, 0.5)'),
      #             showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(y = ~SEIR_int$pred_R_med.y, type = 'scatter', mode = 'lines', line = list(color = "rgba(100, 225, 0, 1)", width = 3), name = "Prediction") %>%
      
      add_trace(y = ~SEIR_int$pred_uprR, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(100, 225, 0, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~SEIR_int$pred_lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(100, 225, 0, 0.2)'), name = "Lower", line = list(color = "rgba(100, 225, 0, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>%
      add_lines(
        y = range(0:max(SEIR_int$pred_R_med.y, na.rm = TRUE)), x = "2021-01-12",
        line = list(color = "white", width = 3), hoverinfo = "skip", showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c("2020-11-24", "2021-01-26"), title = 'Date'),
        yaxis = list(title = 'Total Removed'),
        #hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
    
  })
  
  
  
  
  output$graphR0 <- renderPlotly({
    
    plot_ly(plt_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
            line = list(width = 3),
            hoverinfo = "text",
            text   = ~text) %>%
      add_markers(data = plt_data, x = ~date, y = ~r, mode = "marker",
                  marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%
      add_ribbons(ymin = ~lower,
                  ymax = ~upper,
                  line = list(color = 'rgba(245, 121, 58, 0.5)'),
                  fillcolor = 'rgba(245, 121, 58, 0.5)',
                  hoverinfo = "none") %>%
      layout(
        title = list(xanchor = "left", x = 0), #text = cap,
        xaxis = list(title = "Date", #titlefont = axis_title_font,
                     zeroline = T, #tickfont = tickfont,
                     range=c(date_initial,date_final)),
        yaxis = list(title = "R(t)", #titlefont = axis_title_font,
                     zeroline = T), #tickfont = tickfont
        shapes = list(
          type = "line", xref = "paper", yref = "data",
          x0 = 0, x1 = 1, y0 = 1, y1 = 1,
          line = list(color = "rgba(255, 255, 255, 1.0)", width = 3)
        ),
        showlegend = FALSE,
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t) %>% 
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
    
    
  })
  
}



shinyApp(ui=ui, server=server)




library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

source("data_read.R")
source("SIR_intervals.R")
source("estimate_tvr.R")
source("mexicocity_dataread.R")

pred_SIR = sir_intervals("SIR")
pred_I_SIR = pred_SIR[[1]]
pred_R_SIR = pred_SIR[[2]]

pred_SEIR = sir_intervals("SEIR")
pred_I_SEIR = pred_SEIR[[1]]
pred_R_SEIR = pred_SEIR[[2]]

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
  color = "white"
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
                                      br(),
                                      imageOutput("mexicoFlag"),
                                      br(),
                                      p("Welcome to our web app for tracking and modeling SARS-CoV-2 pandemic in Mexico. Our aim is to provide a resource for individuals to visualize the COVID-19 trends in Mexico in order to better understand the situation. 
                                        some text, image ??, info about time period, policy, how to use graphs ?"),
                                      br(),
                                      h4("Policy overview"),
                                      p("Mexico suspended all nonessential activities at the beginning of the pandemic, in March 2020. At the end of May 2020, the national government's sanitary emergency expired, and Mexico shifted towards a stoplight system that operated at the state level. This is still the system that is currently in place. Indicators of COVID-19 are assessed weekly, and each state is assigned a color - green, yellow, orange, or red - based on the indicators."),
                                      br(),
                                      h4("Stoplight policy information"),
                                      p("Red: stay at home if possible, masking is mandatory in all public spaces, economic and social restrictions activities are dictated by local and federal authorities"),
                                      p("orange: reduction in community movement, masking is mandatory in all public spaces, economic and social activities at 50% capacity"),
                                      p("Yellow: slight decrease in community movement, masking is mandatory in indoor public spaces and public transportation and recommended in outdoor spaces in which social distancing is not possible, economic and social activities at 75% capacity"),
                                      p("Green: no movement restrictions, masking is recommended in indoor public spaces and mandatory on public transportation"),
                                      br(),
                                      h4("Web app overview"),
                                      p("The descriptive plots and statistics are displayed for the entirety of the pandemic until [date], when data from our primary source, JHU, stops reporting recovered individuals. The predictive models are performed on a specific timeframe- November 20, 2020 to March 1, 2021."),
                                      br(),
                                      p("Information about datasources can be found at the bottom of the 'About us' page.")
                             ),
                             
                             
                             
                             navbarMenu("Descriptive Graphs",
                                        tabPanel("Cumulative Cases", titlePanel("Cumulative Reported Cases"), plotlyOutput("graphCumulativeI"),
                                                 br(),
                                                 p("This graph displays the cumulative number of infections individuals since the start of the pandemic.")),
                                        tabPanel("Cumulative Removed", titlePanel("Cumulative Removed"), plotlyOutput("graphCumulativeR"),
                                                 br(),
                                                 p("This graph displays the cumulative number of removed individuals, which encompasses deaths and recoveries, since the start of the pandemic.")),
                                        tabPanel("Daily Active Cases", titlePanel("Daily Active Cases"), plotlyOutput("graphActiveI"),
                                                 br(),
                                                 p("This graph displays the daily number of active cases, which is obtained by subtracting the total number of removed by the total number of cases.")),
                                        tabPanel("Stacked", titlePanel("Stacked Plot"),
                                                 plotlyOutput("graphStacked"),
                                                 br(),
                                                 p("This graph displays the daily number of infections, deaths, and recoveries.")
                                                 
                                                 # sidebarPanel(
                                                 #   # selectInput(inputId =  "y", label = "label",
                                                 #   #             choices = names(plotMexico)),
                                                 #   checkboxGroupInput("name", "Data:",
                                                 #                      choices=unique(stack$name), selected = unique(stack$name)),
                                                 #   id = "sidebar"
                                                 # )
                                        ),
                                        tabPanel("Mexico City Comparison", titlePanel("Mexico City Compared with National"), plotlyOutput("graphMXC"))
                             ),
                             
                             navbarMenu("Descriptive Statistics",
                                        tabPanel("Test Positivity Rate",
                                                 titlePanel("Test Positivity Rate (TPR)"),
                                                 plotlyOutput("TPRgraph"),
                                                 br(),
                                                 p("This graph displays the TPR in Mexico. TPR is calculated by dividing the number of positive COVID-19 tests by the total number of COVID-19 tests performed.")),
                                        tabPanel("Vaccinations", titlePanel("Proportion of Population Vaccinated"),
                                                 plotlyOutput("graphVax"),
                                                 br(),
                                                 p("This graph displays the proportion of vaccinated individuals, which is calculated by dividing the number of individuals who have received 1 or 2 doses by the total population in Mexico.")),
                                        tabPanel("R Estimation", titlePanel("R Estimation"),
                                                 plotlyOutput("graphR0"),
                                                 br(),
                                                 p("This graph displays an estimation of the time varying reproduction number (Rt) over time using EpiEstim. Rt is a measure of the expected number of secondary cases produced by a single infection at a specific point in time."))
                                        
                             ),
                             
                             navbarMenu("SIR Estimations",
                                        tabPanel("SIR Active", titlePanel("SIR Active Cases Estimation"),
                                                 plotlyOutput("graphSIRActive"),
                                                 br(),
                                                 p("This graph displays the SIR prediction for active cases (the 'I' compartment in SIR), which is displayed as the orange line. The blue bars represent the actual number of active cases.")),
                                        tabPanel("SIR Removed", titlePanel("SIR Removed Cases Estimation"),
                                                 plotlyOutput("graphSIRRem"),
                                                 br(),
                                                 p("This graph displays the SIR prediction for recovered cases (the 'R' compartment in SIR), which is displayed as the orange line. The blue bars represent the actual number of recovered cases."))
                             ),
                             
                             navbarMenu("SEIR Estimations",
                                        tabPanel("SEIR Active", titlePanel("SEIR Active Cases Estimation"),
                                                 plotlyOutput("graphSEIRActive"),
                                                 br(),
                                                 p("This graph displays the SEIR prediction for active cases (the 'I' compartment in SEIR), which is displayed as the orange line. The blue bars represent the actual number of active cases.")),
                                        tabPanel("SEIR Removed", titlePanel("SEIR Removed Cases Estimation"),
                                                 plotlyOutput("graphSEIRRem"),
                                                 br(),
                                                 p("This graph displays the SEIR prediction for recovered cases (the 'R' compartment in SIR), which is displayed as the orange line. The blue bars represent the actual number of recovered cases."))),
                             tabPanel("About Us",
                                      titlePanel("About Us"),
                                      br(),
                                      p("text about us"),
                                      br(),
                                      p("info about data/sources"))
                             
                  )
                  
                )
                
)
server <- function(input, output, session){
  
  output$mexicoFlag <- renderImage({
    
    filename <- "mexicoFlag.png"
    list(src = filename, contentType = 'image/png', alt = paste("Mexico Flag"), style="display: block; margin-left: auto; margin-right: auto;",
         width = 500,
         height = 500
  
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
    plot_ly(vaccinations, x = ~date, y = ~prop_1dose, type = "scatter", mode = "line", name = "1 dose", color = I("#F5793A"), fill = 'tozeroy', line = list(width = 3), hovertemplate = "%{y} %") %>%
      add_trace(y = ~vaccinations$prop_2doses, type = "scatter", mode = "line", name = "2 doses", color = I("#60A5E8"), line = list(width = 3), hovertemplate = "%{y} %") %>% 
      layout(
        yaxis = list(title = "Percentage of vaccinated population", range = c(0,100), hoverformat = "0.2f"),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        hovermode = "x unified",
        font = t
      ) 
    
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
    # p1 <- p1 + labs(y = "Active Cases", x = "Date")
    # ggplotly(p1) %>% 
    #   layout(
    #     hovermode = "x unified")
    
    plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual",
            color = I("#60A5E8")) %>% 
      add_trace(y = ~pred_I_SIR_graph$loess, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)", width = 3), name = "Model") %>%
      add_trace(y = ~pred_I_SIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE, hoverinfo = 'skip') %>% 
      add_trace(y = ~pred_I_SIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), hoverinfo = 'skip', showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c(date_initial, date_final)),
        yaxis = list(title = 'Active Infections'),
        xaxis = list(title = 'Date'),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
  })
  
  output$graphSIRRem <- renderPlotly({
    
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
    
    plot_ly(mexicoSmall, x = ~date, y = ~R, type = "bar", name = "Actual",
            color = I("#A95AA1")) %>% 
      add_trace(y = ~pred_R_SIR$pred_R_med, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)', width = 3), name = "Model") %>%
      # add_trace(y = ~pred_R$uprR, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>% 
      # add_trace(y = ~pred_R$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE)
      add_ribbons(ymin = ~pred_R_SIR$lwrR,
                  ymax = ~pred_R_SIR$uprR,
                  line = list(color = 'rgb(245, 121, 58, 0.2)'),
                  fillcolor = 'rgba(245, 121, 58, 0.2)',
                  showlegend = FALSE) %>% 
      layout(
        xaxis = list(
          range=c(date_initial,date_final)),
        yaxis = list(title = 'Total Removed'),
        xaxis = list(title = 'Date'),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
    
  })
  
  
  output$graphSEIRActive <- renderPlotly({
    
    plot_ly(mexicoSmall, x = ~date, y = ~I, type = "bar", name = "Actual",
            color = I("#60A5E8")) %>% 
      add_trace(y = ~pred_I_SEIR_graph$loess, type = 'scatter', mode = 'lines', name = "Model", line = list(color = 'rgba(245, 121, 58,, 1)', width = 3)) %>%
      add_trace(y = ~pred_I_SEIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(y = ~pred_I_SEIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.5)'), name = "Lower", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE, hoverinfo = "none") %>% 
      layout(
        xaxis = list(
          range=c(date_initial,date_final)),
        yaxis = list(title = 'Active Infections'),
        xaxis = list(title = 'Date'),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t
      )
  })
  
  output$graphSEIRRem <- renderPlotly({
    
    plot_ly(mexicoSmall, x = ~date, y = ~R, type = "bar", name = "Actual", color = I("#A95AA1")) %>% 
      add_trace(y = ~pred_R_SEIR$pred_R_med, type = 'scatter', mode = 'lines', name = "Model", color = I("rgba(245, 121, 58, 1.0)"), width = 3)%>%
      # add_trace(y = ~pred_R_SEIR$uprR, type = 'scatter', mode = 'lines', name = "Upper", showlegend = FALSE) %>%
      # add_trace(y = ~pred_R_SEIR$lwrR, type = 'scatter', mode = 'lines', fill = 'tonexty', name = "Lower", showlegend = FALSE) %>% 
      add_ribbons(ymin = ~pred_R_SEIR$lwrR,
                  ymax = ~pred_R_SEIR$uprR,
                  # line = list(color = 'rgba(245, 121, 58, 0.1)'),
                  # fillcolor = 'rgba(245, 121, 58, 0.3)',
                  color = I('rgba(245, 121, 58, 0.5)'),
                  showlegend = FALSE, hoverinfo = "none") %>% 
      layout(
        xaxis = list(
          range=c(date_initial,date_final)),
        yaxis = list(title = 'Total Removed'),
        xaxis = list(title = 'Date'),
        hovermode = "x unified",
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




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
mxgov = mxgov %>% mutate(text = paste0("TPR: ", tpr_rolavg))


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
                  navbarPage("Tracking COVID-19 in Mexico",
                            
                             tabPanel("Stacked Plotly",
                                      titlePanel("title of graph"),
                                      mainPanel(
                                        h3("Or could put title here"),
                                        plotlyOutput("graphStacked"),
                                        br(),
                                        p("some descriptive text")
                                      ),
                                      sidebarPanel(
                                        # selectInput(inputId =  "y", label = "label",
                                        #             choices = names(plotMexico)),
                                        checkboxGroupInput("name", "Data:",
                                                           choices=unique(stack$name), selected = unique(stack$name)),
                                        id = "sidebar"
                                      )),
                             
                             tabPanel("Test Positivity Rate",
                                      titlePanel("title of graph"),
                                      plotlyOutput("TPRgraph"),
                                      br(),
                                      p("some descriptive text")),
                             
                             navbarMenu("Cumulative",
                                        tabPanel("Cumulative Infections", h3("title option"), plotlyOutput("graphCumulativeI"),
                                                 br(),
                                                 p("some descriptive text")),
                                        tabPanel("Cumulative Removed", plotlyOutput("graphCumulativeR"),
                                                 br(),
                                                 p("some descriptive text")),
                                        tabPanel("Daily Active Cases", plotlyOutput("graphActiveI"),
                                                 br(),
                                                 p("some descriptive text"))
                             ),
                             
                             navbarMenu("SIR Estimations",
                                        tabPanel("SIR Active", plotlyOutput("graphSIRActive"),
                                                 br(),
                                                 p("some descriptive text")),
                                        tabPanel("SIR Removed", plotlyOutput("graphSIRRem"),
                                                 br(),
                                                 p("some descriptive text"))
                             ),
                             
                             navbarMenu("SEIR Estimations",
                                        tabPanel("SEIR Active", plotlyOutput("graphSEIRActive"),
                                                 br(),
                                                 p("some descriptive text")),
                                        tabPanel("SEIR Removed", plotlyOutput("graphSEIRRem"),
                                                 br(),
                                                 p("some descriptive text"))),
                             
                             tabPanel("R Estimation", plotlyOutput("graphR0"),
                                      br(),
                                      p("some descriptive text"))
                             
                  )
                  
                )
                
)
server <- function(input, output, session){
  
  dataplot <- eventReactive(input$name, {
    stack <- stack %>% filter(as.factor(name) %in% c(input$name))
  })
  
  output$graphStacked <- renderPlotly({
    plot_ly(dataplot(), x = ~date, y =~val, color = ~name, colors = c("#F5793A", "#60A5E8", "#A95AA1"),
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
      add_trace(x = ~date, y = ~tpr_rolavg, name = "test", text = ~text, hoverinfo = 'text') %>%
      layout(showlegend = F) %>% 
      layout(
        xaxis = list(title = "Date",
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = '#ffff'),
        yaxis = list(title = "Test Positivity Rate (7-day Ave.)",
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = '#ffff'),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)', 
        font = t,
        hoverlabel = list(bgcolor = 'rgba(0,0,0,0)')
        )
  })
  
  output$graphCumulativeI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~cases_total, type = "bar",
            color = I("#60A5E8")) %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Total Cases', hoverformat = ".2f"), 
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
             yaxis = list(title = 'Total Removed', hoverformat = "0.2"), 
             xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
             font = t)
  })
  
  output$graphActiveI <- renderPlotly({
    plot_ly(mexicoDescriptives, x = ~date, y = ~I, type = "bar",
            color = I("#0F2080")) %>%
      layout(barmode = "stack", title = list(xanchor = "left", x = 0), legend =
               list(font = list(size = 16)), hovermode = "x unified",
             yaxis = list(title = 'Active Infections'), xaxis = list(title = 'Date'),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             hoverlabel = list(bgcolor = 'rgba(0,0,0,0.5)'),
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
      add_trace(y = ~pred_I_SIR_graph$loess, type = 'scatter', mode = 'lines', line = list(color = "rgba(245, 121, 58, 1)"), name = "Predicted") %>%
      add_trace(y = ~pred_I_SIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE) %>% 
      add_trace(y = ~pred_I_SIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.2)'), name = "Lower", line = list(color = "rgba(245, 121, 58, 0.2)"), showlegend = FALSE) %>% 
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
      add_trace(y = ~pred_R_SIR$pred_R_med, type = 'scatter', mode = 'lines', line = list(color = 'rgb(245, 121, 58,, 1)'), name = "Predicted") %>%
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
      add_trace(y = ~pred_I_SEIR_graph$loess, type = 'scatter', mode = 'lines', name = "Predicted", line = list(color = 'rgba(245, 121, 58,, 1)')) %>%
      add_trace(y = ~pred_I_SEIR_graph$upper, type = 'scatter', mode = 'lines', name = "Upper", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE) %>% 
      add_trace(y = ~pred_I_SEIR_graph$lower, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = list(color = 'rgba(245, 121, 58, 0.5)'), name = "Lower", line = list(color = 'rgba(245, 121, 58, 0.5)'), showlegend = FALSE) %>% 
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
      add_trace(y = ~pred_R_SEIR$pred_R_med, type = 'scatter', mode = 'lines', name = "Predicted", color = I("rgba(245, 121, 58, 1.0)"))%>%
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
            # add line color?
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




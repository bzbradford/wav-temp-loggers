library(tidyverse)
library(shiny)
library(plotly)
library(shinyBS)

loggers <- unique(therm_data$LoggerSN)

ui <- fluidPage(
  tags$head(
    tags$style("
      .container-fluid {
        max-width: 1000px;
      }
      
      h4 {
        border-bottom: 2px solid lightgrey;
      }
    "
    )
  ),
  
  h1("Water Action Volunteers", align = "center"),
  
  br(),
  
  h3("2021 Temperature Logger Data", align = "center"),
  
  br(),
  
  selectInput(
    inputId = "logger",
    label = "Select temperature logger SN:",
    choices = loggers
  ),
  
  fluidRow(
    column(12,
      h4("Logger data time series"),
      plotlyOutput("plot")
    )
  ),
  
  br(),
  
  fluidRow(
    column(6,
      h4("Station info"),
      tableOutput("stnInfoTable")
    ),
    column(6,
      h4("Station data summary"),
      tableOutput("stnSummaryTable")
    )
  ),
  
)

server <- function(input, output, sessions) {
  # select station data
  stn_data <- reactive({
    dplyr::filter(therm_data, LoggerSN == input$logger) %>%
    arrange(DateTime)
  })
  
  # get matching station inventory
  stn_info <- reactive({
    therm_inventory %>%
    dplyr::filter(`2021 Therm SN` == input$logger) %>%
    pivot_longer(everything(), values_transform = as.character) %>%
    arrange(name)
  })
  
  # create station daily totals
  daily <- reactive({
    df <- stn_data()
    req(nrow(df) > 0)
    
    df %>%
    group_by(Date) %>%
    summarise(
      N = n(),
      Min = min(Temp_F),
      Max = max(Temp_F),
      Mean = round(mean(Temp_F), 2),
      Lat = Latitude[1],
      Long = Longitude[1]
    ) %>%
    dplyr::filter(N == 24) %>%
    mutate(DateTime = as.POSIXct(paste(Date, "12:00:00")))
  })
  
  # create station summary for display
  stn_summary <- reactive({
    df <- daily()
    req(nrow(df) > 0)
    
    df %>%
    summarise(
      Location = paste0(Lat[1], ", ", Long[1]),
      `Date Deployed` = as.Date(min(Date)),
      `Date Retrieved` = as.Date(max(Date)),
      `Number of Days` = n(),
      `Number of Readings` = sum(N),
      `Max Temperature` = max(Max),
      `Min Temperature` = min(Min),
      `Max Daily Average` = max(Mean),
      `Min Daily Average` = min(Mean),
      `Average Temperature` = mean(Mean)
    ) %>%
    mutate(
      across(`Max Temperature`:`Average Temperature`, round, 2),
      across(`Max Temperature`:`Average Temperature`, paste, "°F")) %>%
    pivot_longer(everything(), values_transform = as.character)
  })
  
  output$stnInfoTable <- renderTable({
    stn_info()
  })
  
  output$stnSummaryTable <- renderTable({
    stn_summary()
  })
  
  # plot hourly and daily temp data
  output$plot <- renderPlotly({
    df_daily <- daily()
    df_hourly <- stn_data()
    logger_name <- input$logger
    
    req(nrow(df_daily) > 0)
    req(nrow(df_hourly) > 0)
    
    plot_ly() %>%
      add_ribbons(
        data = df_daily,
        x = ~ DateTime,
        ymin = ~ Min,
        ymax = ~ Max,
        line = list(
          color = "lightblue",
          opacity = 0),
        fillcolor = "lightblue",
        opacity = 0.5,
        name = "Daily Range",
        hovertemplate = "Daily Range<extra></extra>"
      ) %>%
      add_lines(
        data = df_daily,
        x = ~ DateTime,
        y = ~ Min,
        line = list(
          color = "lightblue",
          opacity = 0.6),
        name = "Daily Min",
        showlegend = F
      ) %>%
      add_lines(
        data = df_daily,
        x = ~ DateTime,
        y = ~ Max,
        line = list(
          color = "lightblue",
          opacity = 0.6),
        name = "Daily Max",
        showlegend = F
      ) %>%
      add_trace(
        data = df_hourly,
        x = ~ DateTime,
        y = ~ Temp_F,
        name = "Hourly Temperature (F)",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#1f77b4",
          width = 1,
          opacity = 0.8
        )) %>%
      add_trace(
        data = df_daily,
        x = ~ DateTime,
        y = ~ Mean,
        name = "Mean Daily Temp. (F)",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "orange"
        )) %>% 
      layout(
        title = paste("Temperature at", logger_name),
        showlegend = TRUE,
        xaxis = list(title = "Date and Time"),
        yaxis = list(title = "Temperature (F)"),
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.25,
          y = 1
        )
      )
  })
  
}

shinyApp(ui, server)

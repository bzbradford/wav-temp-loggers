library(tidyverse)
library(shiny)
library(plotly)
library(shinyBS)
library(leaflet)


# Load data ---------------------------------------------------------------


therm_data <- read_csv("data/2021-thermistor-data.csv.gz")
therm_inventory <- read_csv("data/thermistor-inventory-2021.csv") %>%
  drop_na(`2021 Therm SN`) %>%
  filter(`2021 Therm SN` %in% therm_data$LoggerSN) %>%
  arrange(`Station ID`)
therm_locs <- read_csv("data/thermistor-locations.csv") %>%
  left_join({
    therm_inventory %>%
      select(
        StationID = `Station ID`,
        LoggerSN = `2021 Therm SN`
      )
  }) %>%
  drop_na(LoggerSN)

loggers <- unique(therm_data$LoggerSN)

logger_list <- list()
for (i in 1:nrow(therm_inventory)) {
  sn = therm_inventory$`2021 Therm SN`[i]
  id = therm_inventory$`Station ID`[i]
  name = therm_inventory$`WAV Station Name`[i]
  label = ifelse(
    is.na(id),
    paste0("SN ", sn, ", Unknown WAV station"),
    paste0("SN ", sn, ", WAV station ", id, ": ", name)
  )
  logger_list[[label]] = sn
}


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  title = "WAV Temperature Logger Data",
  
  tags$head(
    includeHTML("google-analytics.html"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style("
      body {
        font-family: 'Lato', sans-serif;
      }
      
      .container-fluid {
        max-width: 1000px;
      }
      
      .panel-body {
        padding: 0px;
      }
      
      .panel-body p {
        padding: 0.5em 1em 0em 1em;
      }
    ")
  ),
  
  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),
  
  br(),
  
  h2("2021 Temperature Logger Data", align = "center"),
  
  br(),
  
  selectInput(
    inputId = "logger",
    label = "Select temperature logger:",
    choices = logger_list,
    width = "100%"
  ),
  
  bsCollapse(
    bsCollapsePanel(
      title = "Logger location map",
      value = "map",
      uiOutput("mapUI")
    ),
    open = "map"
  ),
  
  br(),
  
  uiOutput("plotUI"),
  br(),
  
  uiOutput("infoUI"),
  br(),
  
  uiOutput("downloadUI"),
  br(),
  
  fluidRow(
    column(12,
      h4("More information:"),
      p(a("Water Action Volunteers", href = "https://wateractionvolunteers.org", target = "_blank"), "is an organization run by the UW-Madison Division of Extension and the Wisconsin Department of Natural Resources. The aim is to engage volunteers across the state to perform repeated, periodic water quality measurements of local streams to improve our understanding of stream health and to inform conservation and restoration efforts. At some stations, automatic temperature loggers are deployed which capture water temperature at hourly intervals. Colder water is generally associated with cleaner, healthier stream ecosystems, including higher insect and fish populations."),
      p("Temperature is often referred to as a 'master variable' in aquatic ecosystems because temperature determines the speed of important processes, from basic chemical reactions to the growth and metabolism of fishes and other organisms. In addition, temperature determines the type of fish community that the stream can support. It is especially important to gather stream temperature information over many years in order to track how quickly our streams are warming due to climate change. The continuous data loggers you have deployed and maintained are a 21st-century approach to monitoring stream temperature, providing accurate, high-resolution temperature data as we monitor the health of streams across the state."),
      p("Tips for understanding and interacting with the temperature plot:"),
      tags$ul(
        tags$li("Hover over the chart to see hourly and daily temperature measurement details."),
        tags$li("Click on the legend to show / hide the time series of hourly or daily temperature."),
        tags$li("Drag the mouse over a time period to zoom in, and double click to return to the original view."),
        tags$li("While hovering over the plot, click the camera icon along the top to download a picture of the plot."),
        tags$li("Anomalous temperature readings at the very beginning and end of the data may reflect air temperatures before the logger was deployed into the stream. It's also possible that the logger because exposed to the air during deployment if water levels dropped.")
      )
    )
  ),
  
  br(),
  
  p(
    style = "color: grey; font-size: smaller;",
    align = "center",
    em(paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")))
  )
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, sessions) {
  
  ## Reactive values ----
  
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
  
  
  
  ## UIs ----
  
  output$mapUI <- renderUI({
    list(
      leafletOutput("map"),
      p(em(HTML("Currently selected logger is shown in <span style='color: green;'>green</span>."), "Click on any other logger to select it."))
    )
    
  })
  
  output$plotUI <- renderUI({
    fluidRow(
      column(12,
        h4("Logger data time series"),
        plotlyOutput("plot")
      )
    )
  })
  
  output$infoUI <- renderUI({
    fluidRow(
      column(6,
        h4("Station info"),
        br(),
        renderTable(stn_info(), colnames = F)
      ),
      column(6,
        h4("Station data summary"),
        br(),
        renderTable(stn_summary(), colnames = F)
      )
    )
  })
  
  output$downloadUI <- renderUI({
    cur_label <- paste0("Download current logger data (", nrow(stn_data()), " observations)")
    all_label <- paste0("Download all logger data (", length(loggers), " loggers, ", nrow(therm_data), " observations)")
    fluidRow(
      column(12,
        h4("Download data:"),
        downloadButton("selectedLoggerDL", cur_label),
        downloadButton("allLoggerDL", all_label)
      )
    )
  })
  
  
  
  ## Map ----
  
  output$map <- renderLeaflet({
    leaflet(therm_locs) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "WAV station ", StationID, ": ", StationName, "<br>",
          "Logger SN: ", LoggerSN),
          HTML),
        layerId = ~as.character(LoggerSN),
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "purple",
        fillOpacity = 0.5
      ) %>%
      addCircleMarkers(
        data = therm_locs[1,],
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~HTML(paste0(
          "WAV station ", StationID, ": ", StationName, "<br>",
          "Logger SN: ", LoggerSN)),
        layerId = "cur_point",
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "green",
        fillOpacity = 0.75
      )
  })
  
  # handle displaying selected logger in green
  observe({
    req(input$logger)
    
    leafletProxy("map") %>%
      removeMarker("cur_point")
    
    cur_stn <- therm_locs %>%
      filter(LoggerSN == input$logger)
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = cur_stn,
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~HTML(paste0(
          "WAV station ", StationID, ": ", StationName, "<br>",
          "Logger SN: ", LoggerSN)),
        layerId = "cur_point",
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "green",
        fillOpacity = 0.75
      )
  })
  
  # get clicked logger location and select it
  observe({
    updateSelectInput(
      inputId = "logger",
      selected = input$map_marker_click
    )
  })
  
  
  ## Download buttons ----
  
  output$allLoggerDL <- downloadHandler(
    filename = "wav-thermistor-data.csv",
    content = function(file) {write_csv(therm_data, file)}
  )
  
  output$selectedLoggerDL <- downloadHandler(
    filename = paste0("wav-thermistor-data-logger-", input$logger, ".csv"),
    content = function(file) {write_csv(stn_data(), file)}
  )
  
  
  
  ## Plot ----
  
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
        title = paste("Temperature at logger", logger_name),
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

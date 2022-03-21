library(tidyverse)
library(shiny)
library(plotly)
library(shinyBS)
library(leaflet)
library(htmlwidgets)


# Functions ---------------------------------------------------------------

f_to_c <- function(f) {
  (f - 32) * 5.0 / 9.0
}



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
  
  uiOutput("loggerSelectUI", style = "z-index: 1100;"),
  uiOutput("mapUI"),
  uiOutput("plotUI"),
  # uiOutput("infoUI"),
  # br(),
  
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
  
  uiOutput("downloadUI"), br(),
  
  br(),
  
  hr(),
  
  p(
    style = "color: grey; font-size: smaller; font-style: italic;",
    align = "center",
    "Developed by Ben Bradford, UW-Madison Entomology", br(),
    paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")), br(),
    a("Source code", href = "https://github.com/bzbradford/wav-temp-loggers", target = "_blank")
  )
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, sessions) {
  
  ## Reactive values ----
  
  random_logger <- sample(therm_locs$LoggerSN, 1)
  
  cur_stn <- reactive({
    req(input$logger)
    
    therm_locs %>%
      filter(LoggerSN == input$logger)
  })
  
  # select station data
  stn_data <- reactive({
    req(input$logger)
    
    therm_data %>%
      filter(LoggerSN == input$logger) %>%
      arrange(DateTime)
  })
  
  
  
  ## Logger select ----
  
  output$loggerSelectUI <- renderUI({
    fluidRow(
      column(12,
        selectInput(
          inputId = "logger",
          label = "Select temperature logger:",
          choices = logger_list,
          selected = random_logger,
          width = "100%"
        ),
        style = "z-index: 1001;"
      )
    )

  })
  
  
  
  ## Map ----
  
  output$mapUI <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Logger location map",
        value = "map",
        leafletOutput("map"),
        div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
          p(em(HTML("Currently selected logger is shown in <span style='color: green;'>green</span>."), "Click on any other logger to select it, or choose from the list above.")),
          p(
            actionButton("zoom_in", "Zoom to selected site"),
            actionButton("reset_zoom", "Zoom out to all sites")
          )
        )
      ),
      open = "map"
    )
  })
  
  basemaps <- c(
    "Grey Canvas",
    "Open Street Map",
    "ESRI Topo Map",
    "Nat Geo Topo Map"
  )
  
  output$map <- renderLeaflet({
    leaflet(therm_locs) %>%
      addTiles(group = basemaps[2]) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps[1]) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = basemaps[4]) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = basemaps[3]) %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "<b>Station ID:</b> ", StationID, "<br>",
          "<b>Station Name:</b> ", gsub("\n", "<br>&nbsp;&nbsp;&nbsp;&nbsp;", str_wrap(StationName, width = 40)), "<br>",
          "<b>Logger SN:</b> ", LoggerSN),
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
        data = filter(therm_locs, LoggerSN == random_logger),
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "<b>Station ID:</b> ", StationID, "<br>",
          "<b>Station Name:</b> ", str_trunc(StationName, width = 50), "<br>",
          "<b>Logger SN:</b> ", LoggerSN),
          HTML),
        layerId = "cur_point",
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "green",
        fillOpacity = 0.75
      ) %>%
      addLayersControl(
        baseGroups = basemaps,
        options = layersControlOptions(collapsed = T)
      ) %>%
      htmlwidgets::onRender("
        function() {
          $('.leaflet-control-layers-list').prepend('<b>Basemap:</b>');
        }
      ")
  })
  
  # handle displaying selected logger in green
  observe({
    leafletProxy("map") %>%
      removeMarker("cur_point")
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = cur_stn(),
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "<b>Station ID:</b> ", StationID, "<br>",
          "<b>Station Name:</b> ", str_trunc(StationName, width = 50), "<br>",
          "<b>Logger SN:</b> ", LoggerSN),
          HTML),
        layerId = "cur_point",
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "green",
        fillOpacity = 0.75
      )
  })
  
  observeEvent(input$zoom_in, {
    leafletProxy("map") %>%
      setView(
        lat = cur_stn()$Latitude,
        lng = cur_stn()$Longitude,
        zoom = 10
      )
  })

  observeEvent(input$reset_zoom, {
    leafletProxy("map") %>%
      fitBounds(
        lat1 = min(therm_locs$Latitude),
        lat2 = max(therm_locs$Latitude),
        lng1 = min(therm_locs$Longitude),
        lng2 = max(therm_locs$Longitude)
      )
  })
  
  # get clicked logger location and select it
  observe({
    updateSelectInput(
      inputId = "logger",
      selected = input$map_marker_click
    )
  })
  
  
  
  ## Station summary info ----
  
  output$infoUI <- renderUI({
    fluidRow(
      column(6,
        h4("Logger data summary:"),
        br(),
        renderTable(stn_summary(), colnames = F)
      ),
      column(6,
        h4("Additional station information:"),
        br(),
        renderTable(stn_info(), colnames = F)
      )
    )
  })
  
  # get matching station inventory
  stn_info <- reactive({
    req(input$logger)
    
    therm_inventory %>%
      dplyr::filter(`2021 Therm SN` == input$logger) %>%
      pivot_longer(everything(), values_transform = as.character) %>%
      arrange(name)
  })
  
  # create station daily totals
  daily <- reactive({
    df <- stn_data()
    req(nrow(df) > 0)
    req(input$tempUnits)
    temp_col <- paste0("Temp_", input$tempUnits)
    
    df %>%
      group_by(Date) %>%
      summarise(
        N = n(),
        Min = min(!!rlang::sym(temp_col)),
        Max = max(!!rlang::sym(temp_col)),
        Mean = round(mean(!!rlang::sym(temp_col)), 2),
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
    req(input$tempUnits)
    units <- input$tempUnits
    
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
        across(`Max Temperature`:`Average Temperature`, paste, paste0("°", units))) %>%
      pivot_longer(everything(), values_transform = as.character)
  })
  
  
  
  
  ## Plot ----
  
  output$plotUI <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Logger data time series",
        value = "plot",
        br(),
        plotlyOutput("plot"),
        div(style = "margin: 0.5em 1em; 0.5em 1em;",
          p(em("High or widely fluctuating temperatures may indicate that the logger became exposed to the air, either before/after deployment, or when stream levels dropped below the point where the logger was anchored.")),
          uiOutput("annotationText"),
          hr(),
          p(
            div(strong("Temperature units:"), style = "float: left; margin-right: 1em;"),
            radioButtons(
              inputId = "tempUnits",
              label = NULL,
              inline = T,
              choices = c("Fahrenheit" = "F", "Celcius" = "C")
            ),
            div(strong("Optional plot annotations:"), style = "float: left; margin-right: 1em;"),
            radioButtons(
              inputId = "plotAnnotation",
              label = NULL,
              inline = T,
              choices = c("Brook trout temperature range", "None")
            )
          ),
          hr(),
          uiOutput("infoUI")
        )
      ),
      open = "plot"
    )
  })
  
  output$annotationText <- renderUI({
    if (grepl("Brook trout", input$plotAnnotation)) {
      req(input$tempUnits)
      units <- input$tempUnits
      temps <- c(52, 61, 72)
      if (units == "C") { temps <- round(f_to_c(temps), 1) }
      msg <- paste0("Optimal brook trout temperatures are shown shaded dark green (", temps[1], "-", temps[2], "°", units, "), acceptable temperatures in light green (", temps[2], "-", temps[3], "°", units, "), too hot in orange, and too cold in blue.")
      p(em(msg))
    }
  })
  
  rect <- function(ymin, ymax, color = "red") {
    list(
      type = "rect",
      fillcolor = color,
      line = list(color = color),
      opacity = 0.1,
      y0 = ymin,
      y1 = ymax,
      xref = "paper",
      x0 = 0,
      x1 = 1,
      layer = "below"
    )
  }
  
  output$plot <- renderPlotly({
    req(input$logger)
    req(input$tempUnits)
    
    df_daily <- daily()
    df_hourly <- stn_data()
    logger_name <- input$logger
    
    req(nrow(df_daily) > 0)
    req(nrow(df_hourly) > 0)
    
    # handle units
    units <- input$tempUnits
    temp_col <- paste0("Temp_", units)
    ytitle <- paste0("Temperature (°", units, ")")
    if (units == "F") {
      yrange <- c(30, 100)
    } else {
      yrange <- c(0, 37)
    }
    
    plt <- plot_ly() %>%
      add_ribbons(
        data = df_daily,
        x = ~ DateTime,
        ymin = ~ Min,
        ymax = ~ Max,
        line = list(
          color = "lightblue",
          width = 0.5,
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
          width = 1,
          opacity = 0.5),
        name = "Daily Min",
        showlegend = F
      ) %>%
      add_lines(
        data = df_daily,
        x = ~ DateTime,
        y = ~ Max,
        line = list(
          color = "lightblue",
          width = 1,
          opacity = 0.5),
        name = "Daily Max",
        showlegend = F
      ) %>%
      add_trace(
        x = df_hourly$DateTime,
        y = df_hourly[[temp_col]],
        name = "Hourly Temperature",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#1f77b4",
          width = 0.5,
          opacity = 0.8
        )) %>%
      add_trace(
        data = df_daily,
        x = ~ DateTime,
        y = ~ Mean,
        name = "Mean Daily Temp.",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "orange"
        )) %>% 
      layout(
        title = ifelse(
          logger_name %in% therm_locs$LoggerSN,
          str_trunc(paste0("Station ", cur_stn()$StationID, ": ", cur_stn()$StationName), width = 80),
          paste0("Logger ", logger_name, " (Unknown WAV Station)")),
        showlegend = TRUE,
        xaxis = list(title = "Date and Time"),
        yaxis = list(
          title = ytitle,
          range = yrange,
          zerolinecolor = "lightgrey"),
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.25,
          y = 1
        ),
        margin = list(t = 50)
      )
    
    if (grepl("Brook trout", input$plotAnnotation)) {
      temps <- c(32, 52, 61, 72, 100)
      if (units == "C") { temps <- f_to_c(temps) }
      colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
      
      plt <- plt %>%
        layout(
          shapes = lapply(1:length(colors), function(i) {
            rect(temps[i], temps[i + 1], colors[i])
          })
        )
    }
    
    plt
  })
  
  
  
  
  
  
  
  ## Download buttons ----
  
  output$downloadUI <- renderUI({
    cur_label <- paste0("Download current logger data (", nrow(stn_data()), " observations)")
    all_label <- paste0("Download all logger data (", length(logger_list), " loggers, ", nrow(therm_data), " observations)")
    fluidRow(
      column(12,
        h4("Download data:"),
        downloadButton("selectedLoggerDL", cur_label),
        downloadButton("allLoggerDL", all_label)
      )
    )
  })
  
  output$allLoggerDL <- downloadHandler(
    filename = "wav-thermistor-data.csv",
    content = function(file) {write_csv(therm_data, file)}
  )
  
  output$selectedLoggerDL <- downloadHandler(
    filename = paste0("wav-thermistor-data-logger-", input$logger, ".csv"),
    content = function(file) {write_csv(stn_data(), file)}
  )
  
  
  
}

shinyApp(ui, server)

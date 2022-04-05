library(tidyverse)
library(shiny)
library(plotly)
library(shinyBS)
library(leaflet)
library(htmlwidgets)
library(DT)


# Functions ---------------------------------------------------------------

c_to_f <- function(c, d = 1) {
  round(c * 9.0 / 5.0 + 32, d)
}

f_to_c <- function(f, d = 1) {
  round((f - 32) * 5.0 / 9.0, d)
}

colorize <- function(text, color) {
  paste0("<span style='color: ", color, "'>", text, "</span>")
}




# Load data ---------------------------------------------------------------

years <- c(2020, 2021)

zips <- list(
  "2020" = "downloads/2020-thermistor-data.zip",
  "2021" = "downloads/2021-thermistor-data.zip"
)

therm_data <- bind_rows(
  read_csv("data/2020-thermistor-data.csv.gz", col_types = cols()),
  read_csv("data/2021-thermistor-data.csv.gz", col_types = cols())
)

therm_inventory_in <- bind_rows(
  read_csv("data/2020-therm-inventory.csv", col_types = cols()),
  read_csv("data/2021-therm-inventory.csv", col_types = cols())
)

therm_years <- therm_inventory_in %>%
  group_by(StationID) %>%
  summarise(YearsActive = paste(unlist(Year), collapse = ", "))

therm_inventory <- left_join(therm_inventory_in, therm_years, by = "StationID") %>%
  select(Year, YearsActive, everything())



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
      
      .has-feedback .form-control {
        padding-right: 12px;
        min-width: 5em;
      }
    ")
  ),
  
  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),
  
  br(),
  
  h2("Continuous Temperature Monitoring Data", align = "center"),
  
  br(),
  
  p(
    div(style = "float: left; margin-right: 1em;", strong("Select data year:")),
    radioButtons(
      inputId = "year",
      label = NULL,
      choices = years,
      selected = max(years),
      inline = T
    )
  ),
  uiOutput("loggerSelectUI", style = "z-index: 1100;"),
  uiOutput("mapUI"),
  uiOutput("plotUI"),
  uiOutput("loggerListUI"),
  uiOutput("downloadUI"),
  
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
  
  ## On startup ----

  random_stn <- therm_inventory %>%
    filter(Year == max(years)) %>%
    pull(StationID) %>%
    sample(1)
  
  
  
  ## Reactive values ----
  
  logger_list <- reactive({
    req(input$year)
    
    therm_inventory %>%
      filter(Year == input$year) %>%
      mutate(Label = paste0("Station ", StationID, ": ", StationName)) %>%
      arrange(StationID) %>%
      select(Label, StationID) %>%
      deframe() %>%
      as.list()
  })
  
  cur_stn <- reactive({
    req(input$year)
    req(input$logger)
    
    therm_inventory %>%
      filter(Year == input$year, StationID == input$logger)
  })
  
  # select station data
  stn_data <- reactive({
    req(input$year)
    req(input$logger)
    
    therm_data %>%
      filter(Year == input$year, StationID == input$logger) %>%
      arrange(DateTime)
  })
  
  
  
  ## Logger select ----
  
  output$loggerSelectUI <- renderUI({
    fluidRow(
      column(12,
        selectInput(
          inputId = "logger",
          label = "Select temperature logger:",
          choices = list("Select a station" = NULL),
          width = "100%"
        ),
        style = "z-index: 1001;"
      )
    )
  })
  
  observeEvent(input$year, {
    stations <- logger_list()
    if (is.null(input$logger)) {
      selected <- random_stn
    } else if (input$logger %in% stations) {
      selected <- input$logger
    } else {
      selected <- stations[1]
    }
    updateSelectInput(
      inputId = "logger",
      choices = stations,
      selected = selected
    )
  })
  
  
  ## Map ----
  
  output$mapUI <- renderUI({
    bsCollapse(
      id = "map_collapse",
      open = "map",
      bsCollapsePanel(
        title = "Logger location map",
        value = "map",
        leafletOutput("map"),
        div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
          p(em(HTML(paste0("Currently selected logger is shown in ", colorize("green", "green"), ". Click on any other logger to select it, or choose from the list above.")))),
          p(
            actionButton("zoom_in", "Zoom to selected site"),
            actionButton("reset_zoom", "Zoom out to all sites")
          )
        )
      )
    )
  })
  
  basemaps <- c(
    "Grey Canvas",
    "Open Street Map",
    "ESRI Topo Map"
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lat1 = min(therm_inventory$Latitude),
        lat2 = max(therm_inventory$Latitude),
        lng1 = min(therm_inventory$Longitude),
        lng2 = max(therm_inventory$Longitude)
      ) %>%
      addTiles(group = basemaps[2]) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps[1]) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = basemaps[3]) %>%
      addMapPane("all_points", zIndex = 420) %>%
      addMapPane("cur_point", zIndex = 430) %>%
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
  
  observeEvent(list(logger_list(), input$map_collapse), {
    req(input$year)
    
    df <- therm_inventory %>%
      filter(Year == input$year)
    
    leafletProxy("map") %>%
      clearGroup("all_points") %>%
      addCircleMarkers(
        data = df,
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "<b>Station ID:</b> ", StationID, "<br>",
          "<b>Station Name:</b> ", gsub("\n", "<br>&nbsp;&nbsp;&nbsp;&nbsp;", str_wrap(StationName, width = 40)), "<br>",
          "<b>Years covered:</b> ", YearsActive),
          HTML),
        group = "all_points",
        layerId = ~ StationID,
        options = pathOptions(pane = "all_points"),
        radius = 5,
        weight = 0.5,
        color = "black",
        fill = "green",
        fillColor = "purple",
        fillOpacity = 0.5
      )
  })
  
  # handle displaying selected logger in green
  observeEvent(list(cur_stn(), input$map_collapse), {
    req(cur_stn())
    
    leafletProxy("map") %>%
      clearGroup("cur_point") %>%
      addCircleMarkers(
        data = cur_stn(),
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~lapply(paste0(
          "<b>Station ID:</b> ", StationID, "<br>",
          "<b>Station Name:</b> ", str_trunc(StationName, width = 50), "<br>",
          "<b>Years covered:</b> ", YearsActive),
          HTML),
        layerId = ~StationID,
        group = "cur_point",
        options = pathOptions(pane = "cur_point"),
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
        lat1 = min(therm_inventory$Latitude),
        lat2 = max(therm_inventory$Latitude),
        lng1 = min(therm_inventory$Longitude),
        lng2 = max(therm_inventory$Longitude)
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
    req(input$year)
    req(input$logger)
    
    therm_inventory %>%
      filter(Year == input$year, StationID == input$logger) %>%
      pivot_longer(everything(), values_transform = as.character)
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
        plotlyOutput("plot"),
        div(
          style = "margin: 0.5em 1em; 0.5em 1em;",
          uiOutput("annotationText"),
          p(em("High or widely fluctuating temperatures may indicate that the logger became exposed to the air, either before/after deployment, or when stream levels dropped below the point where the logger was anchored.")),
          hr(),
          p(
            div(strong("Temperature units:"), style = "float: left; margin-right: 1em;"),
            radioButtons(
              inputId = "tempUnits",
              label = NULL,
              inline = T,
              choices = list("Fahrenheit" = "F", "Celsius" = "C")
            ),
            div(strong("Optional plot annotations:"), style = "float: left; margin-right: 1em;"),
            radioButtons(
              inputId = "plotAnnotation",
              label = NULL,
              inline = T,
              choices = list(
                "Brook trout temperature range" = "btrout",
                "Warm/cool/coldwater classification" = "wtemp",
                "None")
            )
          ),
          hr(),
          uiOutput("infoUI")
        )
      ),
      open = "plot"
    )
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
    req(input$plotAnnotation)
    units <- input$tempUnits
    annotation <- input$plotAnnotation
    
    df_daily <- daily()
    df_hourly <- stn_data()
    logger_name <- input$logger
    plot_title <- str_trunc(paste0("Station ", cur_stn()$StationID, ": ", cur_stn()$StationName), width = 80)
    
    req(nrow(df_daily) > 0)
    req(nrow(df_hourly) > 0)
    
    # handle units
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
        title = plot_title,
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
    
    # add annotation color bands
    if (annotation != "None") {
      if (annotation == "btrout") {
        temps <- c(32, 52, 61, 72, 100) # F
        if (units == "C") temps <- f_to_c(temps)
        colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
      } else if (annotation == "wtemp") {
        temps <- c(0, 72, 77, 100) # F
        if (units == "C") temps <- f_to_c(temps)
        colors <- c("blue", "cornflowerblue", "darkorange")
      }
      
      plt <- plt %>%
        layout(
          shapes = lapply(1:length(colors), function(i) {
            rect(temps[i], temps[i + 1], colors[i])
          })
        )
    }
    
    plt
  })
  
  
  # Annotation explanation text for below the plot
  output$annotationText <- renderUI({
    req(input$plotAnnotation)
    req(input$tempUnits)
    annotation <- input$plotAnnotation
    units <- input$tempUnits
    unit_text <- paste0("°", units)
    
    if (annotation == "btrout") {
      temps <- c(52, 61, 72)
      if (units == "C") temps <- f_to_c(temps)
      p(em(HTML(paste0(
        "Optimal brook trout temperatures are shown shaded ", colorize("dark green", "darkgreen"),
        " (", temps[1], "-", temps[2], unit_text, "), acceptable temperatures in ", colorize("light green", "darkseagreen"),
        " (", temps[2], "-", temps[3], unit_text, "), too hot in ", colorize("orange", "orange"),
        " and too cold in ", colorize("blue", "blue"), "."))))
    } else if (annotation == "wtemp") {
      temps <- c(72, 77)
      if (units == "C") temps <- f_to_c(temps)
      p(em(HTML(paste0(
        "The DNR classifies streams as ", colorize("coldwater", "blue"), " when maximum summer temperatures are below ",
        temps[1], unit_text, ", as ", colorize("coolwater", "deepskyblue"), " streams when maximum temperatures are between ",
        temps[1], " and ", temps[2], unit_text, ", and as ", colorize("warmwater", "orange"),
        " streams when maximum temperatures are above ", temps[2], unit_text, "."))))
    }
  })
  
  
  ## Logger/Station lists ----
  
  output$loggerListUI <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Searchable list of loggers",
        div(
          style = "margin: 0.5em 1em; 0.5em 1em; overflow: auto",
          {
            therm_inventory_in %>%
              mutate(across(where(is.numeric), as.character)) %>%
              renderDataTable(
                filter = "top",
                options = list(
                  pageLength = 5,
                  lengthMenu = c(5, 10, 25, 100)
                ))
          }
        )
      )
    )
  })
  
  
  ## Download buttons ----
  
  output$downloadUI <- renderUI({
    req(input$year)
    req(logger_list())
    req(cur_stn())
    
    bsCollapse(
      bsCollapsePanel(
        title = "Download data",
        div(
          style = "margin: 0.5em 1em; 0.5em 1em;",
          h4("Data download options:"),
          tags$ul(
            tags$li(
              paste0("Data for selected site: ", input$year, ", Station #", cur_stn()$StationID, ": ", cur_stn()$StationName),
              br(),
              downloadButton("selectedLoggerDL")),
            tags$li(
              style = "margin-top: 1em;",
              paste0("Data for all sites in ", input$year, ": ", length(logger_list()), " stations, ",
                formatC(nrow(filter(therm_data, Year == input$year)), format = "d", big.mark = ","), " observations"),
              br(),
              downloadButton("allLoggerDL")),
            tags$li(
              style = "margin-top: 1em;",
              paste0("Logger list: ", nrow(therm_inventory_in), " loggers deployed at ", length(unique(therm_inventory_in$StationID)), " stations over ", length(unique(therm_inventory_in$Year)), " years"),
              br(),
              downloadButton("loggerListDL")
            )
          )
        )
      )
    )
  })
  
  output$selectedLoggerDL <- downloadHandler(
    filename = paste0(input$year, " wav thermistor data for station ", cur_stn()$StationID, ".csv"),
    content = function(file) {
      write_csv(stn_data(), file)
    }
  )
  
  output$allLoggerDL <- downloadHandler(
    filename = paste0(input$year, " wav thermistor data.zip"),
    content = function(file) {
      file.copy(zips[[input$year]], file)
    },
    contentType = "application/zip"
  )
  
  output$loggerListDL <- downloadHandler(
    filename = "wav thermistor list.csv",
    content = function(file) {
      write_csv(therm_inventory_in, file)
    }
  )
  
  
  
}

shinyApp(ui, server)

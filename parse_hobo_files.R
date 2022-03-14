library(tidyverse)
library(plotly)


# Helper functions --------------------------------------------------------

c_to_f <- function(c) {
  c * 9.0 / 5 + 32
}

f_to_c <- function(f) {
  (f - 32) * 5.0 / 9 
}

# temp_convert <- function(temp, unit) {
#   ifelse(
#     unit == "F",
#     
#   )
# }

temp_check <- function(temp, unit) {
  ifelse(
    unit == "F",
    (temp > 32) & (temp < 100),
    (temp > 0) & (temp < 40)
  )
}

# c_to_f(0)
# c_to_f(10)
# f_to_c(32)
# f_to_c(212)

# test --------------------------------------------------------------------

import <- read_csv("hobo_data/20812288.csv") %>%
  select(2:3)

names(import)

grepl("°F", names(import)[2])

max(import[,2], na.rm = T)

temp_check(import[2], "F")

grepl("°C", names(import)[2])

  select(DateTime = 2, Temp_F = 3) %>%
  mutate(DateTime = lubridate::parse_date_time(DateTime, c("mdy HMS p", "mdy HMS")))

  mutate(DateTime = ifelse(
    grepl("[AP]M", DateTime),
    lubridate::parse_date_time(DateTime, "mdy HMS p"),
    lubridate::parse_date_time(DateTime, "mdy HMS"))) %>%
  mutate(DateTime)


temp_check(110, "F")
  

# Load and prepare thermistor data ----------------------------------------

# load logger data
hobo_path <- "hobo_data"
files <- list.files(hobo_path, "*.csv")

# file <- "10706439.csv"

raw_data <- lapply(files, function(file) {
  fname <- gsub(".csv", "", file)
  tryCatch({
    import <- suppressMessages(read_csv(file.path(hobo_path, file), col_types = cols(), skip = 1))
    message(paste0("\nSN: ", fname))
    unit <- ifelse(grepl("°F", names(import)[3]), "F", "C")
    data <- import %>%
      select(DateTime = 2, Temp = 3) %>%
      mutate(Temp = round(Temp, 2)) %>%
      mutate(Unit = unit) %>%
      drop_na() %>%
      mutate(LoggerSN = as.numeric(fname), .before = 1) %>%
      mutate(DateTime = lubridate::parse_date_time(DateTime, c("mdy HMS p", "mdy HMS"))) %>%
      mutate(Date = as.Date(DateTime), .before = DateTime) %>%
      mutate(TempOK = temp_check(Temp, Unit))
    cat(paste0(
      " => ", nrow(data), " obs\n",
      " => ", as.Date(min(data$Date)), " - ", as.Date(max(data$Date)), "\n",
      " => ", min(data$Temp), " - ", max(data$Temp), " °", unit, "\n"))
    if (lubridate::year(min(data$Date)) != lubridate::year(max(data$Date))) {
      message(" WARNING: Multiple years in data range!")
    }
    if (!all(data$TempOK)) {
      message(" WARNING: Temperature value(s) out of range!")
      data <- data %>% filter(TempOK)
    }
    data %>%
      select(-TempOK) %>%
      mutate(
        Temp_F = ifelse(Unit == "F", Temp, round(c_to_f(Temp), 2)),
        Temp_C = ifelse(Unit == "C", Temp, round(f_to_c(Temp), 2))
      )
  },
    error = function(e) { message(fname, " => FAIL\n") })
}) %>% bind_rows()

# get supplemental information
therm_locs <- read_csv("thermistor-locations.csv")
therm_inventory <- read_csv("thermistor-inventory-2021.csv")

# are we missing anything?
setdiff(raw_data$LoggerSN, therm_inventory$`2021 Therm SN`) # should be zero
# union(raw_data$LoggerSN, therm_inventory$`2021 Therm SN`)

# add info to raw data
therm_data <- raw_data %>%
  left_join({
    therm_locs %>%
      left_join(
        select(therm_inventory, c(LoggerSN = "2021 Therm SN", StationID = "Station ID")))
  })

# save joined data
write_csv(therm_data, "2021-thermistor-data.csv.gz")



# per-station data/display ------------------------------------------------

# pick a station
# logger <- therm_data$LoggerSN[1]
# logger <- "20812288"
logger <- sample(therm_data$LoggerSN, 1)

# select station data
stn_data <- dplyr::filter(therm_data, LoggerSN == logger) %>%
  arrange(DateTime)

# get matching station inventory
stn_info <- therm_inventory %>%
  dplyr::filter(`2021 Therm SN` == logger) %>%
  pivot_longer(everything(), values_transform = as.character) %>%
  arrange(name)

# create station daily totals
daily <- stn_data %>%
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

# create station summary for display
stn_summary <- daily %>%
  summarise(
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

# plot hourly and daily temp data
plot_ly() %>%
  add_ribbons(
    data = daily,
    x = ~ DateTime,
    ymin = ~ Min,
    ymax = ~ Max,
    line = list(
      color = "lightblue",
      opacity = 0),
    fillcolor = "lightblue",
    opacity = 0.5,
    connectgaps = T,
    name = "Daily Range"
  ) %>%
  add_trace(
    data = stn_data,
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
    data = daily,
    x = ~ DateTime,
    y = ~ Mean,
    name = "Mean Daily Temp. (F)",
    type = "scatter",
    mode = "lines",
    line = list(
      color = "orange"
    )) %>% 
  layout(
    title = paste("Temperature at", logger),
    showlegend = TRUE,
    xaxis = list(title = "Date and Time"),
    yaxis = list(title = "Temperature (F)"),
    hovermode = "x unified")

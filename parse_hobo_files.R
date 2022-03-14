library(tidyverse)
library(plotly)



# test --------------------------------------------------------------------

# read_csv("hobo_data/20812288.csv") %>%
#   select(DateTime = 2, Temp_F = 3) %>%
#   mutate(DateTime = lubridate::parse_date_time(DateTime, c("mdy HMS p", "mdy HMS")))
#   
#   mutate(DateTime = ifelse(
#     grepl("[AP]M", DateTime),
#     lubridate::parse_date_time(DateTime, "mdy HMS p"),
#     lubridate::parse_date_time(DateTime, "mdy HMS"))) %>%
#   mutate(DateTime)



# Load and prepare thermistor data ----------------------------------------

# load logger data
therm_path <- "hobo_data"
files <- list.files(therm_path, "*.csv")

raw_data <- lapply(files, function(file) {
  fname = tools::file_path_sans_ext(file)
  tryCatch({
    import <- suppressMessages(read_csv(file.path(hobo_path, file), col_types = cols(), skip = 1)) %>%
      select(DateTime = 2, Temp_F = 3) %>%
      drop_na() %>%
      mutate(LoggerSN = as.numeric(fname), .before = 1) %>%
      mutate(DateTime = lubridate::parse_date_time(DateTime, c("mdy HMS p", "mdy HMS"))) %>%
      mutate(Date = as.Date(DateTime), .before = DateTime)
    message(fname, " => ", nrow(import), " obs, ", as.Date(min(import$Date)), " - ", as.Date(max(import$Date)))
    import
  },
    error = function(e) {
      message(fname, " => FAIL")
    })
}) %>%
  bind_rows()

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

# first station
logger <- therm_data$LoggerSN[1]
logger <- "20812288"
stn_data <- dplyr::filter(therm_data, LoggerSN == logger) %>%
  arrange(DateTime)

stn_info <- therm_inventory %>%
  dplyr::filter(`2021 Therm SN` == logger) %>%
  pivot_longer(everything(), values_transform = as.character) %>%
  arrange(name)

daily <- stn_data %>%
  group_by(Date) %>%
  summarise(
    n = n(),
    min = min(Temp_F),
    max = max(Temp_F),
    mean = mean(Temp_F)
  ) %>%
  dplyr::filter(n == 24) %>%
  mutate(DateTime = as.POSIXct(paste(Date, "12:00:00")))

stn_summary <- daily %>%
  summarise(
    `Date Deployed` = as.Date(min(Date)),
    `Date Retrieved` = as.Date(max(Date)),
    `Number of Days` = n(),
    `Number of Readings` = sum(n),
    `Max Temperature` = max(max),
    `Min Temperature` = min(min),
    `Max Daily Average` = max(mean),
    `Min Daily Average` = min(mean),
    `Average Temperature` = mean(mean)
  ) %>%
  mutate(
    across(`Max Temperature`:`Average Temperature`, round, 2),
    across(`Max Temperature`:`Average Temperature`, paste, "°F")) %>%
  pivot_longer(everything(), values_transform = as.character)


plot_ly() %>%
  add_trace(
    data = stn_data,
    x = ~DateTime,
    y = ~Temp_F,
    name = "Hourly Temperature (F)",
    type = "scatter",
    mode = "lines",
    line = list(
      width = 1
    )) %>%
  add_trace(
    data = daily,
    x = ~DateTime,
    y = ~mean,
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
    yaxis = list(title = "Temperature (F)"))

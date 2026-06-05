
# Install packages
{
  install.packages("dataRetrieval")
  install.packages("dplyr")
  install.packages('sf')
  install.packages("zoo")
  install.packages("lubridate")
}

# Load libraries
{
  library(dataRetrieval) # read data from USGS
  library(dplyr)         # data manipulation
  library(sf)            # for handling sf objects (used by read_waterdata_daily)
  library(zoo)           # for interpolation
  library(lubridate)
}

# Read data ---------------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

ace <- ace.raw %>%
  filter(location != 0)

ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Define USGS site and parameter ------------------------------------------
site.ihsc <- "04092750"
paramflow <- "00060"   # Discharge (cfs)

# Fetch daily water flow -------------------------------------------
flow <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site.ihsc, sep = "-"),
  parameter_code = paramflow,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(ace$date)), as.character(max(ace$date))))

# Clean USGS data
flow_values <- flow %>%
  st_drop_geometry() %>%
  select(time, value) %>%
  group_by(time) %>%
  summarize(
    flow_cfs = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

all_days <- tibble(
  time = seq(
    from = min(flow_values$time),
    to   = max(flow_values$time),
    by   = "day"
  )
)

flow_complete <- all_days %>%
  left_join(
    flow_values,
    by = "time"
  ) %>%
  arrange(time) %>%
  mutate(
    flow_cfs = zoo::na.approx(
      flow_cfs,
      x = time,
      na.rm = FALSE
    )
  ) %>%
  mutate(
    flow_cfs = zoo::na.locf(
      flow_cfs,
      na.rm = FALSE
    )
  ) %>%
  mutate(
    flow_cfs = zoo::na.locf(
      flow_cfs,
      fromLast = TRUE))

ace <- ace %>%
  left_join(flow_complete, by = c("date" = "time"))

ace <- ace %>%
  mutate(log_flow = log10(flow_cfs))




# save
write.csv(fx, "Data/FoxRiver/FoxRiver_temp.csv", row.names = FALSE)

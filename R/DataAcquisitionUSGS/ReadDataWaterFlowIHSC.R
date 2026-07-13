
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
ace_dates <- read.csv("Data/RemediationActivities/all_activity_daily.csv")
ace_dates$date <- as.Date(ace_dates$date, origin = "1899-12-30")

# Define USGS site and parameter ------------------------------------------
site.ihsc <- "04092750"
paramflow <- "00060"   # Discharge (cfs)

# Fetch daily water flow -------------------------------------------
flow <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site.ihsc, sep = "-"),
  parameter_code = paramflow,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(ace_dates$date)), as.character(max(ace_dates$date))))

# Clean USGS data
flow_values <- flow %>%
  st_drop_geometry() %>%
  select(time, flow_cfs = value)

flow_ihsc <- ace_dates %>%
  left_join(
    flow_values,
    by = c("date" = "time")
  ) %>%
  transmute(
    date,
    flow_cfs,
    flow_abs = abs(flow_cfs),   # Absolute discharge (reverse flows are negative)
    log_flow = log10(abs(flow_cfs))
  )

# Save
write.csv(flow_ihsc, "Data/USGS/flow_ihsc.csv", row.names = FALSE)

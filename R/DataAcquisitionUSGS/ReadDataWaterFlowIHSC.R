
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
  select(time, flow_cfs = value)

flow_ihsc <- ace %>%
  select(date) %>%
  left_join(
    flow_values,
    by = c("date" = "time")
  ) %>%
  transmute(
    date,
    flow_cfs,
    flow_abs = abs(flow_cfs), # Negative values meaning contrary flow direction
    log_flow = log10(abs(flow_cfs))
  )

# Save
write.csv(flow_ihsc, "Data/USGS/flow_ihsc.csv", row.names = FALSE)

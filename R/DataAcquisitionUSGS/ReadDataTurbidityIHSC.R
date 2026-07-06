
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
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))
# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")
# Get unique date values
ace_dates <- ace[!duplicated(ace$date), "date", drop = FALSE]

# Define USGS site and parameter ------------------------------------------
site.ihsc <- "04092750"
paramturb <- "63680"   # Turbidity. FNU (Formazin Nephelometric Units)

# Fetch daily water flow -------------------------------------------
turb <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site.ihsc, sep = "-"),
  parameter_code = paramturb,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(ace$date)), as.character(max(ace$date))))

turb_values <- turb %>%
  st_drop_geometry() %>%
  select(time, turb_FNU = value)

turb_ihsc <- ace_dates %>%
  select(date) %>%
  left_join(
    turb_values,
    by = c("date" = "time")
  ) %>%
  transmute(
    date,
    turb_FNU)

# Save
write.csv(turb_ihsc, "Data/USGS/turb_ihsc.csv", row.names = FALSE)

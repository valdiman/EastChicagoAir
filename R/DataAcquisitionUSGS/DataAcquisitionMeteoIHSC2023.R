
# Install packages
{
  install.packages("dataRetrieval")
  install.packages("dplyr")
  install.packages('sf')
  install.packages("lubridate")
}

# Load libraries
{
  library(dataRetrieval) # read data from USGS
  library(dplyr)         # data manipulation
  library(sf)            # for handling sf objects (used by read_waterdata_daily)
  library(lubridate)
}

# Read data ---------------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))
# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Define USGS site and parameter ------------------------------------------
# Met Station ON Cdf at East Chicago, IN
# Air temperature
air_temp <- readNWISuv(
  siteNumbers = "413853087290401",
  parameterCd = "00020",
  startDate = "2023-01-01",
  endDate = "2023-12-31")

air_temp_daily <- air_temp %>%
  mutate(
    date = as.Date(dateTime)
  ) %>%
  group_by(date) %>%
  summarize(
    air_temp_C = mean(X_00020_00000, na.rm = TRUE),
    .groups = "drop")

# Save
write.csv(air_temp_daily, "Data/USGS/airtemp_ihsc2023.csv", row.names = FALSE)

# Wind speed
wind_speed <- readNWISuv(
  siteNumbers = "413853087290401",
  parameterCd = "62625",
  startDate = "2023-01-01",
  endDate = "2023-12-31")

wind_speed_daily <- wind_speed %>%
  mutate(
    date = as.Date(dateTime)
  ) %>%
  group_by(date) %>%
  summarize(
    wind_speed_ms = mean(X_62625_00000, na.rm = TRUE),
    .groups = "drop")

# Save
write.csv(wind_speed_daily, "Data/USGS/windspeed_ihsc2023.csv",
          row.names = FALSE)

# Wind direction
wind_dir <- readNWISuv(
  siteNumbers = "413853087290401",
  parameterCd = "00036",
  startDate = "2023-01-01",
  endDate = "2023-12-31")

# Circular mean for wind direction
circ_mean_deg <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  (atan2(mean(sin(x * pi / 180)), mean(cos(x * pi / 180))) * 180 / pi) %% 360
}

wind_dir_daily <- wind_dir %>%
  mutate(
    date = as.Date(dateTime)
  ) %>%
  group_by(date) %>%
  summarize(
    wind_dir_deg =
      circ_mean_deg(X_00036_00000),
    .groups = "drop")

# Save
write.csv(wind_dir_daily, "Data/USGS/winddir_ihsc2023.csv",
          row.names = FALSE)


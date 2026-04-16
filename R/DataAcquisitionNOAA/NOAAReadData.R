# Code to read NOAA meteorological data
# https://docs.ropensci.org/rnoaa/articles/rnoaa.html
# Documentation:
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("rnoaa") # For future use, noaaweather package will need to be installed it.
install.packages("leaflet")
install.packages("dplyr")
install.packages("stringr")
install.packages("geosphere")

# Libraries
{
  library(rnoaa)
  library(geosphere)
  library(dplyr)
  library(lubridate)
  library(zoo)
}

# The rnoaa package will soon be retired and archived because the underlying
# APIs have changed dramatically. The package currently works but does not pull
# the most recent data in all cases. A noaaWeather package is planned as a
# replacement but the functions will not be interchangeable.

# IHSC coordinates
site_lat <- 41.646490
site_lon <- -87.473562

# Load NOAA ISD station list
stations <- isd_stations()

# Find nearby stations (within 50 km)
stations_nearby <- stations %>%
  mutate(distance_km = distHaversine(cbind(site_lon, site_lat), cbind(lon, lat)) / 1000) %>%
  filter(distance_km <= 50)

# Select the nearest major station (1: CHICAGO MIDWAY INTL ARPT; 2: GARY/CHICAGO AIRPORT)
station.1 <- stations[stations$usaf == "725340", ] # 2001:2005
station.2 <- stations[stations$usaf == "725337", ] # 2006:2023

# Read ACE Data
ace <- read.csv("Data/Air/ACEData.csv")
# Remove blanks cells
ace.1 <- subset(ace, !grepl("0", location))
ace.1$date <- as.Date(ace.1$date, origin = "1899-12-30")

# Helper function to download ISD data for a range of years
download_isd_years <- function(usaf, wban, years) {
  weather_list <- list()
  
  for (y in years) {
    cat("Downloading year:", y, "\n")
    dat <- isd(usaf = usaf, wban = wban, year = y)
    weather_list[[as.character(y)]] <- dat
  }
  
  bind_rows(weather_list)
}

# Years by station
years_1 <- 2001:2005
years_2 <- 2006:2023

# Download data from each station
weather_1 <- download_isd_years(
  usaf = as.character(station.1$usaf[1]),
  wban = as.character(station.1$wban[1]),
  years = years_1
)

weather_2 <- download_isd_years(
  usaf = as.character(station.2$usaf[1]),
  wban = as.character(station.2$wban[1]),
  years = years_2
)

# Combine both station periods
weather_all <- bind_rows(weather_1, weather_2)

# Dates where station 1 air temperature should be used
target_dates <- as.Date(c(
  "2014-11-01",
  "2015-07-05",
  "2015-07-11",
  "2015-08-22",
  "2023-05-06"
))

target_years <- sort(unique(as.integer(format(target_dates, "%Y"))))

# Download station 1 only for the years needed to fill those dates
weather_1_extra <- download_isd_years(
  usaf = as.character(station.1$usaf[1]),
  wban = as.character(station.1$wban[1]),
  years = target_years
)

# Circular mean for wind direction
circ_mean_deg <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  (atan2(mean(sin(x * pi / 180)), mean(cos(x * pi / 180))) * 180 / pi) %% 360
}

# Daily values from station 2 (main dataset)
weather_daily <- weather_all %>%
  mutate(
    date = as.Date(date, format = "%Y%m%d"),
    air_temp = as.numeric(temperature),
    wind_speed = as.numeric(wind_speed),
    wind_direction = as.numeric(wind_direction),
    air_pressure = as.numeric(air_pressure),
    
    air_temp = ifelse(air_temp %in% c(9999, 99999), NA, air_temp / 10),
    wind_speed = ifelse(wind_speed %in% c(9999, 99999), NA, wind_speed / 10),
    wind_direction = ifelse(wind_direction %in% c(999, 9999, 99999), NA, wind_direction),
    air_pressure = ifelse(air_pressure %in% c(99999, 999999), NA, air_pressure / 10)
  ) %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarise(
    air_temp = mean(air_temp, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    wind_direction = circ_mean_deg(wind_direction),
    air_pressure = mean(air_pressure, na.rm = TRUE),
    .groups = "drop"
  )

# Daily air temperature from station 1 for target dates only
weather_1_daily <- weather_1_extra %>%
  mutate(
    date = as.Date(date, format = "%Y%m%d"),
    air_temp = as.numeric(temperature),
    air_temp = ifelse(air_temp %in% c(9999, 99999), NA, air_temp / 10)
  ) %>%
  filter(date %in% target_dates) %>%
  group_by(date) %>%
  summarise(air_temp_station1 = mean(air_temp, na.rm = TRUE), .groups = "drop")

# Replace air temperature on those dates with station 1 values
weather_daily <- weather_daily %>%
  left_join(weather_1_daily, by = "date") %>%
  mutate(
    air_temp = if_else(date %in% target_dates, air_temp_station1, air_temp)
  ) %>%
  select(-air_temp_station1)

ace.1 <- ace.1 %>%
  left_join(weather_daily, by = "date")

# Check values
summary(ace.1$air_temp)
summary(ace.1$wind_speed)
summary(ace.1$air_pressure)
summary(ace.1$wind_direction)

# Save
write.csv(ace.1, "Data/Meteorology/ace.1IHSC_Meteo.csv", row.names = FALSE)


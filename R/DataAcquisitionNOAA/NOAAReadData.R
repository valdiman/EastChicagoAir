# Code to read NOAA meteorological data
# https://docs.ropensci.org/rnoaa/articles/rnoaa.html
# Documentation:
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf

# Packages and libraries needed --------------------------------------------
# Install packages
{
  install.packages("rnoaa") # For future use, noaaweather package will need to be installed it.
  install.packages("leaflet")
  install.packages("dplyr")
  install.packages("stringr")
  install.packages("geosphere")
}

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

# Site coordinates
site_lat <- 41.646490
site_lon <- -87.473562

# NOAA stations
stations <- isd_stations()

stations_nearby <- stations %>%
  mutate(
    distance_km = distHaversine(
      cbind(site_lon, site_lat),
      cbind(lon, lat)
    ) / 1000
  ) %>%
  filter(distance_km <= 50)

# Midway (used for 2001-2005 and gap filling)
station1 <- stations %>% filter(usaf == "725340")

# Gary/Chicago (main station 2006-2024)
station2 <- stations %>% filter(usaf == "725337")

# Activity dates
all_dates <- read.csv("Data/RemediationActivities/all_activity_daily.csv")
all_dates$date <- as.Date(all_dates$date, origin = "1899-12-30")

# Helper function
download_isd_years <- function(usaf, wban, years){
  out <- vector("list", length(years))
  for(i in seq_along(years)){
    cat("Downloading", years[i], "\n")
    out[[i]] <- isd(
      usaf = as.character(usaf),
      wban = as.character(wban),
      year = years[i]
    )
  }
  bind_rows(out)
}

# Download data
weather1 <- download_isd_years(
  usaf = station1$usaf[1],
  wban = station1$wban[1],
  years = 2001:2005
)

weather2 <- download_isd_years(
  usaf = station2$usaf[1],
  wban = station2$wban[1],
  years = 2006:2024
)

weather_all <- bind_rows(weather1, weather2)

# Circular mean
circ_mean_deg <- function(x){
  x <- x[!is.na(x)]
  if(length(x)==0) return(NA_real_)
  (
    atan2(
      mean(sin(x*pi/180)),
      mean(cos(x*pi/180))
    )*180/pi
  ) %% 360
}

# Daily weather
weather_daily <- weather_all %>%
  mutate(
    date = as.Date(date,"%Y%m%d"),
    air_temp = as.numeric(temperature),
    wind_speed = as.numeric(wind_speed),
    wind_direction = as.numeric(wind_direction),
    air_pressure = as.numeric(air_pressure),
    air_temp =
      ifelse(air_temp==9999,NA,air_temp/10),
    wind_speed =
      ifelse(wind_speed==9999,NA,wind_speed/10),
    wind_direction =
      ifelse(wind_direction %in% c(999,9999,99999),
             NA,
             wind_direction),
    air_pressure =
      ifelse(air_pressure==99999,
             NA,
             air_pressure/10)
  ) %>%
  group_by(date) %>%
  summarise(
    air_temp =
      if(all(is.na(air_temp))) NA_real_
    else mean(air_temp,na.rm=TRUE),
    wind_speed =
      if(all(is.na(wind_speed))) NA_real_
    else mean(wind_speed,na.rm=TRUE),
    wind_direction =
      circ_mean_deg(wind_direction),
    air_pressure =
      if(all(is.na(air_pressure))) NA_real_
    else mean(air_pressure,na.rm=TRUE),
    .groups="drop"
  )

# Find missing air temperatures
missing_dates <- weather_daily %>%
  filter(is.na(air_temp)) %>%
  pull(date)

missing_years <- sort(unique(as.integer(format(missing_dates,"%Y"))))

# Download Midway for missing years
weather1_extra <- download_isd_years(
  station1$usaf,
  station1$wban,
  missing_years
)

# Daily Midway temperatures
weather1_fill <- weather1_extra %>%
  mutate(
    date = as.Date(date,"%Y%m%d"),
    air_temp = as.numeric(temperature),
    air_temp =
      ifelse(air_temp==9999,
             NA,
             air_temp/10)
  ) %>%
  filter(date %in% missing_dates) %>%
  group_by(date) %>%
  summarise(
    air_temp_station1 =
      if(all(is.na(air_temp))) NA_real_
    else mean(air_temp,na.rm=TRUE),
    .groups="drop"
  )

# Replace missing temperatures
weather_daily <- weather_daily %>%
  left_join(weather1_fill, by="date") %>%
  mutate(
    air_temp =
      coalesce(
        air_temp,
        air_temp_station1
      )
  ) %>%
  select(-air_temp_station1)

# Final dataset
weather_daily <- all_dates %>%
  distinct(date) %>%
  arrange(date) %>%
  left_join(weather_daily, by = "date")

# Check values
summary(weather_daily$air_temp)
summary(weather_daily$wind_speed)
summary(weather_daily$air_pressure)
summary(weather_daily$wind_direction)

# Save
write.csv(weather_daily, "Data/Meteorology/MeteoEC.csv",
          row.names = FALSE)

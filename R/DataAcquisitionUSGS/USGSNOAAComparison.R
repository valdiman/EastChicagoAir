# Compare IHSC meteorological station with
# Chicago Midway Airport and Gary/Chicago Airport

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("rnoaa")
  install.packages("ggplot2")
}

# Library
{
  library(dplyr)
  library(rnoaa)
  library(ggplot2)
}

# Read IHSC daily data ----------------------------------------------------
airtemp_ihsc <- read.csv("Data/USGS/airtemp_ihsc.csv")
windspeed_ihsc <- read.csv("Data/USGS/windspeed_ihsc.csv")
winddir_ihsc <- read.csv("Data/USGS/winddir_ihsc.csv")

airtemp_ihsc$date <- as.Date(airtemp_ihsc$date)
windspeed_ihsc$date <- as.Date(windspeed_ihsc$date)
winddir_ihsc$date <- as.Date(winddir_ihsc$date)

# Helper functions --------------------------------------------------------
circ_mean_deg <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_real_)
  
  (atan2(
    mean(sin(x*pi/180)),
    mean(cos(x*pi/180))
  ) * 180/pi) %% 360
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_real_)
  mean(x)
}

rmse <- function(x, y) {
  sqrt(mean((x-y)^2, na.rm = TRUE))
}

mae <- function(x, y) {
  mean(abs(x-y), na.rm = TRUE)
}

# NOAA stations -----------------------------------------------------------
stations <- isd_stations()

# Chicago Midway
station.1 <- stations %>%
  filter(usaf == "725340") %>%
  slice(1)

# Gary/Chicago Airport
station.2 <- stations %>%
  filter(usaf == "725337") %>%
  slice(1)

station.1_usaf <- as.character(station.1$usaf)
station.1_wban <- as.character(station.1$wban)

station.2_usaf <- as.character(station.2$usaf)
station.2_wban <- as.character(station.2$wban)

# Download 2023 data ------------------------------------------------------
weather1 <- isd(
  usaf = station.1_usaf,
  wban = station.1_wban,
  year = 2023)

weather2 <- isd(
  usaf = station.2_usaf,
  wban = station.2_wban,
  year = 2023)

# Convert raw ISD to daily values -----------------------------------------
prep_daily <- function(df) {
  
  df %>%
    mutate(
      date = as.Date(date, format = "%Y%m%d"),
      
      air_temp = as.numeric(temperature),
      wind_speed = as.numeric(wind_speed),
      wind_direction = as.numeric(wind_direction),
      air_pressure = as.numeric(air_pressure),
      
      air_temp =
        ifelse(
          air_temp %in% c(9999,99999),
          NA,
          air_temp/10),
      
      wind_speed =
        ifelse(
          wind_speed %in% c(9999,99999),
          NA,
          wind_speed/10),
      
      wind_direction =
        ifelse(
          wind_direction %in% c(999,9999,99999),
          NA,
          wind_direction),
      
      air_pressure =
        ifelse(
          air_pressure %in% c(99999,999999),
          NA,
          air_pressure/10)
    ) %>%
    group_by(date) %>%
    summarise(
      air_temp = safe_mean(air_temp),
      wind_speed = safe_mean(wind_speed),
      wind_direction = circ_mean_deg(wind_direction),
      air_pressure = safe_mean(air_pressure),
      .groups = "drop")
}

daily_airport1 <- prep_daily(weather1)
daily_airport2 <- prep_daily(weather2)

# Save for Mike
write.csv(daily_airport1, "Data/Mike/MeteorologyChicagoMidway2023.csv",
          row.names = FALSE)
write.csv(daily_airport2, "Data/Mike/MeteorologyGaryAirport2023.csv",
          row.names = FALSE)

# Comparison datasets -----------------------------------------------------
build_comparison <- function(airport_daily,
                             airtemp_ihsc,
                             windspeed_ihsc,
                             winddir_ihsc) {
  
  temp_compare <- airport_daily %>%
    select(
      date,
      air_temp_airport = air_temp
    ) %>%
    inner_join(
      airtemp_ihsc,
      by = "date")
  
  windspeed_compare <- airport_daily %>%
    select(
      date,
      wind_speed_airport = wind_speed
    ) %>%
    inner_join(
      windspeed_ihsc,
      by = "date")
  
  winddir_compare <- airport_daily %>%
    select(
      date,
      wind_direction_airport = wind_direction
    ) %>%
    inner_join(
      winddir_ihsc,
      by = "date"
    ) %>%
    mutate(
      wd_diff =
        ((wind_dir_deg -
            wind_direction_airport +
            180) %% 360) - 180,
      
      wind_dir_wrapped =
        wind_direction_airport + wd_diff)
  
  list(
    temp = temp_compare,
    windspeed = windspeed_compare,
    winddir = winddir_compare)
}

midway <- build_comparison(
  daily_airport1,
  airtemp_ihsc,
  windspeed_ihsc,
  winddir_ihsc)

gary <- build_comparison(
  daily_airport2,
  airtemp_ihsc,
  windspeed_ihsc,
  winddir_ihsc)

# Summary statistics ------------------------------------------------------
summarize_linear <- function(obs, ref) {
  
  data.frame(
    Correlation = cor(obs, ref, use = "complete.obs"),
    Bias = mean(obs - ref, na.rm = TRUE),
    MAE = mae(obs, ref),
    RMSE = rmse(obs, ref))
}

summarize_direction <- function(wd_diff) {
  
  data.frame(
    Bias = mean(wd_diff, na.rm = TRUE),
    SD = sd(wd_diff, na.rm = TRUE),
    MAE = mean(abs(wd_diff), na.rm = TRUE),
    RMSE = sqrt(mean(wd_diff^2, na.rm = TRUE)))
}

# Midway results ----------------------------------------------------------
temp_midway <- summarize_linear(
  midway$temp$air_temp_C,
  midway$temp$air_temp_airport)

windspeed_midway <- summarize_linear(
  midway$windspeed$wind_speed_ms,
  midway$windspeed$wind_speed_airport)

winddir_midway <- summarize_direction(
  midway$winddir$wd_diff)

# Gary results ------------------------------------------------------------
temp_gary <- summarize_linear(
  gary$temp$air_temp_C,
  gary$temp$air_temp_airport)

windspeed_gary <- summarize_linear(
  gary$windspeed$wind_speed_ms,
  gary$windspeed$wind_speed_airport)

winddir_gary <- summarize_direction(
  gary$winddir$wd_diff)

temp_midway
temp_gary

windspeed_midway
windspeed_gary

winddir_midway
winddir_gary

# 1:1 plots ---------------------------------------------------------------
temp_plot_df <- bind_rows(
  
  midway$temp %>%
    transmute(
      airport = "Midway",
      x = air_temp_airport,
      y = air_temp_C),
  
  gary$temp %>%
    transmute(
      airport = "Gary",
      x = air_temp_airport,
      y = air_temp_C))

ggplot(
  temp_plot_df,
  aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed") +
  coord_equal() +
  facet_wrap(~ airport) +
  labs(
    x = "Airport Temperature (°C)",
    y = "IHSC Temperature (°C)",
    title = "Temperature Comparison") +
  theme_minimal()

windspeed_plot_df <- bind_rows(
  
  midway$windspeed %>%
    transmute(
      airport = "Midway",
      x = wind_speed_airport,
      y = wind_speed_ms),
  
  gary$windspeed %>%
    transmute(
      airport = "Gary",
      x = wind_speed_airport,
      y = wind_speed_ms))

ggplot(
  windspeed_plot_df,
  aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed") +
  coord_equal() +
  facet_wrap(~ airport) +
  labs(
    x = "Airport Wind Speed (m/s)",
    y = "IHSC Wind Speed (m/s)",
    title = "Wind Speed Comparison") +
  theme_minimal()

winddir_plot_df <- bind_rows(
  
  midway$winddir %>%
    transmute(
      airport = "Midway",
      x = wind_direction_airport,
      y = wind_dir_wrapped),
  
  gary$winddir %>%
    transmute(
      airport = "Gary",
      x = wind_direction_airport,
      y = wind_dir_wrapped))

ggplot(
  winddir_plot_df,
  aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed") +
  coord_equal() +
  facet_wrap(~ airport) +
  labs(
    x = "Airport Wind Direction (°)",
    y = "IHSC Wind Direction",
    title = "Wind Direction Comparison") +
  theme_minimal()



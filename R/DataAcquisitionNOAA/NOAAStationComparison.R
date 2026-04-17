# ============================================================
# NOAA ISD: download 2 stations, compare 2010-2020, and fill
# missing dates from station 2 with station 1
# ============================================================

# ------------------------------------------------------------
# 1) Packages & libraries
# ------------------------------------------------------------
{
  install.packages("rnoaa") # For future use, noaaweather package will need to be installed it.
  install.packages("dplyr")
  install.packages("geosphere")
  install.packages("ggplot2")
  install.packages("patchwork")
}

# Libraries
{
  library(dplyr)
  library(geosphere)
  library(rnoaa)
  library(ggplot2)
  library(patchwork)
}

# ------------------------------------------------------------
# 2) Site coordinates
# ------------------------------------------------------------
site_lat <- 41.646490
site_lon <- -87.473562

# ------------------------------------------------------------
# 3) Load NOAA ISD station list and inspect nearby stations
# ------------------------------------------------------------
stations <- isd_stations()

stations_nearby <- stations %>%
  mutate(distance_km = distHaversine(cbind(site_lon, site_lat), cbind(lon, lat)) / 1000) %>%
  filter(distance_km <= 50) %>%
  arrange(distance_km)

print(stations_nearby)

# ------------------------------------------------------------
# 4) Select the two stations you want to compare
#    Station 1 = backup / earlier station
#    Station 2 = main station / later station
# ------------------------------------------------------------
station.1 <- stations %>%
  filter(usaf == "725340") %>%
  slice(1)

station.2 <- stations %>%
  filter(usaf == "725337") %>%
  slice(1)

stopifnot(nrow(station.1) == 1, nrow(station.2) == 1)

# Make sure WBAN values are usable
station.1_usaf <- as.character(station.1$usaf)
station.1_wban <- as.character(station.1$wban)

station.2_usaf <- as.character(station.2$usaf)
station.2_wban <- as.character(station.2$wban)

# ------------------------------------------------------------
# 5) Helper function to download ISD data for a range of years
# ------------------------------------------------------------
download_isd_years <- function(usaf, wban, years) {
  weather_list <- list()
  
  for (y in years) {
    cat("Downloading year:", y, "\n")
    
    dat <- tryCatch(
      isd(usaf = usaf, wban = wban, year = y),
      error = function(e) {
        message("  Failed for year ", y, ": ", e$message)
        return(NULL)
      }
    )
    
    weather_list[[as.character(y)]] <- dat
  }
  
  bind_rows(weather_list)
}

# ------------------------------------------------------------
# 6) Download data for both stations, 2010-2020
# ------------------------------------------------------------
years <- 2010:2020

weather1 <- download_isd_years(
  usaf = station.1_usaf,
  wban = station.1_wban,
  years = years
)

weather2 <- download_isd_years(
  usaf = station.2_usaf,
  wban = station.2_wban,
  years = years
)

# ------------------------------------------------------------
# 7) Helper functions for daily aggregation
# ------------------------------------------------------------
circ_mean_deg <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  (atan2(mean(sin(x * pi / 180)), mean(cos(x * pi / 180))) * 180 / pi) %% 360
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

# ------------------------------------------------------------
# 8) Clean and summarize a raw ISD station dataset to daily values
# ------------------------------------------------------------
prep_daily <- function(df) {
  df %>%
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
      air_temp = safe_mean(air_temp),
      wind_speed = safe_mean(wind_speed),
      wind_direction = circ_mean_deg(wind_direction),
      air_pressure = safe_mean(air_pressure),
      .groups = "drop"
    )
}

# ------------------------------------------------------------
# 9) Build daily data for each station
# ------------------------------------------------------------
daily1 <- prep_daily(weather1) %>%
  filter(date >= as.Date("2010-01-01"), date <= as.Date("2020-12-31")) %>%
  rename_with(~ paste0(.x, "_s1"), -date)

daily2 <- prep_daily(weather2) %>%
  filter(date >= as.Date("2010-01-01"), date <= as.Date("2020-12-31")) %>%
  rename_with(~ paste0(.x, "_s2"), -date)

# ------------------------------------------------------------
# 10) Compare stations on overlapping dates
# ------------------------------------------------------------
compare_2010_2020 <- full_join(daily1, daily2, by = "date") %>%
  mutate(
    air_temp_diff = air_temp_s2 - air_temp_s1,
    wind_speed_diff = wind_speed_s2 - wind_speed_s1,
    air_pressure_diff = air_pressure_s2 - air_pressure_s1
  ) %>%
  arrange(date)

# ------------------------------------------------------------
# 11) Keep only days where BOTH stations have data
# ------------------------------------------------------------
temp_df <- compare_2010_2020 %>%
  filter(!is.na(air_temp_s1), !is.na(air_temp_s2))

wind_speed_df <- compare_2010_2020 %>%
  filter(!is.na(wind_speed_s1), !is.na(wind_speed_s2))

# Wind direction is a circular variable (0° = 360°), so direct comparisons
# using raw degrees can be misleading near the wrap-around boundary. For example,
# 5° and 355° are only 10° apart in reality, but appear far apart numerically.
# To address this, we compute the circular difference between stations and
# “wrap” the wind direction from station 2 so that it represents the closest
# equivalent angle to station 1. This effectively removes the discontinuity
# at 0/360° and allows for meaningful 1:1 comparisons. Station 1 is used as
# the reference, so station 2 is shifted accordingly; this is a convention and
# does not alter the physical interpretation of the data, only its representation
# for comparison purposes.

wind_dir_df <- compare_2010_2020 %>%
  filter(!is.na(wind_direction_s1), !is.na(wind_direction_s2)) %>%
  mutate(
    # Circular difference in [-180, 180]
    wd_diff = ((wind_direction_s2 - wind_direction_s1 + 180) %% 360) - 180,
    
    # Shift station 2 to the nearest equivalent angle to station 1
    wind_direction_s2_wrapped = wind_direction_s1 + wd_diff
  )

# ------------------------------------------------------------
# 12) 1:1 plot helper
# ------------------------------------------------------------
plot_1to1 <- function(df, x, y, xlab, ylab, title) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_equal() +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
}

# ------------------------------------------------------------
# 13) Temperature and wind speed 1:1 plots
# ------------------------------------------------------------
p_temp <- plot_1to1(
  temp_df,
  x = "air_temp_s1",
  y = "air_temp_s2",
  xlab = "Station 1 Temperature",
  ylab = "Station 2 Temperature",
  title = "Temperature: Station 1 vs Station 2"
)

p_wind_speed <- plot_1to1(
  wind_speed_df,
  x = "wind_speed_s1",
  y = "wind_speed_s2",
  xlab = "Station 1 Wind Speed",
  ylab = "Station 2 Wind Speed",
  title = "Wind Speed: Station 1 vs Station 2"
)

# ------------------------------------------------------------
# 14) Wind direction 1:1 plot using wrapped angles
# ------------------------------------------------------------
lims <- c(-180, 400)

p_wind_dir <- ggplot(wind_dir_df, aes(x = wind_direction_s1, y = wind_direction_s2_wrapped)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  coord_equal(xlim = lims, ylim = lims) +
  labs(
    title = "Wind Direction: Station 1 vs Station 2",
    x = "Station 1 Wind Direction (degrees)",
    y = "Station 2 Wind Direction wrapped"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 15) Show plots
# ------------------------------------------------------------
p_temp
p_wind_speed
p_wind_dir

# ------------------------------------------------------------
# 16) Correlations / summaries
# ------------------------------------------------------------

# Correlations
cor(temp_df$air_temp_s1, temp_df$air_temp_s2, use = "complete.obs")
cor(wind_speed_df$wind_speed_s1, wind_speed_df$wind_speed_s2, use = "complete.obs")

# Wind direction: circular difference summary
mean(wind_dir_df$wd_diff, na.rm = TRUE)
sd(wind_dir_df$wd_diff, na.rm = TRUE)

# Bias (mean error)
mean(temp_df$air_temp_s2 - temp_df$air_temp_s1, na.rm = TRUE)
mean(wind_speed_df$wind_speed_s2 - wind_speed_df$wind_speed_s1, na.rm = TRUE)

# For wind direction, use circular bias instead of raw subtraction
mean(wind_dir_df$wd_diff, na.rm = TRUE)

# RMSE
rmse <- function(x, y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}

rmse(temp_df$air_temp_s1, temp_df$air_temp_s2)
rmse(wind_speed_df$wind_speed_s1, wind_speed_df$wind_speed_s2)

# Do NOT use RMSE on raw wind direction degrees.
# Use the circular difference instead:
sqrt(mean(wind_dir_df$wd_diff^2, na.rm = TRUE))

# Mean Absolute error (mae)
mae <- function(x, y) mean(abs(x - y), na.rm = TRUE)

mae(temp_df$air_temp_s1, temp_df$air_temp_s2)
mae(wind_speed_df$wind_speed_s1, wind_speed_df$wind_speed_s2)
mean(abs(wind_dir_df$wd_diff), na.rm = TRUE)

# ------------------------------------------------------------
# Summary of station comparison (2010–2020)
# ------------------------------------------------------------
# Temperature shows excellent agreement between stations
# (R ≈ 0.99), with a small negative bias (~ -0.37°C) and
# relatively low error (MAE ≈ 1.36°C, RMSE ≈ 1.79°C). This
# indicates the stations are highly consistent and temperature
# data can be considered interchangeable, with minor bias.

# Wind speed shows moderate agreement (R ≈ 0.84), with a
# systematic negative bias (~ -0.67 m/s) and moderate error
# (MAE ≈ 0.97 m/s, RMSE ≈ 1.19 m/s). Differences likely reflect
# local site effects (e.g., roughness, exposure), suggesting
# caution when combining without adjustment.

# Wind direction, evaluated using circular statistics, shows a
# small mean directional bias (~ -11°) but substantial spread
# (MAE ≈ 22°, RMSE ≈ 33°). This indicates general agreement in
# prevailing direction but considerable day-to-day variability,
# consistent with local atmospheric influences.

# Overall, temperature is highly comparable between stations,
# wind speed is moderately comparable with bias, and wind
# direction is suitable for general patterns but less reliable
# for precise agreement.


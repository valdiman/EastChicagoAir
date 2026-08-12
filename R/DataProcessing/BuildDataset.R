# Dataset construction
# Air concentration
# Meteorological data
# Hydrolic conditions
# Water parameters
# Remediation activities

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("lubridate")
}

# Library
{
  library(dplyr)
  library(tidyr)
  library(lubridate)
}

# Air PCB data ------------------------------------------------------------
# in pg/m3
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))
# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Change format to wide
ace_wide <- ace %>%
  pivot_wider(
    id_cols = date,
    names_from = location,
    values_from = c(
      PCB8,
      PCB15,
      PCB18.30,
      PCB20.28,
      PCB31,
      PCB8_unc,
      PCB15_unc,
      PCB18.30_unc,
      PCB20.28_unc,
      PCB31_unc
    ),
    names_glue = "{.value}_{location}"
  )

# Meteorological data -----------------------------------------------------
meteo_data <- read.csv("Data/Meteorology/MeteoEC.csv")
# Change forma to date
meteo_data$date <- as.Date(meteo_data$date, origin = "1899-12-30")

# Transform to Kelvin
meteo_data$air_temp <- meteo_data$air_temp + 273.15

# Calculate inverse temperature
meteo_data <- meteo_data %>%
  mutate(invT = 1000 / air_temp)

# Activity data -----------------------------------------------------------
activity_daily <- read.csv("Data/RemediationActivities/all_activity_dailyV2.csv")
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$activity <- factor(activity_daily$activity)

# Water data --------------------------------------------------------------
# Flow
water_flow <- read.csv("Data/USGS/flow_ihsc.csv")
water_flow$date <- as.Date(water_flow$date)
# Water temperature
water_temp <- read.csv("Data/USGS/tempwater_ihsc.csv")
water_temp$date <- as.Date(water_temp$date)

# Water turbidity
water_turb <- read.csv("Data/USGS/turb_ihsc.csv")
water_turb$date <- as.Date(water_turb$date)

# Merge datasets ----------------------------------------------------------
final_data <- activity_daily %>%
  left_join(ace_wide, by = "date") %>%
  left_join(meteo_data, by = "date") %>%
  left_join(water_flow, by = "date") %>%
  left_join(water_temp, by = "date") %>%
  left_join(water_turb, by = "date") #%>%

# Seasonality variables ---------------------------------------------------
z <- 2 * pi / 365.25

final_data <- final_data %>%
  mutate(
    day_of_year = yday(date),
    sin_season = sin(z * day_of_year),
    cos_season = cos(z * day_of_year))

# Activity-based source wind indicators -----------------------------------
#
# These variables classify wind direction according to the remediation
# activity occurring on each day. They provide a consistent source/non-source
# indicator for the full study period.
#
# Idle:
#   Historical contaminated sediment area.
#
# Dredging:
#   Historical/dredging source sector. This is a broad sector and does not
#   use the daily dredging GPS location.
#
# Construction:
#   Construction-specific source sector based on the construction location.
#
# Source sectors (degrees clockwise from north):
#
# South:
#   Idle / Dredging = 35.64°–121.94°
#   Construction    = 335.93°–73.42° (wraps across 0°)
#
# HS:
#   Idle / Dredging = 352.16°–96.12° (wraps across 0°)
#   Construction    = 354.92°–31.39° (wraps across 0°)
#
# The resulting variables identify whether wind was Source or NonSource
# for the activity occurring on that day.

final_data <- final_data %>%
  mutate(
    SourceWind_South = case_when(
      is.na(wind_direction) ~ NA_character_,
      
      activity == "Idle" &
        between(wind_direction, 35.64, 121.94) ~ "Idle_Source",
      activity == "Idle" ~ "Idle_NonSource",
      
      activity == "Dredging" &
        between(wind_direction, 35.64, 121.94) ~ "Dredging_Source",
      activity == "Dredging" ~ "Dredging_NonSource",
      
      # Construction: wind from approximately north (0° ± 30°)
      activity == "Construction" &
        (wind_direction >= 330 | wind_direction <= 30) ~ "Construction_Source",
      activity == "Construction" ~ "Construction_NonSource",
      
      TRUE ~ NA_character_
    )
  )

final_data <- final_data %>%
  mutate(
    SourceWind_HS = case_when(
      is.na(wind_direction) ~ NA_character_,
      
      activity == "Idle" &
        (wind_direction >= 352.16 | wind_direction <= 96.12) ~ "Idle_Source",
      activity == "Idle" ~ "Idle_NonSource",
      
      activity == "Dredging" &
        (wind_direction >= 352.16 | wind_direction <= 96.12) ~ "Dredging_Source",
      activity == "Dredging" ~ "Dredging_NonSource",
      
      # Construction: wind from approximately north (0° ± 30°)
      activity == "Construction" &
        (wind_direction >= 330 | wind_direction <= 30) ~ "Construction_Source",
      activity == "Construction" ~ "Construction_NonSource",
      
      TRUE ~ NA_character_
    )
  )

# Dredging-specific location and wind exposure -----------------------------
#
# These variables use the daily dredging location estimated from the midpoint
# between Buoy 1 and Buoy 2. Distances are calculated from the estimated
# dredging location to South and HS. Source bearings are calculated from each
# sampling location toward the estimated dredging location, so they can be
# compared directly with meteorological wind direction (which represents the
# direction the wind is coming from).
#
# The continuous wind-angle variables represent the circular difference
# between the observed wind direction and the direction toward the dredging
# location. Smaller values indicate stronger alignment between the wind and
# potential transport from dredging toward the sampling location.
#
# DredgingSource_South and DredgingSource_HS classify dredging as Source when
# the wind-angle is within the selected 30-degree threshold. "NoDredging"
# indicates that dredging was not occurring on that date.

dredge_wind_daily <- read.csv("Data/RemediationActivities/dredge_wind_daily.csv")
dredge_wind_daily$date <- as.Date(dredge_wind_daily$date)

final_data <- final_data %>%
  left_join(
    dredge_wind_daily %>%
      select(
        date,
        n_gps,
        dredging_wind_angle_South,
        dredging_wind_angle_HS,
        DredgingSource_South,
        DredgingSource_HS
      ),
    by = "date"
  )

# Export data
write.csv(final_data, "Data/FinalDataset/DatasetV03.csv",
          row.names = FALSE)

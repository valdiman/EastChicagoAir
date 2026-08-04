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

# Source wind indicators --------------------------------------------------
# The objective of the SourceWind variables is to identify whether the daily
# wind direction originated from the active contamination/remediation source
# relative to each monitoring station (South and HS).
#
# Because the location of the primary source changes throughout the study,
# the source wind sector is defined according to the remediation activity
# occurring on each day:
#
#   • Idle:
#       The source corresponds to the historical contaminated sediment area.
#
#   • Dredging:
#       The same source sector as Idle is used because dredging operations
#       occurred within the historical contaminated area. The dredging
#       location varies daily, but the overall source sector relative to
#       each monitoring station remains within the same angular range.
#
#   • Construction:
#       A different source sector is used because construction activities
#       occurred in a different area of the canal. The angular limits were
#       determined from the construction area relative to each monitoring
#       station using Google Earth Pro bearings.
#
# Source sectors (degrees clockwise from north):
#
# South station
#   Idle / Dredging : 35.64° – 121.94°
#   Construction    : 335.93° – 73.42° (wraps across north)
#
# HS station
#   Idle / Dredging : 352.16° – 96.12° (wraps across north)
#   Construction    : 354.92° – 31.39° (wraps across north)
#
# Wind sectors that cross north (0°/360°) are evaluated using:
#   wind_direction >= lower_limit OR wind_direction <= upper_limit
#
# The resulting variables (SourceWind_South and SourceWind_HS) indicate
# whether the wind originated from the active source sector ("Source") or
# from any other direction ("NonSource") for the corresponding day's activity.

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
      
      activity == "Construction" &
        (wind_direction >= 335.93 | wind_direction <= 73.42) ~ "Construction_Source",
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
      
      activity == "Construction" &
        (wind_direction >= 354.92 | wind_direction <= 31.39) ~ "Construction_Source",
      activity == "Construction" ~ "Construction_NonSource",
      
      TRUE ~ NA_character_
    )
  )

# Export data
write.csv(final_data, "Data/FinalDataset/DatasetV02.csv",
          row.names = FALSE)

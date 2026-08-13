# FINAL DATASET CONSTRUCTION
#
# Builds the daily analysis dataset by combining:
#   - Air PCB concentrations
#   - Meteorological data
#   - Remediation activities
#   - Water flow
#   - Water temperature
#   - Water turbidity
#   - Historical source-wind indicators
#   - Construction source-wind indicators
#   - Dredging-location-specific source-wind indicators
#
# Source concepts are kept separate:
#
#   HistoricalSourceWind_*
#       Persistent historical contaminated-source pathway.
#       Applies regardless of the remediation activity occurring that day.
#
#   ConstructionSourceWind_*
#       Additional construction source pathway.
#       Applies only during construction.
#
#   DredgingSourceWind_*
#       Additional dredging source pathway.
#       Uses the GPS-derived daily dredging location and wind alignment.
#
# Activity remains separate:
#   Idle / Construction / Dredging

# Packages and libraries needed -------------------------------------------
{
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("lubridate")
}

{
  library(dplyr)
  library(tidyr)
  library(lubridate)
}

# AIR PCB DATA ------------------------------------------------------------
# Air PCB concentrations are in pg/m3
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

# Remove blank/invalid location rows
ace <- subset(ace.raw, !grepl("0", location))

# Convert Excel-style date to Date
ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Convert air PCB data to wide format
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

# METEOROLOGICAL DATA -----------------------------------------------------
meteo_data <- read.csv("Data/Meteorology/MeteoEC.csv")

# Convert date
meteo_data$date <- as.Date(meteo_data$date, origin = "1899-12-30")

# Convert air temperature to Kelvin
meteo_data$air_temp <- meteo_data$air_temp + 273.15

# Calculate inverse temperature
meteo_data <- meteo_data %>%
  mutate(
    invT = 1000 / air_temp)

# REMEDIATION ACTIVITY DATA -----------------------------------------------
activity_daily <- read.csv("Data/RemediationActivities/all_activity_dailyV2.csv")

activity_daily$date <- as.Date(activity_daily$date)

activity_daily$activity <- factor(
  activity_daily$activity,
  levels = c(
    "Idle",
    "Construction",
    "Dredging"))

# WATER DATA --------------------------------------------------------------
# Flow
water_flow <- read.csv("Data/USGS/flow_ihsc.csv")
water_flow$date <- as.Date(water_flow$date)

# Water temperature
water_temp <- read.csv("Data/USGS/tempwater_ihsc.csv")
water_temp$date <- as.Date(water_temp$date)

# Water turbidity
water_turb <- read.csv("Data/USGS/turb_ihsc.csv")
water_turb$date <- as.Date(water_turb$date)

# MERGE CORE DATASETS -----------------------------------------------------
final_data <- activity_daily %>%
  left_join(
    ace_wide,
    by = "date"
  ) %>%
  left_join(
    meteo_data,
    by = "date"
  ) %>%
  left_join(
    water_flow,
    by = "date"
  ) %>%
  left_join(
    water_temp,
    by = "date"
  ) %>%
  left_join(
    water_turb,
    by = "date"
  )

# SEASONALITY VARIABLES ---------------------------------------------------
z <- 2 * pi / 365.25

final_data <- final_data %>%
  mutate(
    day_of_year = yday(date),
    sin_season = sin(z * day_of_year),
    cos_season = cos(z * day_of_year)
  )

# HISTORICAL SOURCE-WIND INDICATORS ---------------------------------------
# The historical contaminated area is treated as a potential source
# throughout the entire study period, independent of activity.
#
# Therefore, these variables answer:
#
#   "Was the wind coming from the historical contaminated source area
#    toward the monitoring station?"
#
# These variables do NOT depend on whether the day was Idle, Construction,
# or Dredging.
#
# Source sectors:
# South: 35.64° - 121.94°
#
# HS: 352.16° - 96.12°(wraps across 0°)

final_data <- final_data %>%
  mutate(
    
    HistoricalSourceWind_South = case_when(
      
      is.na(wind_direction) ~
        NA_character_,
      
      between(
        wind_direction,
        35.64,
        121.94
      ) ~
        "Source",
      
      TRUE ~
        "NonSource"
    ),
    
    HistoricalSourceWind_HS = case_when(
      
      is.na(wind_direction) ~
        NA_character_,
      
      wind_direction >= 352.16 |
        wind_direction <= 96.12 ~
        "Source",
      
      TRUE ~
        "NonSource"
    )
  )

# Convert to factors
final_data <- final_data %>%
  mutate(
    
    HistoricalSourceWind_South = factor(
      HistoricalSourceWind_South,
      levels = c(
        "NonSource",
        "Source"
      )
    ),
    
    HistoricalSourceWind_HS = factor(
      HistoricalSourceWind_HS,
      levels = c(
        "NonSource",
        "Source"
      )
    )
  )

# CONSTRUCTION SOURCE-WIND INDICATORS -------------------------------------
# Construction is an additional potential source that exists only during
# construction days.
#
# For both sampling locations, construction is treated as a potential source
# when wind comes approximately from north.
#
# Source sector:
#   330° - 360° / 0° - 30°
#
# This corresponds to approximately 0° ± 30°.
#
# NoConstruction = construction was not occurring.
# Source         = construction was occurring and wind was from the source
#                  sector.
# NonSource      = construction was occurring but wind was outside the
#                  source sector.

construction_source_angle <- 30

final_data <- final_data %>%
  mutate(
    
    ConstructionSourceWind_South = case_when(
      
      activity != "Construction" ~
        "NoConstruction",
      
      is.na(wind_direction) ~
        NA_character_,
      
      wind_direction >=
        (360 - construction_source_angle) |
        wind_direction <=
        construction_source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    ),
    
    ConstructionSourceWind_HS = case_when(
      
      activity != "Construction" ~
        "NoConstruction",
      
      is.na(wind_direction) ~
        NA_character_,
      
      wind_direction >=
        (360 - construction_source_angle) |
        wind_direction <=
        construction_source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    )
  )

# Convert to factors
final_data <- final_data %>%
  mutate(
    
    ConstructionSourceWind_South = factor(
      ConstructionSourceWind_South,
      levels = c(
        "NoConstruction",
        "NonSource",
        "Source"
      )
    ),
    
    ConstructionSourceWind_HS = factor(
      ConstructionSourceWind_HS,
      levels = c(
        "NoConstruction",
        "NonSource",
        "Source"
      )
    )
  )

# DREDGING-SPECIFIC LOCATION AND WIND EXPOSURE ----------------------------
# The dredging-specific analysis uses the actual daily dredging location
# estimated from the midpoint between Buoy 1 and Buoy 2.
#
# The dredging wind alignment variables were calculated in the separate
# dredge_wind_daily analysis:
#
#   dredging_wind_alignment_South_deg
#   dredging_wind_alignment_HS_deg
#
# Interpretation:
#
#   0°   = wind coming directly from the dredging location toward the sampler
#   90°  = approximately crosswind
#   180° = wind coming from the opposite direction
#
# DredgingSourceWind variables are separate from HistoricalSourceWind.
#
# Therefore, on a dredging day it is possible to have:
#
#   HistoricalSourceWind_South = Source
#   DredgingSourceWind_South   = NonSource
#
# meaning the historical contaminated source is upwind, but the actual
# dredging location is not.

dredge_wind_daily <- read.csv("Data/RemediationActivities/dredge_wind_daily.csv")
dredge_wind_daily$date <- as.Date(dredge_wind_daily$date)

# Add only variables that are new to the master dataset.
#
# We do not re-import:
#   - wind_direction
#   - wind_speed
#   - turbidity
#   - dredging distance
#   - daily volume
#
# because those variables already exist in final_data.

final_data <- final_data %>%
  left_join(
    dredge_wind_daily %>%
      select(
        date,
        n_gps,
        location_quality,
        dredging_wind_alignment_South_deg,
        dredging_wind_alignment_HS_deg
      ),
    by = "date")

# DREDGING SOURCE-WIND CLASSIFICATION -------------------------------------
# DredgingSourceWind variables answer:
#
#   "On a dredging day, was the wind sufficiently aligned with the actual
#    daily dredging location to potentially transport material toward the
#    sampling station?"
#
# Primary threshold:
#   ±30 degrees
#
# Alternative thresholds such as ±15° and ±45° can be evaluated later
# as sensitivity analyses.
#
# Categories:
#
#   NoDredging
#       No dredging occurred on that date.
#
#   Source
#       Dredging occurred, the location and wind direction were available,
#       and wind alignment was within the selected threshold.
#
#   NonSource
#       Dredging occurred, the location and wind direction were available,
#       but wind alignment was outside the selected threshold.
#
#   NA
#       Dredging occurred, but the source status could not be determined
#       because the dredging location and/or wind direction was unavailable.

dredging_source_angle <- 30

final_data <- final_data %>%
  mutate(
    
    DredgingSourceWind_South = case_when(
      
      activity != "Dredging" ~
        "NoDredging",
      
      is.na(
        dredging_wind_alignment_South_deg
      ) ~
        NA_character_,
      
      dredging_wind_alignment_South_deg <=
        dredging_source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    ),
    
    DredgingSourceWind_HS = case_when(
      
      activity != "Dredging" ~
        "NoDredging",
      
      is.na(
        dredging_wind_alignment_HS_deg
      ) ~
        NA_character_,
      
      dredging_wind_alignment_HS_deg <=
        dredging_source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    )
  )

# Convert to factors
final_data <- final_data %>%
  mutate(
    
    DredgingSourceWind_South = factor(
      DredgingSourceWind_South,
      levels = c(
        "NoDredging",
        "NonSource",
        "Source"
      )
    ),
    
    DredgingSourceWind_HS = factor(
      DredgingSourceWind_HS,
      levels = c(
        "NoDredging",
        "NonSource",
        "Source"
      )
    )
  )

# QUALITY-CONTROL CHECKS --------------------------------------------------
# Activity counts
table(final_data$activity, useNA = "ifany")

# Historical source wind
table(final_data$activity, final_data$HistoricalSourceWind_South,
      useNA = "ifany")

table(final_data$activity, final_data$HistoricalSourceWind_HS,
      useNA = "ifany")

# Construction source wind
table(final_data$activity, final_data$ConstructionSourceWind_South,
      useNA = "ifany")

table(final_data$activity, final_data$ConstructionSourceWind_HS,
      useNA = "ifany")

# Dredging source wind
table(final_data$activity, final_data$DredgingSourceWind_South,
      useNA = "ifany")

table(final_data$activity, final_data$DredgingSourceWind_HS,
      useNA = "ifany")

# Check dredging days with missing source classification
final_data %>%
  filter(
    activity == "Dredging",
    is.na(DredgingSourceWind_South) |
      is.na(DredgingSourceWind_HS)
  ) %>%
  select(
    date,
    n_gps,
    location_quality,
    wind_direction,
    dredging_wind_alignment_South_deg,
    dredging_wind_alignment_HS_deg,
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  )

# QC for the location
final_data <- final_data %>%
  mutate(
    location_quality = factor(
      location_quality,
      levels = c("None", "Low", "Moderate", "Good"),
      ordered = TRUE))

# EXPORT ------------------------------------------------------------------
write.csv(final_data, "Data/FinalDataset/DatasetV03.csv",
          row.names = FALSE)

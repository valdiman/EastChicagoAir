# Dredging location and wind analysis --------------------------------------
#
# Purpose:
#   1. Estimate the daily dredging location from the midpoint between
#      Buoy 1 and Buoy 2.
#   2. Calculate the distance from the estimated dredging location to
#      the South and HS sampling locations.
#   3. Calculate the bearing from each sampling location toward the
#      estimated dredging location.
#   4. Compare the observed daily wind direction with those bearings.
#   5. Classify dredging as Source, NonSource, or NoDredging.
#
# Source = dredging was occurring and the wind direction was within
#          the selected angular threshold of the dredging source direction.
# NonSource = dredging was occurring but wind direction was outside
#             the selected source threshold.
# NoDredging = no dredging was occurring on that date.
# NA = dredging was occurring, but the wind direction and/or dredging
#      location could not be determined.
#
# The 30-degree source threshold is an analytical assumption and can
# be evaluated later using sensitivity analyses with alternative thresholds.

# Install packages
{
  install.packages("tidyverse")
  install.packages("lubridate")
  install.packages("geosphere")
}

# Upload libraries
{
  library(tidyverse)
  library(lubridate)
  library(geosphere)
}

# Helper functions ----------------------------------------------------------

# Convert values to numeric.
# Text values such as "Offline" are converted to NA.
clean_num <- function(x) {
  x <- suppressWarnings(
    readr::parse_number(as.character(x))
  )
  
  x[!is.finite(x)] <- NA_real_
  
  x
}

# Clean latitude and remove impossible coordinates
clean_lat <- function(x) {
  x <- clean_num(x)
  
  x[!is.na(x) & (x < -90 | x > 90)] <- NA_real_
  
  x
}

# Clean longitude and remove impossible coordinates
clean_lon <- function(x) {
  x <- clean_num(x)
  
  x[!is.na(x) & (x < -180 | x > 180)] <- NA_real_
  
  x
}

# Median that returns NA if all observations are missing
safe_median <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    NA_real_
  } else {
    median(x)
  }
}

# Minimum circular difference between two angles
# Returns values from 0 to 180 degrees.
angle_diff <- function(a, b) {
  abs((a - b + 180) %% 360 - 180)
}

# Sampling locations --------------------------------------------------------
# Coordinates are longitude, latitude.
south <- c(-87.487138, 41.647095)
hs <- c(-87.487700, 41.641241)

# Read turbidity / buoy data from ACE --------------------------------------
years <- c(2012:2020, 2024)

dredge_turb <- map_dfr(years, ~{
  
  read_csv(
    paste0("Data/ACE/waterquality_", .x, ".csv"),
    
    col_select = c(
      ReadingDate,
      DredgeContribution,
      Buoy1Latitude,
      Buoy1Longitude,
      Buoy2Latitude,
      Buoy2Longitude
    ),
    
    col_types = cols(
      ReadingDate = col_character(),
      DredgeContribution = col_character(),
      Buoy1Latitude = col_character(),
      Buoy1Longitude = col_character(),
      Buoy2Latitude = col_character(),
      Buoy2Longitude = col_character()
    ),
    
    show_col_types = FALSE
  )
})

# Clean dates, turbidity, and GPS data -------------------------------------
dredge_turb <- dredge_turb %>%
  mutate(
    ReadingDate = mdy_hm(ReadingDate),
    
    date = as.Date(ReadingDate),
    
    DredgeContribution = clean_num(DredgeContribution),
    
    Buoy1Latitude = clean_lat(Buoy1Latitude),
    Buoy1Longitude = clean_lon(Buoy1Longitude),
    
    Buoy2Latitude = clean_lat(Buoy2Latitude),
    Buoy2Longitude = clean_lon(Buoy2Longitude)
  ) %>%
  filter(
    !is.na(date),
    date != as.Date("2024-08-28")
  )

# Estimate dredging location at each 15-minute observation -----------------
dredge_turb <- dredge_turb %>%
  mutate(
    dredge_lat = if_else(
      !is.na(Buoy1Latitude) &
        !is.na(Buoy2Latitude),
      
      (Buoy1Latitude + Buoy2Latitude) / 2,
      
      NA_real_
    ),
    
    dredge_lon = if_else(
      !is.na(Buoy1Longitude) &
        !is.na(Buoy2Longitude),
      
      (Buoy1Longitude + Buoy2Longitude) / 2,
      
      NA_real_
    )
  )

# Calculate 15-minute distance and bearing to each station -----------------
dredge_turb <- dredge_turb %>%
  mutate(
    
    # Distance from dredge to South
    dredging_distance_to_South_m =
      distHaversine(
        cbind(dredge_lon, dredge_lat),
        south
      ),
    
    # Distance from dredge to HS
    dredging_distance_to_HS_m =
      distHaversine(
        cbind(dredge_lon, dredge_lat),
        hs
      ),
    
    # Bearing from dredge toward South
    dredging_bearing_to_South =
      bearing(
        cbind(dredge_lon, dredge_lat),
        south
      ) %% 360,
    
    # Bearing from dredge toward HS
    dredging_bearing_to_HS =
      bearing(
        cbind(dredge_lon, dredge_lat),
        hs
      ) %% 360
  )

# Summarize to one row per day ----------------------------------------------
dredge_daily <- dredge_turb %>%
  group_by(date) %>%
  summarize(
    
    # Daily data-availability / quality-control indicators ---------------------
    #
    # The ACE water-quality files contain measurements at approximately
    # 15-minute intervals, corresponding to a maximum of about 96 observations
    # per complete day (24 hours x 4 observations per hour).
    #
    # n_turb:
    #   Number of valid (non-missing) DredgeContribution observations available
    #   for the day. This variable describes the amount of information supporting
    #   the daily dredging-related turbidity statistics (mean, minimum, and
    #   maximum). A value of 96 represents approximately complete daily coverage,
    #   whereas lower values indicate partial coverage. A value of 0 indicates
    #   that no valid DredgeContribution measurements were available that day.
    #
    # n_gps:
    #   Number of observations for which a valid dredging location could be
    #   estimated from the midpoint between Buoy 1 and Buoy 2. A valid midpoint
    #   requires usable latitude and longitude information from both buoys.
    #   This variable therefore describes the amount of GPS information supporting
    #   the estimated daily dredging location, distance to South/HS, source
    #   bearing, and dredging-wind alignment calculations.
    #
    # n_turb and n_gps are evaluated independently. A day may therefore have
    # valid GPS information but no valid DredgeContribution measurements, or
    # vice versa. Missing turbidity measurements do not automatically imply that
    # the dredging location is unavailable, and missing GPS information does not
    # automatically imply that dredging did not occur.
    #
    # These variables are retained as quality-control indicators and can be used
    # in sensitivity analyses to assess whether results depend on the amount of
    # daily observational coverage.
    
    # Number of valid turbidity measurements
    n_turb = sum(!is.na(DredgeContribution)),
    
    # Number of observations with valid buoy midpoint
    n_gps = sum(
      !is.na(dredge_lat) &
        !is.na(dredge_lon)
    ),
    
    # Daily turbidity statistics
    turb_dredge_mean = if (n_turb > 0) {
      mean(
        DredgeContribution,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    turb_dredge_min = if (n_turb > 0) {
      min(
        DredgeContribution,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    turb_dredge_max = if (n_turb > 0) {
      max(
        DredgeContribution,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    # Daily representative dredging location
    dredge_lat = safe_median(dredge_lat),
    dredge_lon = safe_median(dredge_lon),
    
    # Daily representative distance
    dredging_distance_to_South_m =
      safe_median(
        dredging_distance_to_South_m
      ),
    dredging_distance_to_HS_m =
      safe_median(
        dredging_distance_to_HS_m
      ),
    .groups = "drop"
  )

# Calculate daily source bearings ------------------------------------------
# These bearings are calculated FROM the sampling station TO the estimated
# dredging location, because meteorological wind direction represents the
# direction the wind is coming FROM.

dredge_daily <- dredge_daily %>%
  mutate(
    dredging_source_bearing_South = if_else(
      !is.na(dredge_lat) &
        !is.na(dredge_lon),
      
      bearing(
        south,
        cbind(
          dredge_lon,
          dredge_lat
        )
      ) %% 360,
      
      NA_real_
    ),
    
    dredging_source_bearing_HS = if_else(
      !is.na(dredge_lat) &
        !is.na(dredge_lon),
      
      bearing(
        hs,
        cbind(
          dredge_lon,
          dredge_lat
        )
      ) %% 360,
      
      NA_real_
    )
  )

# Read daily meteorological data -------------------------------------------
meteo_data <- read.csv("Data/Meteorology/MeteoEC.csv")

meteo_data$date <- as.Date(meteo_data$date, origin = "1899-12-30")

# Add meteorological variables ---------------------------------------------
dredge_wind_daily <- dredge_daily %>%
  left_join(
    meteo_data %>%
      select(
        date,
        wind_direction,
        wind_speed
      ),
    by = "date")

# Calculate circular wind-source angle -------------------------------------
# 0 degrees = wind coming directly from the direction of the dredging source.
# 90 degrees = approximately crosswind.
# 180 degrees = wind coming from the opposite direction.

dredge_wind_daily <- dredge_wind_daily %>%
  mutate(
    
    dredging_wind_alignment_South_deg =
      angle_diff(
        wind_direction,
        dredging_source_bearing_South
      ),
    
    dredging_wind_alignment_HS_deg =
      angle_diff(
        wind_direction,
        dredging_source_bearing_HS
      )
  )

# Read daily remediation activity data -------------------------------------
activity_daily <- read.csv("Data/RemediationActivities/all_activity_dailyV2.csv")
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$activity <- factor(activity_daily$activity)

# Add activity and daily dredged volume ------------------------------------
dredge_wind_daily <- dredge_wind_daily %>%
  left_join(
    activity_daily %>%
      select(
        date,
        activity),
    by = "date")

# Classify dredging-specific source wind -----------------------------------
# DredgingSourceWind variables identify whether wind conditions were
# consistent with transport from the estimated daily dredging location
# toward each air monitoring station (South and HS).
#
# The daily dredging location is estimated from the midpoint between the
# upstream and downstream buoy GPS positions. For each station, the direction
# from the station toward the estimated dredging location is compared with
# the observed meteorological wind direction.
#
# dredging_wind_alignment_*_deg is the absolute circular angular difference
# between these two directions and ranges from 0 to 180 degrees:
#
#   0 degrees   = wind coming directly from the dredging location
#                 toward the monitoring station (maximum alignment)
#   90 degrees  = approximately crosswind
#   180 degrees = wind coming from the opposite direction
#
# A source-angle threshold is used to convert the continuous alignment
# measure into a categorical dredging-source indicator. The primary
# classification uses +/- 30 degrees. Alternative thresholds (e.g.,
# +/- 15 and +/- 45 degrees) can be evaluated in sensitivity analyses.
#
# Categories:
#
#   NoDredging = no dredging occurred on that date.
#
#   Source = dredging occurred and the wind-direction difference was
#            <= source_angle, indicating wind aligned with transport
#            from the dredging location toward the monitoring station.
#
#   NonSource = dredging occurred but the wind-direction difference was
#               > source_angle.
#
#   NA = dredging occurred, but the classification could not be determined
#        because wind direction and/or dredging location was unavailable.

source_angle <- 30  # Primary threshold; sensitivity: 15, 45 degrees

dredge_wind_daily <- dredge_wind_daily %>%
  mutate(
    
    DredgingSourceWind_South = case_when(
      
      activity != "Dredging" ~
        "NoDredging",
      
      is.na(dredging_wind_alignment_South_deg) ~
        NA_character_,
      
      dredging_wind_alignment_South_deg <= source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    ),
    
    DredgingSourceWind_HS = case_when(
      
      activity != "Dredging" ~
        "NoDredging",
      
      is.na(dredging_wind_alignment_HS_deg) ~
        NA_character_,
      
      dredging_wind_alignment_HS_deg <= source_angle ~
        "Source",
      
      TRUE ~
        "NonSource"
    ),
    
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

# Optional quality indicator for daily GPS location ------------------------
dredge_wind_daily <- dredge_wind_daily %>%
  mutate(
    
    location_quality = case_when(
      
      n_gps >= 48 ~ "Good",
      
      n_gps >= 10 ~ "Moderate",
      
      n_gps > 0 ~ "Low",
      
      TRUE ~ "None"
    ),
    
    location_quality = factor(
      location_quality,
      levels = c(
        "None",
        "Low",
        "Moderate",
        "Good"
      )
    )
  )

# Inspect final dredging table ---------------------------------------------
dredge_wind_daily %>%
  select(
    date,
    activity,
    
    # Quality control
    n_gps,
    location_quality,
    
    # Dredging turbidity
    turb_dredge_mean,
    turb_dredge_min,
    turb_dredge_max,
    
    # South
    dredging_distance_to_South_m,
    dredging_wind_alignment_South_deg,
    DredgingSourceWind_South,
    
    # HS
    dredging_distance_to_HS_m,
    dredging_wind_alignment_HS_deg,
    DredgingSourceWind_HS
  ) %>%
  print(n = 20)

# Create final dredging wind dataset --------------------------------------
final_data <- dredge_wind_daily %>%
  select(
    date,
    n_gps,
    location_quality,
    dredging_wind_alignment_South_deg,
    DredgingSourceWind_South,
    dredging_wind_alignment_HS_deg,
    DredgingSourceWind_HS
  )

# Save ---------------------------------------------------------------------
write.csv(final_data,"Data/RemediationActivities/dredge_wind_daily.csv",
          row.names = FALSE)


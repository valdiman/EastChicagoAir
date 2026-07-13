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
activity_daily <- read.csv("Data/RemediationActivities/all_activity_daily.csv")
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$Construction <- factor(activity_daily$Construction)
activity_daily$Dredging <- factor(activity_daily$Dredging)
activity_daily$Idle <- factor(activity_daily$Idle)

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

# Water turbidity dredging
water_dredg_turb <- read.csv("Data/ACE/tubidity_dredge_ihsc.csv")
water_dredg_turb$date <- as.Date(water_dredg_turb$date)

# Merge datasets ----------------------------------------------------------
final_data <- activity_daily %>%
  left_join(ace_wide, by = "date") %>%
  left_join(meteo_data, by = "date") %>%
  left_join(water_flow, by = "date") %>%
  left_join(water_temp, by = "date") %>%
  left_join(water_turb, by = "date") %>%
  left_join(water_dredg_turb, by = "date")

# Seasonality variables ---------------------------------------------------
z <- 2 * pi / 365.25

final_data <- final_data %>%
  mutate(
    julian_day = yday(date),
    sin_season = sin(z * julian_day),
    cos_season = cos(z * julian_day))

# Source wind indicators --------------------------------------------------
final_data <- final_data %>%
  mutate(
    SourceWind_South = factor(
      ifelse(
        between(wind_direction, 30, 90),
        "Source",
        "NonSource"),
      levels = c("NonSource", "Source")),  # NonSource is the reference,
    SourceWind_HS = factor(
      ifelse(
        between(wind_direction, 0, 70),
        "Source",
        "NonSource"),
      levels = c("NonSource", "Source")))  # NonSource is the reference

# Export data
write.csv(final_data, "Data/FinalDataset/DatasetV01.csv",
          row.names = FALSE)

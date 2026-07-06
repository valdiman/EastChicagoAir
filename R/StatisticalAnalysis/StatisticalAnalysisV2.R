# Air PCB concentrations during remediation activities

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("ggplot")
  install.packages("car")
  install.packages("emmeans")
}

# Library
{
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(car)
  library(emmeans)
}

# Air PCB data ------------------------------------------------------------
# in pg/m3
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

ace <- ace.raw %>%
  filter(location != 0)

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
meteo_data <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")
# Change forma to date
meteo_data$date <- as.Date(meteo_data$date, origin = "1899-12-30")

# Transform to Kelvin
meteo_data$air_temp <- meteo_data$air_temp + 273.15

# Calculate inverse temperature
meteo_data <- meteo_data %>%
  mutate(invT = 1000 / air_temp)

# Activity data -----------------------------------------------------------
activity_daily <- read.csv("Data/RemediationActivities/activity_dailyV2.csv")
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$Activity <- factor(activity_daily$Activity)

# Water data --------------------------------------------------------------
# Flow
water_flow <- read.csv("Data/USGS/flow_ihsc.csv")
water_flow$date <- as.Date(water_flow$date)
# Get unique values
#water_flow_unique <- water_flow[!duplicated(water_flow$date), ]
# Water temperature
water_temp <- read.csv("Data/USGS/tempwater_ihsc.csv")
water_temp$date <- as.Date(water_temp$date)
# Get unique values
#water_temp_unique <- water_temp[!duplicated(water_temp$date), ]

# Water turbidity
water_turb <- read.csv("Data/USGS/turb_ihsc.csv")
water_turb$date <- as.Date(water_turb$date)
# Get unique values
#water_turb_unique <- water_turb[!duplicated(water_turb$date), ]

# Merge datasets ----------------------------------------------------------
ace_wide <- ace_wide %>%
  left_join(
    meteo_data, by = "date"
    ) %>%
  left_join(
    water_flow, by = "date"
    ) %>%
  left_join(
    water_temp, by = "date"
    ) %>%
  left_join(
    water_turb, by = "date"
    ) %>%
  left_join(
    activity_daily, by = "date")

ace$Activity <-
  relevel(ace$Activity, ref = "Idle")

# Seasonality variables ---------------------------------------------------
z <- 2 * pi / 365.25

# South
south_ace$julian_day <- as.numeric(format(south_ace$date, "%j"))
south_ace$sin_season <- sin(z * south_ace$julian_day)
south_ace$cos_season <- cos(z * south_ace$julian_day)

# HS
hs_ace$julian_day <- as.numeric(format(hs_ace$date, "%j"))
hs_ace$sin_season <- sin(z * hs_ace$julian_day)
hs_ace$cos_season <- cos(z * hs_ace$julian_day)

# SourceWind --------------------------------------------------------------
south_ace$SourceWind <- factor(
  ifelse(
    between(south_ace$wind_direction, 30, 90), "Source", "NonSource"),
  levels = c("NonSource", "Source")) # NonSource is the reference

hs_ace$SourceWind <- factor(
  ifelse(
    between(hs_ace$wind_direction, 0, 70), "Source", "NonSource"),
  levels = c("NonSource", "Source"))


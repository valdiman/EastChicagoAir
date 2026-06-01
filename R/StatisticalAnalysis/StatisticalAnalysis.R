# Preliminary statistical analysis
# Initial ideas:
# (1) difference between activities
# (2) time trend analysis
# (3) spatial analysis

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("tidyr")
  install.packages("car")
  install.packages("rstatix")
}

# Upload libraries
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(car)
  library(rstatix)
  library(emmeans)
}

# Read data ---------------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

# Prepare data ------------------------------------------------------------
# Remove empty spaces
ace <- ace.raw %>%
  filter(location != 0)

# Select columns
ace <- ace[, 1:7]

# Change units to pg/m3 from ng/m3
ace <- ace %>%
  mutate(across(starts_with("PCB"), ~ . / 1000))
# Convert numeric date to Date format assuming Excel-style serial number
# ccvs file needs to be this format for the date XXXXX (e.g., 41188)
ace$date <- as.Date(ace$date, origin = "1899-12-30")
# Check format
str(ace[1:3, ])

# Read air meteorological conditions --------------------------------------
meteo.data <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")
# Convert format to date
meteo.data$date <- as.Date(meteo.data$date, origin = "1899-12-30")
# Change air temperature to Kelvin
meteo.data$air_temp <- meteo.data$air_temp + 273.15
# Add 1/T (invT)
meteo.data <- meteo.data %>%
  mutate(invT = 1000 / meteo.data$air_temp)
# Because 2 locations, meteorological data is duplicates
meteo_unique <- meteo.data[!duplicated(meteo.data$date), ]
# Check format
str(meteo_unique[1:3, ])

# Read daily activity -----------------------------------------------------
activity_daily <- read.csv("Data/RemediationProject/activity_daily.csv")
# Change format
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$Activity <- as.factor(activity_daily$Activity)

# Add meteorological and daily activities to ace --------------------------
ace <- ace %>%
  left_join(meteo_unique, by = "date") %>%
  left_join(activity_daily, by = "date")

# Set idle as reference
ace$Activity <- relevel(ace$Activity, ref = "Idle")

# Select locations
south_ace <- subset(ace, location == "South")
hs_ace <- subset(ace, location == "HS")

# Filter by wind direction
# Both sites are located in general southeast of dredging
# Select only when wind is blowing into both sites
# 0° for North, 90° for East, 180° for South, and 270° for West
south_ace_w <- south_ace %>%
  filter(
    Activity != "Dredging" |
      between(wind_direction, 30, 90))

hs_ace_w <- hs_ace %>%
  filter(
    Activity != "Dredging" |
      between(wind_direction, 0, 70))

# Statistical analysis: Activities ----------------------------------------
# H1: Airborne PCB concentrations differ among dredging, construction,
# and idle periods at the South site.
south_ace$julian_day <- as.numeric(format(south_ace$date, "%j"))

z <- 2*pi/365.25

south_ace$sin_season <- sin(z * south_ace$julian_day)
south_ace$cos_season <- cos(z * south_ace$julian_day)

# PCB8
fit.pcb8.s <- lm(log10(PCB8) ~ Activity + invT + wind_speed + sin_season +
                 cos_season, data = south_ace)

car::Anova(fit.pcb8.s, type = 2)

emmeans(fit.pcb8.s, ~ Activity)

pairs(emmeans(fit.pcb8.s, ~ Activity))


# PCB 15
fit.pcb15.s <- lm(log10(PCB15) ~ Activity + invT + wind_speed + sin_season +
                   cos_season, data = south_ace)

car::Anova(fit.pcb15.s, type = 2)

emmeans(fit.pcb15.s, ~ Activity)

pairs(emmeans(fit.pcb15.s, ~ Activity))

# PCB 18+30
fit.pcb18.s <- lm(log10(PCB18.30) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = south_ace)

car::Anova(fit.pcb18.s, type = 2)

emmeans(fit.pcb18.s, ~ Activity)

pairs(emmeans(fit.pcb18.s, ~ Activity))

# PCB 20+28
fit.pcb20.s <- lm(log10(PCB20.28) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = south_ace)

car::Anova(fit.pcb20.s, type = 2)

emmeans(fit.pcb20.s, ~ Activity)

pairs(emmeans(fit.pcb20.s, ~ Activity))

# PCB 31
fit.pcb31.s <- lm(log10(PCB31) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = south_ace)

car::Anova(fit.pcb31.s, type = 2)

emmeans(fit.pcb31.s, ~ Activity)

pairs(emmeans(fit.pcb31.s, ~ Activity))

# Summary
south_h1_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  F_TypeII = c(11.29, 45.64, 56.25, 62.91, 53.02),
  P_Value = c("<0.001", "<0.001", "<0.001", "<0.001", "<0.001"),
  Pattern = c(
    "D > C ≈ I",
    "D > I > C",
    "D > I > C",
    "D > I > C",
    "D > I > C"
  ),
  Idle_vs_Construction_pct = c(NA, 18, 29, 24, 22),
  Dredging_vs_Idle_pct = c(48, 67, 118, 107, 101),
  Dredging_vs_Construction_pct = c(39, 96, 181, 157, 145)
)

south_h1_summary

# HS
hs_ace$julian_day <- as.numeric(format(hs_ace$date, "%j"))

z <- 2*pi/365.25

hs_ace$sin_season <- sin(z * hs_ace$julian_day)
hs_ace$cos_season <- cos(z * hs_ace$julian_day)

# PCB8
fit.pcb8.hs <- lm(log10(PCB8) ~ Activity + invT + wind_speed + sin_season +
                   cos_season, data = hs_ace)

car::Anova(fit.pcb8.hs, type = 2)

emmeans(fit.pcb8.hs, ~ Activity)

pairs(emmeans(fit.pcb8.hs, ~ Activity))

# PCB15
fit.pcb15.hs <- lm(log10(PCB15) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = hs_ace)

car::Anova(fit.pcb15.hs, type = 2)

emmeans(fit.pcb15.hs, ~ Activity)

pairs(emmeans(fit.pcb15.hs, ~ Activity))

# PCB18+30
fit.pcb18.hs <- lm(log10(PCB18.30) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = hs_ace)

car::Anova(fit.pcb18.hs, type = 2)

emmeans(fit.pcb18.hs, ~ Activity)

pairs(emmeans(fit.pcb18.hs, ~ Activity))

# PCB20+28
fit.pcb20.hs <- lm(log10(PCB20.28) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = hs_ace)

car::Anova(fit.pcb20.hs, type = 2)

emmeans(fit.pcb20.hs, ~ Activity)

pairs(emmeans(fit.pcb20.hs, ~ Activity))

# PCB31
fit.pcb31.hs <- lm(log10(PCB31) ~ Activity + invT + wind_speed + sin_season +
                    cos_season, data = hs_ace)

car::Anova(fit.pcb31.hs, type = 2)

emmeans(fit.pcb31.hs, ~ Activity)

pairs(emmeans(fit.pcb31.hs, ~ Activity))

hs_h1_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  F_TypeII = c(32.05, 3.61, 0.70, 0.11, 2.27),
  P_Value = c("<0.001", "0.027", "0.495", "0.895", "0.104"),
  Pattern = c(
    "Construction > Idle > Dredging",
    "Construction ≈ Idle > Dredging",
    "No Activity Effect",
    "No Activity Effect",
    "No Activity Effect"
  )
)

hs_h1_summary

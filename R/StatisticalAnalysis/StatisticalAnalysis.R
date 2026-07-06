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
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

ace <- ace.raw %>%
  filter(location != 0)

ace <- ace[, 1:7]

# From pg to ng
ace <- ace %>%
  mutate(across(starts_with("PCB"), ~ . / 1000))

# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Meteorological data -----------------------------------------------------
meteo.data <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")

# Change forma to date
meteo.data$date <- as.Date(meteo.data$date, origin = "1899-12-30")

# Transform to Kelvin
meteo.data$air_temp <- meteo.data$air_temp + 273.15

# Calculate inverse temperature
meteo.data <- meteo.data %>%
  mutate(invT = 1000 / air_temp)

# Get unique date values
meteo_unique <- meteo.data[!duplicated(meteo.data$date), ]

# Activity data -----------------------------------------------------------
activity_daily <- read.csv("Data/RemediationProject/activity_dailyV2.csv")
activity_daily$date <- as.Date(activity_daily$date)
activity_daily$Activity <- factor(activity_daily$Activity)

# Water data --------------------------------------------------------------
# Flow
water_flow <- read.csv("Data/USGS/flow_ihsc.csv")
water_flow$date <- as.Date(water_flow$date)
# Get unique values
water_flow_unique <- water_flow[!duplicated(water_flow$date), ]
# Water temperature
water_temp <- read.csv("Data/USGS/tempwater_ihsc.csv")
water_temp$date <- as.Date(water_temp$date)
# Get unique values
water_temp_unique <- water_temp[!duplicated(water_temp$date), ]

# Water turbidity
water_turb <- read.csv("Data/USGS/turb_ihsc.csv")
water_turb$date <- as.Date(water_turb$date)
# Get unique values
water_turb_unique <- water_turb[!duplicated(water_turb$date), ]

# Merge datasets ----------------------------------------------------------
ace <- ace %>%
  left_join(
    meteo_unique, by = "date"
    ) %>%
  left_join(
    water_flow_unique, by = "date"
    ) %>%
  left_join(
    water_temp_unique, by = "date"
    ) %>%
  left_join(
    water_turb_unique, by = "date"
    ) %>%
  left_join(
    activity_daily, by = "date")

ace$Activity <-
  relevel(ace$Activity, ref = "Idle")

# Monitoring locations ----------------------------------------------------
south_ace <- subset(ace, location == "South")

hs_ace <- subset(ace, location == "HS")

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

# Common environmental covariates
covars <- paste(
  "invT",
  "wind_speed",
  "sin_season",
  "cos_season",
  "flow_abs",
  "water_temp",
  sep = " + ")

# Activity model
run_activity_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ ",
                        "Activity + SourceWind + ",
                        covars)
  
  fit <- lm(as.formula(formula_txt), data = data)
  
  list(model = fit, anova = car::Anova(fit, type = 2),
       activity = emmeans(fit, pairwise ~ Activity),
       sourcewind = emmeans(fit, pairwise ~ SourceWind),
       summary = summary(fit))
}

# Exposure model
run_exposure_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ ",
                        "Exposure_d1 + SourceWind + ",
                        covars)
  
  fit <- lm(as.formula(formula_txt), data = data)
  
  list(model = fit, anova = car::Anova(fit, type = 2),
       summary = summary(fit))
}

# Distance model
run_distance_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ ",
                        "log10(MinDist_Dredge) + SourceWind + ",
                        covars)
  
  fit <- lm(as.formula(formula_txt),
            data = subset(data,
                          !is.na(MinDist_Dredge)))
  
  list(model = fit, anova = car::Anova(fit, type = 2),
       summary = summary(fit))
}

# Turbidity-enhanced models
covars_turb <- paste(
  "invT",
  "wind_speed",
  "sin_season",
  "cos_season",
  "flow_abs",
  "water_temp",
  "turb_FNU",
  sep = " + "
)

run_activity_turb_model <- function(data, pcb_var){
  
  formula_txt <- paste0(
    "log10(", pcb_var, ") ~ ",
    "Activity + SourceWind + ",
    covars_turb
  )
  
  fit <- lm(
    as.formula(formula_txt),
    data = data
  )
  
  list(
    model = fit,
    anova = car::Anova(fit, type = 2),
    summary = summary(fit)
  )
}

south_turb <- subset(
  south_ace,
  !is.na(turb_FNU)
)

# PCB8
south_pcb8 <- run_activity_model(
  south_turb,
  "PCB8"
)

south_pcb8_turb <- run_activity_turb_model(
  south_turb,
  "PCB8"
)

AIC(
  south_pcb8$model,
  south_pcb8_turb$model
)

anova(
  south_pcb8$model,
  south_pcb8_turb$model
)

# PCB15

south_pcb15 <- run_activity_model(
  south_turb,
  "PCB15"
)

south_pcb15_turb <- run_activity_turb_model(
  south_turb,
  "PCB15"
)

AIC(
  south_pcb15$model,
  south_pcb15_turb$model
)

anova(
  south_pcb15$model,
  south_pcb15_turb$model
)

# PCB18+30

south_pcb18 <- run_activity_model(
  south_turb,
  "PCB18.30"
)

south_pcb18_turb <- run_activity_turb_model(
  south_turb,
  "PCB18.30"
)

AIC(
  south_pcb18$model,
  south_pcb18_turb$model
)

anova(
  south_pcb18$model,
  south_pcb18_turb$model
)

# PCB20+28

south_pcb20 <- run_activity_model(
  south_turb,
  "PCB20.28"
)

south_pcb20_turb <- run_activity_turb_model(
  south_turb,
  "PCB20.28"
)

AIC(
  south_pcb20$model,
  south_pcb20_turb$model
)

anova(
  south_pcb20$model,
  south_pcb20_turb$model
)

# PCB31

south_pcb31 <- run_activity_model(
  south_turb,
  "PCB31"
)

south_pcb31_turb <- run_activity_turb_model(
  south_turb,
  "PCB31"
)

AIC(
  south_pcb31$model,
  south_pcb31_turb$model
)

anova(
  south_pcb31$model,
  south_pcb31_turb$model
)

cor(
  south_ace[, c("invT", "water_temp")],
  use = "complete.obs"
)

# New model
south_compare <- south_turb %>%
  dplyr::select(
    PCB8,
    Activity,
    SourceWind,
    invT,
    wind_speed,
    sin_season,
    cos_season,
    flow_abs
  ) %>%
  na.omit()


m_nowt_cc <- lm(
  log10(PCB8) ~
    Activity +
    SourceWind +
    invT +
    wind_speed +
    sin_season +
    cos_season +
    flow_abs,
  data = south_compare
)

m_reduced_cc <- lm(
  log10(PCB8) ~
    Activity +
    SourceWind +
    invT +
    wind_speed +
    sin_season,
  data = south_compare
)

AIC(m_nowt_cc, m_reduced_cc)

anova(m_reduced_cc, m_nowt_cc)

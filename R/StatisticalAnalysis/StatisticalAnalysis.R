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

# From ng to pg
ace <- ace %>%
  mutate(across(starts_with("PCB"), ~ . / 1000))

ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Meteorological data -----------------------------------------------------
meteo.data <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")

meteo.data$date <- as.Date(meteo.data$date, origin = "1899-12-30")

meteo.data$air_temp <- meteo.data$air_temp + 273.15

meteo.data <- meteo.data %>%
  mutate(invT = 1000 / air_temp)

meteo_unique <- meteo.data[!duplicated(meteo.data$date), ]

# Activity data -----------------------------------------------------------
activity_daily <- read.csv("Data/RemediationProject/activity_daily.csv")

activity_daily$date <- as.Date(activity_daily$date)

activity_daily$Activity <- factor(activity_daily$Activity)

# Merge datasets ----------------------------------------------------------
ace <- ace %>%
  left_join(
    meteo_unique, by = "date"
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
  levels = c("NonSource", "Source"))

hs_ace$SourceWind <- factor(
  ifelse(
    between(hs_ace$wind_direction, 0, 70), "Source", "NonSource"),
  levels = c("NonSource", "Source"))

# Activity + SourceWind model ---------------------------------------------
run_activity_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ Activity + SourceWind + ",
                        "invT + wind_speed + ", "sin_season + cos_season")
  
  fit <- lm(as.formula(formula_txt), data = data)
  
  list(model = fit, anova = car::Anova(fit, type = 2),
       activity = emmeans(fit, pairwise ~ Activity),
       sourcewind = emmeans(fit, pairwise ~ SourceWind))
}

# South
south_pcb8  <- run_activity_model(south_ace, "PCB8")
south_pcb15 <- run_activity_model(south_ace, "PCB15")
south_pcb18 <- run_activity_model(south_ace, "PCB18.30")
south_pcb20 <- run_activity_model(south_ace, "PCB20.28")
south_pcb31 <- run_activity_model(south_ace, "PCB31")

# HS
hs_pcb8  <- run_activity_model(hs_ace, "PCB8")
hs_pcb15 <- run_activity_model(hs_ace, "PCB15")
hs_pcb18 <- run_activity_model(hs_ace, "PCB18.30")
hs_pcb20 <- run_activity_model(hs_ace, "PCB20.28")
hs_pcb31 <- run_activity_model(hs_ace, "PCB31")

# Dredging inventory ------------------------------------------------------
dredge_inventory <- read.csv("Data/RemediationProject/dredge_inventory.csv")

dredge_inventory$DateStart <- as.Date(dredge_inventory$DateStart,
                                      format = "%m/%d/%Y")

dredge_inventory$DateEnd <- as.Date(dredge_inventory$DateEnd,
                                    format = "%m/%d/%Y")

# Exposure_d1 -------------------------------------------------------------
# Sum(Volume / Distance)

south_ace$Exposure_d1 <- 0
hs_ace$Exposure_d1 <- 0

# South
for(i in seq_len(nrow(south_ace))) {
  
  d <- south_ace$date[i]
  
  active <- dredge_inventory[
    dredge_inventory$DateStart <= d &
      dredge_inventory$DateEnd >= d,
  ]
  
  if(nrow(active) > 0){
    
    south_ace$Exposure_d1[i] <- sum(active$Volume_yd3 / active$Dista_South_m,
                                    na.rm = TRUE)
  }
}

# HS
for(i in seq_len(nrow(hs_ace))) {
  
  d <- hs_ace$date[i]
  
  active <- dredge_inventory[
    dredge_inventory$DateStart <= d &
      dredge_inventory$DateEnd >= d,
  ]
  
  if(nrow(active) > 0){
    
    hs_ace$Exposure_d1[i] <- sum(active$Volume_yd3 / active$Dista_HS_m,
                                 na.rm = TRUE)
  }
}

# Exposure_d1 + SourceWind model ------------------------------------------
run_exposure_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ Exposure_d1 + SourceWind + ",
                        "invT + wind_speed + ", "sin_season + cos_season")
  
  fit <- lm(as.formula(formula_txt), data = data)
  
  list(model = fit, anova = car::Anova(fit, type = 2), summary = summary(fit))
}

# South
south_exp8  <- run_exposure_model(south_ace, "PCB8")
south_exp15 <- run_exposure_model(south_ace, "PCB15")
south_exp18 <- run_exposure_model(south_ace, "PCB18.30")
south_exp20 <- run_exposure_model(south_ace, "PCB20.28")
south_exp31 <- run_exposure_model(south_ace, "PCB31")

# HS
hs_exp8  <- run_exposure_model(hs_ace, "PCB8")
hs_exp15 <- run_exposure_model(hs_ace, "PCB15")
hs_exp18 <- run_exposure_model(hs_ace, "PCB18.30")
hs_exp20 <- run_exposure_model(hs_ace, "PCB20.28")
hs_exp31 <- run_exposure_model(hs_ace, "PCB31")

# Activity + SourceWind summary -------------------------------------------
south_activity_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Activity_F = c(
    south_pcb8$anova["Activity", "F value"],
    south_pcb15$anova["Activity", "F value"],
    south_pcb18$anova["Activity", "F value"],
    south_pcb20$anova["Activity", "F value"],
    south_pcb31$anova["Activity", "F value"]),
  SourceWind_F = c(
    south_pcb8$anova["SourceWind", "F value"],
    south_pcb15$anova["SourceWind", "F value"],
    south_pcb18$anova["SourceWind", "F value"],
    south_pcb20$anova["SourceWind", "F value"],
    south_pcb31$anova["SourceWind", "F value"]))

hs_activity_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Activity_F = c(
    hs_pcb8$anova["Activity", "F value"],
    hs_pcb15$anova["Activity", "F value"],
    hs_pcb18$anova["Activity", "F value"],
    hs_pcb20$anova["Activity", "F value"],
    hs_pcb31$anova["Activity", "F value"]),
  SourceWind_F = c(
    hs_pcb8$anova["SourceWind", "F value"],
    hs_pcb15$anova["SourceWind", "F value"],
    hs_pcb18$anova["SourceWind", "F value"],
    hs_pcb20$anova["SourceWind", "F value"],
    hs_pcb31$anova["SourceWind", "F value"]))

# Exposure summary --------------------------------------------------------
south_exposure_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Exposure_F = c(
    south_exp8$anova["Exposure_d1", "F value"],
    south_exp15$anova["Exposure_d1", "F value"],
    south_exp18$anova["Exposure_d1", "F value"],
    south_exp20$anova["Exposure_d1", "F value"],
    south_exp31$anova["Exposure_d1", "F value"]),
  SourceWind_F = c(
    south_exp8$anova["SourceWind", "F value"],
    south_exp15$anova["SourceWind", "F value"],
    south_exp18$anova["SourceWind", "F value"],
    south_exp20$anova["SourceWind", "F value"],
    south_exp31$anova["SourceWind", "F value"]))

hs_exposure_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Exposure_F = c(
    hs_exp8$anova["Exposure_d1", "F value"],
    hs_exp15$anova["Exposure_d1", "F value"],
    hs_exp18$anova["Exposure_d1", "F value"],
    hs_exp20$anova["Exposure_d1", "F value"],
    hs_exp31$anova["Exposure_d1", "F value"]),
  SourceWind_F = c(
    hs_exp8$anova["SourceWind", "F value"],
    hs_exp15$anova["SourceWind", "F value"],
    hs_exp18$anova["SourceWind", "F value"],
    hs_exp20$anova["SourceWind", "F value"],
    hs_exp31$anova["SourceWind", "F value"]))

# Minimum distance to active dredging -------------------------------------
south_ace$MinDist_Dredge <- NA_real_
hs_ace$MinDist_Dredge <- NA_real_

# South
for(i in seq_len(nrow(south_ace))) {
  
  d <- south_ace$date[i]
  
  active <- dredge_inventory[
    dredge_inventory$DateStart <= d &
      dredge_inventory$DateEnd >= d,
  ]
  
  if(nrow(active) > 0) {
    
    dists <- active$Dista_South_m
    
    dists <- dists[!is.na(dists)]
    
    if(length(dists) > 0) {
      
      south_ace$MinDist_Dredge[i] <- min(dists)
      
    }
  }
}

# HS
for(i in seq_len(nrow(hs_ace))) {
  
  d <- hs_ace$date[i]
  
  active <- dredge_inventory[
    dredge_inventory$DateStart <= d &
      dredge_inventory$DateEnd >= d,
  ]
  
  if(nrow(active) > 0) {
    
    dists <- active$Dista_HS_m
    
    dists <- dists[!is.na(dists)]
    
    if(length(dists) > 0) {
      
      hs_ace$MinDist_Dredge[i] <- min(dists)
      
    }
  }
}

# Safety check
south_ace$MinDist_Dredge[
  is.infinite(south_ace$MinDist_Dredge)
] <- NA

hs_ace$MinDist_Dredge[
  is.infinite(hs_ace$MinDist_Dredge)
] <- NA

# Check
summary(south_ace$MinDist_Dredge)

summary(hs_ace$MinDist_Dredge)

table(is.na(south_ace$MinDist_Dredge))

table(is.na(hs_ace$MinDist_Dredge))

# Minimum distance model --------------------------------------------------
run_distance_model <- function(data, pcb_var){
  
  formula_txt <- paste0("log10(", pcb_var, ") ~ log10(MinDist_Dredge) + ",
                        "SourceWind + ", "invT + wind_speed + ",
                        "sin_season + cos_season")
  
  fit <- lm(
    as.formula(formula_txt), data = subset(data, !is.na(MinDist_Dredge)))
  
  list(model = fit, anova = car::Anova(fit, type = 2), summary = summary(fit))
}

# South
south_dist8 <- run_distance_model(south_ace, "PCB8")
south_dist15 <- run_distance_model(south_ace, "PCB15")
south_dist18 <- run_distance_model(south_ace, "PCB18.30")
south_dist20 <- run_distance_model(south_ace, "PCB20.28")
south_dist31 <- run_distance_model(south_ace, "PCB31")

# HS
hs_dist8 <- run_distance_model(hs_ace, "PCB8")
hs_dist15 <- run_distance_model(hs_ace, "PCB15")
hs_dist18 <- run_distance_model(hs_ace, "PCB18.30")
hs_dist20 <- run_distance_model(hs_ace, "PCB20.28")
hs_dist31 <- run_distance_model(hs_ace, "PCB31")

# Distance summary tables -------------------------------------------------
south_distance_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Distance_F = c(
    south_dist8$anova["log10(MinDist_Dredge)", "F value"],
    south_dist15$anova["log10(MinDist_Dredge)", "F value"],
    south_dist18$anova["log10(MinDist_Dredge)", "F value"],
    south_dist20$anova["log10(MinDist_Dredge)", "F value"],
    south_dist31$anova["log10(MinDist_Dredge)", "F value"]),
  Distance_p = c(
    south_dist8$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    south_dist15$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    south_dist18$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    south_dist20$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    south_dist31$anova["log10(MinDist_Dredge)", "Pr(>F)"]),
  SourceWind_F = c(
    south_dist8$anova["SourceWind", "F value"],
    south_dist15$anova["SourceWind", "F value"],
    south_dist18$anova["SourceWind", "F value"],
    south_dist20$anova["SourceWind", "F value"],
    south_dist31$anova["SourceWind", "F value"]),
  SourceWind_p = c(
    south_dist8$anova["SourceWind", "Pr(>F)"],
    south_dist15$anova["SourceWind", "Pr(>F)"],
    south_dist18$anova["SourceWind", "Pr(>F)"],
    south_dist20$anova["SourceWind", "Pr(>F)"],
    south_dist31$anova["SourceWind", "Pr(>F)"]))

hs_distance_summary <- data.frame(
  Congener = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  Distance_F = c(
    hs_dist8$anova["log10(MinDist_Dredge)", "F value"],
    hs_dist15$anova["log10(MinDist_Dredge)", "F value"],
    hs_dist18$anova["log10(MinDist_Dredge)", "F value"],
    hs_dist20$anova["log10(MinDist_Dredge)", "F value"],
    hs_dist31$anova["log10(MinDist_Dredge)", "F value"]),
  Distance_p = c(
    hs_dist8$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    hs_dist15$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    hs_dist18$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    hs_dist20$anova["log10(MinDist_Dredge)", "Pr(>F)"],
    hs_dist31$anova["log10(MinDist_Dredge)", "Pr(>F)"]),
  SourceWind_F = c(
    hs_dist8$anova["SourceWind", "F value"],
    hs_dist15$anova["SourceWind", "F value"],
    hs_dist18$anova["SourceWind", "F value"],
    hs_dist20$anova["SourceWind", "F value"],
    hs_dist31$anova["SourceWind", "F value"]),
  SourceWind_p = c(
    hs_dist8$anova["SourceWind", "Pr(>F)"],
    hs_dist15$anova["SourceWind", "Pr(>F)"],
    hs_dist18$anova["SourceWind", "Pr(>F)"],
    hs_dist20$anova["SourceWind", "Pr(>F)"],
    hs_dist31$anova["SourceWind", "Pr(>F)"]))

south_distance_summary

hs_distance_summary

# plots
ggplot(
  subset(
    south_ace,
    !is.na(SourceWind)
  ),
  aes(
    SourceWind,
    log10(PCB31)
  )
) +
  geom_boxplot()

ggplot(
  subset(
    south_ace,
    !is.na(SourceWind)
  ),
  aes(
    Activity,
    log10(PCB31)
  )
) +
  geom_boxplot()


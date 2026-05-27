# Preliminary statistical analysis

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("car")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(car)
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

# Read air meteorological conditions --------------------------------------
meteo.data <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")
meteo.data$date <- as.Date(meteo.data$date, origin = "1899-12-30")
# Change to Kelvin
meteo.data$air_temp <- meteo.data$air_temp + 273.15

# Add invT
meteo.data <- meteo.data %>%
  mutate(invT = 1000 / meteo.data$air_temp)

meteo_unique <- meteo.data[!duplicated(meteo.data$date), ]

# Read daily activity -----------------------------------------------------
activity_daily <- read.csv("Data/RemediationProject/activity_daily.csv")

# Change format
activity_daily$Date <- as.Date(activity_daily$Date)
names(activity_daily)[names(activity_daily) == "Date"] <- "date"
activity_daily$Activity <- as.factor(activity_daily$Activity)
activity_unique <- activity_daily[!duplicated(activity_daily$date), ]

# Add meteorological and daily activities to ace --------------------------
ace <- ace %>%
  left_join(meteo_unique, by = "date") %>%
  left_join(activity_unique, by = "date")

ace$Activity <- relevel(
  ace$Activity,
  ref = "Idle")

# Statistical analysis: Activities ----------------------------------------
# Select locations
south_ace <- subset(ace, location == "South")
hs_ace <- subset(ace, location == "HS")

# PCB 8 -------------------------------------------------------------------
# log10 transform
south_ace$logPCB8 <- log10(south_ace$PCB8)

# Histogram
hist(south_ace$logPCB8)

# Q-Q plot
qqnorm(south_ace$logPCB8)
qqline(south_ace$logPCB8,
       col = "red")

# Normality test
by(south_ace$logPCB8,
   south_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB8 ~ Activity,
           data = south_ace)

# ANOVA
fit.pcb8 <- aov(logPCB8 ~ Activity,
           data = south_ace)

qqnorm(residuals(fit.pcb8))
qqline(residuals(fit.pcb8),
       col = "red")

summary(fit.pcb8)

# Post hoc comparisons
TukeyHSD(fit.pcb8)

# PCB 15 -------------------------------------------------------------------
# log10 transform
south_ace$logPCB15 <- log10(south_ace$PCB15)

# Histogram
hist(south_ace$logPCB15)

# Q-Q plot
qqnorm(south_ace$logPCB15)
qqline(south_ace$logPCB15,
       col = "red")

# Normality test
by(south_ace$logPCB15,
   south_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB15 ~ Activity,
           data = south_ace)

# ANOVA
fit.pcb15 <- aov(logPCB15 ~ Activity,
           data = south_ace)

qqnorm(residuals(fit.pcb15))
qqline(residuals(fit.pcb15),
       col = "red")

summary(fit.pcb15)

# Post hoc comparisons
TukeyHSD(fit.pcb15)

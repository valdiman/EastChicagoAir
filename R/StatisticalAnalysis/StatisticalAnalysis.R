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
  install.packages("car")
}

# Upload libraries
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(car)
  library(rstatix)
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

# For each location there is meteorological data, with duplicates
meteo_unique <- meteo.data[!duplicated(meteo.data$date), ]

# Read daily activity -----------------------------------------------------
activity_daily <- read.csv("Data/RemediationProject/activity_daily.csv")

# Change format
activity_daily$Date <- as.Date(activity_daily$Date)
names(activity_daily)[names(activity_daily) == "Date"] <- "date"
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

# Statistical analysis: Activities ----------------------------------------
# South -------------------------------------------------------------------
# PCB 8 -------------------------------------------------------------------
# log10 transform
south_ace$logPCB8 <- log10(south_ace$PCB8)

# Boxplot
ggplot(south_ace,
       aes(Activity, logPCB8,
           fill = Activity)) +
  geom_boxplot()

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

# Boxplot
ggplot(south_ace,
       aes(Activity, logPCB15,
           fill = Activity)) +
  geom_boxplot()

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

# PCB15 doesn't follow normal nor log10 distribution
kruskal.test(PCB15 ~ Activity,
             data = south_ace)

pairwise.wilcox.test(
  south_ace$PCB15,
  south_ace$Activity,
  p.adjust.method = "BH")

# PCB 18+30 -----------------------------------------------------------------
# log10 transform
south_ace$logPCB18.30 <- log10(south_ace$PCB18.30)

# Boxplot
ggplot(south_ace,
       aes(Activity, logPCB18.30,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(south_ace$logPCB18.30)

# Q-Q plot
qqnorm(south_ace$logPCB18.30)
qqline(south_ace$logPCB18.30,
       col = "red")

# Normality test
by(south_ace$logPCB18.30,
   south_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB18.30 ~ Activity,
           data = south_ace)

# ANOVA
fit.pcb18.30 <- aov(logPCB18.30 ~ Activity,
                data = south_ace)

qqnorm(residuals(fit.pcb18.30))
qqline(residuals(fit.pcb18.30),
       col = "red")

summary(fit.pcb18.30)

# Post hoc comparisons
TukeyHSD(fit.pcb18.30)

# PCB 20 + 28 -----------------------------------------------------------------
# log10 transform
south_ace$logPCB20.28 <- log10(south_ace$PCB20.28)

# Boxplot
ggplot(south_ace,
       aes(Activity, logPCB20.28,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(south_ace$logPCB20.28)

# Q-Q plot
qqnorm(south_ace$logPCB20.28)
qqline(south_ace$logPCB20.28,
       col = "red")

# Normality test
by(south_ace$logPCB20.28,
   south_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB20.28 ~ Activity,
           data = south_ace)

# ANOVA
fit.pcb20.28 <- aov(logPCB20.28 ~ Activity,
                    data = south_ace)

qqnorm(residuals(fit.pcb20.28))
qqline(residuals(fit.pcb20.28),
       col = "red")

summary(fit.pcb20.28)

# Post hoc comparisons
TukeyHSD(fit.pcb20.28)

# PCB 31 -----------------------------------------------------------------
# log10 transform
south_ace$logPCB31 <- log10(south_ace$PCB31)

# Boxplot
ggplot(south_ace,
       aes(Activity, logPCB31,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(south_ace$logPCB31)

# Q-Q plot
qqnorm(south_ace$logPCB31)
qqline(south_ace$logPCB31,
       col = "red")

# Normality test
by(south_ace$logPCB31,
   south_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB31 ~ Activity,
           data = south_ace)

# ANOVA
fit.pcb31 <- aov(logPCB31 ~ Activity,
                    data = south_ace)

qqnorm(residuals(fit.pcb31))
qqline(residuals(fit.pcb31),
       col = "red")

summary(fit.pcb31)

# Post hoc comparisons
TukeyHSD(fit.pcb31)

# HS -------------------------------------------------------------------
# PCB 8 -------------------------------------------------------------------
# log10 transform
hs_ace$logPCB8 <- log10(hs_ace$PCB8)

# Boxplot
ggplot(hs_ace,
       aes(Activity, logPCB8,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace$logPCB8)

# Q-Q plot
qqnorm(hs_ace$logPCB8)
qqline(hs_ace$logPCB8,
       col = "red")

# Normality test
by(hs_ace$logPCB8,
   hs_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB8 ~ Activity,
           data = hs_ace)

# Welch ANOVA + Games-Howell
fit.pcb8 <- oneway.test(logPCB8 ~ Activity,
                        data = hs_ace,
                        var.equal = FALSE)

fit.pcb8

# Post hoc comparisons
games_howell_test(
  hs_ace,
  logPCB8 ~ Activity)

# PCB 15 -------------------------------------------------------------------
# log10 transform
hs_ace$logPCB15 <- log10(hs_ace$PCB15)

# Boxplot
ggplot(hs_ace,
       aes(Activity, logPCB15,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace$logPCB15)

# Q-Q plot
qqnorm(hs_ace$logPCB15)
qqline(hs_ace$logPCB15,
       col = "red")

# Normality test
by(hs_ace$logPCB15,
   hs_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB15 ~ Activity,
           data = hs_ace)

# Welch ANOVA + Games-Howell
fit.pcb15 <- oneway.test(logPCB15 ~ Activity,
                        data = hs_ace,
                        var.equal = FALSE)

fit.pcb15

# Post hoc comparisons
games_howell_test(
  hs_ace,
  logPCB15 ~ Activity)

# PCB 18.30 -------------------------------------------------------------------
# log10 transform
hs_ace$logPCB18.30 <- log10(hs_ace$PCB18.30)

# Boxplot
ggplot(hs_ace,
       aes(Activity, logPCB18.30,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace$logPCB18.30)

# Q-Q plot
qqnorm(hs_ace$logPCB18.30)
qqline(hs_ace$logPCB18.30,
       col = "red")

# Normality test
by(hs_ace$logPCB18.30,
   hs_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB18.30 ~ Activity,
           data = hs_ace)

# Welch ANOVA + Games-Howell
fit.pcb18.30 <- oneway.test(logPCB18.30 ~ Activity,
                         data = hs_ace,
                         var.equal = FALSE)

fit.pcb18.30

# Post hoc comparisons
games_howell_test(
  hs_ace,
  logPCB18.30 ~ Activity)

# PCB 20.28 -------------------------------------------------------------------
# log10 transform
hs_ace$logPCB20.28 <- log10(hs_ace$PCB20.28)

# Boxplot
ggplot(hs_ace,
       aes(Activity, logPCB20.28,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace$logPCB20.28)

# Q-Q plot
qqnorm(hs_ace$logPCB20.28)
qqline(hs_ace$logPCB20.28,
       col = "red")

# Normality test
by(hs_ace$logPCB20.28,
   hs_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB20.28 ~ Activity,
           data = hs_ace)

# Welch ANOVA + Games-Howell
fit.pcb20.28 <- oneway.test(logPCB20.28 ~ Activity,
                            data = hs_ace,
                            var.equal = FALSE)

fit.pcb20.28

# Post hoc comparisons
games_howell_test(
  hs_ace,
  logPCB20.28 ~ Activity)

# PCB 31 -------------------------------------------------------------------
# log10 transform
hs_ace$logPCB31 <- log10(hs_ace$PCB31)

# Boxplot
ggplot(hs_ace,
       aes(Activity, logPCB31,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace$logPCB31)

# Q-Q plot
qqnorm(hs_ace$logPCB31)
qqline(hs_ace$logPCB31,
       col = "red")

# Normality test
by(hs_ace$logPCB31,
   hs_ace$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB31 ~ Activity,
           data = hs_ace)

# Welch ANOVA + Games-Howell
fit.pcb31 <- oneway.test(logPCB31 ~ Activity,
                            data = hs_ace,
                            var.equal = FALSE)

fit.pcb31

# Post hoc comparisons
games_howell_test(
  hs_ace,
  logPCB31 ~ Activity)

# Include wind direction in analysis --------------------------------------
hs_ace_w <- hs_ace %>%
  filter(
    Activity != "Dredging" |
      between(wind_direction, 30, 120))

# PCB 8 -------------------------------------------------------------------
# log10 transform
hs_ace_w$logPCB8 <- log10(hs_ace_w$PCB8)

# Boxplot
ggplot(hs_ace_w,
       aes(Activity, logPCB8,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace_w$logPCB8)

# Q-Q plot
qqnorm(hs_ace_w$logPCB8)
qqline(hs_ace_w$logPCB8,
       col = "red")

# Normality test
by(hs_ace_w$logPCB8,
   hs_ace_w$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB8 ~ Activity,
           data = hs_ace_w)

# Welch ANOVA + Games-Howell
fit.pcb8 <- oneway.test(logPCB8 ~ Activity,
                        data = hs_ace_w,
                        var.equal = FALSE)

fit.pcb8

# Post hoc comparisons
games_howell_test(
  hs_ace_w,
  logPCB8 ~ Activity)

# PCB 15 -------------------------------------------------------------------
# log10 transform
hs_ace_w$logPCB15 <- log10(hs_ace_w$PCB15)

# Boxplot
ggplot(hs_ace_w,
       aes(Activity, logPCB15,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace_w$logPCB15)

# Q-Q plot
qqnorm(hs_ace_w$logPCB15)
qqline(hs_ace_w$logPCB15,
       col = "red")

# Normality test
by(hs_ace_w$logPCB15,
   hs_ace_w$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB15 ~ Activity,
           data = hs_ace_w)

# Welch ANOVA + Games-Howell
fit.pcb15 <- oneway.test(logPCB15 ~ Activity,
                        data = hs_ace_w,
                        var.equal = FALSE)

fit.pcb15

# Post hoc comparisons
games_howell_test(
  hs_ace_w,
  logPCB15 ~ Activity)

# PCB 18+30 -----------------------------------------------------------------
# log10 transform
hs_ace_w$logPCB18.30 <- log10(hs_ace_w$PCB18.30)

# Boxplot
ggplot(hs_ace_w,
       aes(Activity, logPCB18.30,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace_w$logPCB18.30)

# Q-Q plot
qqnorm(hs_ace_w$logPCB18.30)
qqline(hs_ace_w$logPCB18.30,
       col = "red")

# Normality test
by(hs_ace_w$logPCB18.30,
   hs_ace_w$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB18.30 ~ Activity,
           data = hs_ace_w)

# Welch ANOVA + Games-Howell
fit.pcb18.30 <- oneway.test(logPCB18.30 ~ Activity,
                         data = hs_ace_w,
                         var.equal = FALSE)

fit.pcb18.30

# Post hoc comparisons
games_howell_test(
  hs_ace_w,
  logPCB18.30 ~ Activity)

# PCB 20 + 28 -----------------------------------------------------------------
# log10 transform
hs_ace_w$logPCB20.28 <- log10(hs_ace_w$PCB20.28)

# Boxplot
ggplot(hs_ace_w,
       aes(Activity, logPCB20.28,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace_w$logPCB20.28)

# Q-Q plot
qqnorm(hs_ace_w$logPCB20.28)
qqline(hs_ace_w$logPCB20.28,
       col = "red")

# Normality test
by(hs_ace_w$logPCB20.28,
   hs_ace_w$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB20.28 ~ Activity,
           data = hs_ace_w)

# Welch ANOVA + Games-Howell
fit.pcb20.28 <- oneway.test(logPCB20.28 ~ Activity,
                            data = hs_ace_w,
                            var.equal = FALSE)

fit.pcb20.28

# Post hoc comparisons
# group2 - group1
games_howell_test(
  hs_ace_w,
  logPCB20.28 ~ Activity)

# PCB 31 -----------------------------------------------------------------
# log10 transform
hs_ace_w$logPCB31 <- log10(hs_ace_w$PCB31)

# Boxplot
ggplot(hs_ace_w,
       aes(Activity, logPCB31,
           fill = Activity)) +
  geom_boxplot()

# Histogram
hist(hs_ace_w$logPCB31)

# Q-Q plot
qqnorm(hs_ace_w$logPCB31)
qqline(hs_ace_w$logPCB31,
       col = "red")

# Normality test
by(hs_ace_w$logPCB31,
   hs_ace_w$Activity,
   shapiro.test)

# Normality test
leveneTest(logPCB31 ~ Activity,
           data = hs_ace_w)

# Welch ANOVA + Games-Howell
fit.pcb31 <- oneway.test(logPCB31 ~ Activity,
                            data = hs_ace_w,
                            var.equal = FALSE)

fit.pcb31

# Post hoc comparisons
# group2 - group1
games_howell_test(
  hs_ace_w,
  logPCB31 ~ Activity)




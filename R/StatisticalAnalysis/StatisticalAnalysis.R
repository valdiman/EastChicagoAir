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
south_ace_w <- south_ace %>%
  filter(
    Activity != "Dredging" |
      between(wind_direction, 30, 120))

hs_ace_w <- hs_ace %>%
  filter(
    Activity != "Dredging" |
      between(wind_direction, 30, 120))

# Statistical analysis: Activities ----------------------------------------
# Function
run_pcb_analysis <- function(data, pcb, dataset_name) {
  
  # Output directory
  output_dir <- file.path("Output/Data/Air/StatisticalAnalysis", pcb)
  # Log transform
  data$logPCB <- log10(data[[pcb]])
  
  # Assumption tests
  shapiro_results <- by(
    data$logPCB,
    data$Activity,
    shapiro.test)
  
  levene_results <- leveneTest(
    logPCB ~ Activity,
    data = data)
  
  # Classical ANOVA + Tukey
  fit_aov <- aov(
    logPCB ~ Activity,
    data = data)
  anova_results <- summary(fit_aov)
  tukey_results <- TukeyHSD(fit_aov)
  tukey_table <- as.data.frame(
    tukey_results$Activity)
  tukey_table$Comparison <- rownames(
    tukey_table)
  rownames(tukey_table) <- NULL
  
  # Welch ANOVA + Games-Howell
  fit_welch <- oneway.test(
    logPCB ~ Activity,
    data = data,
    var.equal = FALSE)
  gh_results <- games_howell_test(
    data,
    logPCB ~ Activity)
  gh_results$fold_change <- 10^(gh_results$estimate)
  
  # Kruskal-Wallis + Dunn
  kw_results <- kruskal.test(
    logPCB ~ Activity,
    data = data)
  dunn_results <- dunn_test(
    data,
    logPCB ~ Activity,
    p.adjust.method = "holm")
  
  # Summary table
  summary_table <- data.frame(
    Dataset = dataset_name,
    PCB = pcb,
    
    Levene_F = levene_results$`F value`[1],
    Levene_p = levene_results$`Pr(>F)`[1],
    
    ANOVA_F = anova_results[[1]]$`F value`[1],
    ANOVA_p = anova_results[[1]]$`Pr(>F)`[1],
    
    Welch_F = unname(fit_welch$statistic),
    Welch_p = fit_welch$p.value,
    
    Kruskal_ChiSq = unname(kw_results$statistic),
    Kruskal_p = kw_results$p.value)
  
  # Save outputs
  write.csv(tukey_table,
            file.path(output_dir,
                      paste0("02_", dataset_name, "_Tukey.csv")),
            row.names = FALSE)
  write.csv(gh_results,
            file.path(output_dir,
                      paste0("03_", dataset_name, "_GamesHowell.csv")),
            row.names = FALSE)
  
  write.csv(dunn_results,
            file.path(output_dir,
                      paste0("04_", dataset_name, "_Dunn.csv")),
            row.names = FALSE)
  
  write.csv(summary_table,
            file.path(output_dir,
                      paste0("01_", dataset_name, "_Summary.csv")),
            row.names = FALSE)
  
  # Return results
  results <- list(
    pcb = pcb,
    dataset = dataset_name,
    
    shapiro = shapiro_results,
    levene = levene_results,
    
    anova = fit_aov,
    anova_summary = anova_results,
    tukey = tukey_results,
    
    welch = fit_welch,
    games_howell = gh_results,
    
    kruskal = kw_results,
    dunn = dunn_results)
  
  return(results)
}

# PCB8
pcb8_south <- run_pcb_analysis(south_ace, "PCB8", "South")
pcb8_south_w <- run_pcb_analysis(south_ace_w, "PCB8", "South_Wind")
pcb8_hs <- run_pcb_analysis(hs_ace, "PCB8", "HS")
pcb8_hs_w <- run_pcb_analysis(hs_ace_w, "PCB8", "HS_Wind")

# PCB15
pcb15_south <- run_pcb_analysis(south_ace, "PCB15", "South")
pcb15_south_w <- run_pcb_analysis(south_ace_w, "PCB15", "South_Wind")
pcb15_hs <- run_pcb_analysis(hs_ace, "PCB15", "HS")
pcb15_hs_w <- run_pcb_analysis(hs_ace_w, "PCB15", "HS_Wind")

# PCB18+30
pcb18_south <- run_pcb_analysis(south_ace, "PCB18.30", "South")
pcb18_south_w <- run_pcb_analysis(south_ace_w, "PCB18.30", "South_Wind")
pcb18_hs <- run_pcb_analysis(hs_ace, "PCB18.30", "HS")
pcb18_hs_w <- run_pcb_analysis(hs_ace_w, "PCB18.30", "HS_Wind")

# PCB20+28
pcb20_south <- run_pcb_analysis(south_ace, "PCB20.28", "South")
pcb20_south_w <- run_pcb_analysis(south_ace_w, "PCB20.28", "South_Wind")
pcb20_hs <- run_pcb_analysis(hs_ace, "PCB20.28", "HS")
pcb20_hs_w <- run_pcb_analysis(hs_ace_w, "PCB20.28", "HS_Wind")

# PCB31
pcb31_south <- run_pcb_analysis(south_ace, "PCB31", "South")
pcb31_south_w <- run_pcb_analysis(south_ace_w, "PCB31", "South_Wind")
pcb31_hs <- run_pcb_analysis(hs_ace, "PCB31", "HS")
pcb31_hs_w <- run_pcb_analysis(hs_ace_w, "PCB31", "HS_Wind")

# Statistical analysis: Temporal ------------------------------------------
m0 <- lm(
  log10(PCB8) ~ Activity,
  data = south_ace
)

summary(m0)

m1 <- lm(
  log10(PCB8) ~
    Activity +
    invT +
    wind_speed,
  data = south_ace
)

# Intercept                = Idle (reference)
# ActivityConstruction     = Construction - Idle
# ActivityDredging         = Dredging - Idle


summary(m1)

south_ace$julian_day <-
  as.numeric(format(south_ace$date, "%j"))

z <- 2*pi/365.25

south_ace$sin_season <- sin(z * south_ace$julian_day)
south_ace$cos_season <- cos(z * south_ace$julian_day)

m2 <- lm(
  log10(PCB8) ~
    Activity +
    invT +
    wind_speed +
    sin_season +
    cos_season,
  data = south_ace
)

AIC(m1, m2)

anova(m1, m2)

summary(m2)$coefficients

par(mfrow = c(2,2))
plot(m2)

library(emmeans)

emmeans(m2, ~ Activity)

pairs(
  emmeans(m2, ~ Activity)
)

south_ace$upwind <-
  south_ace$wind_direction >= 30 &
  south_ace$wind_direction <= 120

m3 <- lm(
  log10(PCB8) ~
    Activity * upwind +
    invT +
    wind_speed +
    sin(z * julian_day) +
    cos(z * julian_day),
  data = south_ace
)





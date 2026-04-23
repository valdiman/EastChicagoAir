# Code to normalize ACC PCB concentation to 288 K

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
}

# Read data ---------------------------------------------------------------
ace <- read.csv("Data/Air/EastChicago/ACE/ACEData.csv")

# ACE Data ----------------------------------------------------------------
# Remove blanks cells (0s)
ace <- subset(ace, !grepl("0", location))
# Change units to pg/m3 from ng/m3
ace <- ace %>%
  mutate(across(starts_with("PCB") & !ends_with("_unc"), ~ . / 1000))
ace <- ace %>%
  mutate(location = factor(location,levels = c("South", "South_CDF", "HS","Con_South")))  # Explicit factor levels
# Convert numeric date to Date format assuming Excel-style serial number
# ccvs file needs to be this format for the date XXXXX (e.g., 41188)
ace$date <- as.Date(ace$date, origin = "1899-12-30")
ace <- ace %>%
  mutate(
    PCB8_unc_label = factor(PCB8_unc, labels = c("≤ DL", "> DL")),
    PCB15_unc_label = factor(PCB15_unc, labels = c("≤ DL", "> DL")),
    PCB18.30_unc_label = factor(PCB18.30_unc, labels = c("≤ DL", "> DL")),
    PCB20.28_unc_label = factor(PCB20.28_unc, labels = c("≤ DL", "> DL")),
    PCB31_unc_label = factor(PCB31_unc, labels = c("≤ DL", "> DL"))
  )

# Both sites --------------------------------------------------------------
# Combining South locations into one
ace$location2 <- ifelse(grepl("South", ace$location), "South", "HS")
ace$location2 <- factor(ace$location2)

# Read air temperature
air.temp <- read.csv("Data/Meteorology/Meteo_EastChicago.csv")
air.temp <- air.temp[, 1:2]
# Change to Kelvin
air.temp$air_temp <- air.temp$air_temp + 273.15

# Transform concentration to partial pressure -----------------------------
mw.pcb <- data.frame(
  PCB = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  mw = c(223.088, 223.088, 257.532, 257.532, 257.532)
)

# Calculate Pp for each PCB
# Check order
all(ace$date == air.temp$date)

R <- 8.2057e-5
mw_vec <- setNames(mw.pcb$mw, mw.pcb$PCB)

ace_pp <- ace %>%
  mutate(
    air_temp = air.temp$air_temp,
    PCB8     = (PCB8     * 1e-9 / mw_vec["PCB8"])     * R * air_temp,
    PCB15    = (PCB15    * 1e-9 / mw_vec["PCB15"])    * R * air_temp,
    PCB18.30 = (PCB18.30 * 1e-9 / mw_vec["PCB18.30"]) * R * air_temp,
    PCB20.28 = (PCB20.28 * 1e-9 / mw_vec["PCB20.28"]) * R * air_temp,
    PCB31    = (PCB31    * 1e-9 / mw_vec["PCB31"])    * R * air_temp
  ) %>%
  select(date, PCB8, PCB15, PCB18.30, PCB20.28, PCB31)

# Plots both sites
df <- ace_pp %>%
  mutate(invT = 1000 / air.temp$air_temp)

fit.pcb8 <- lm(log(PCB8) ~ invT, data = df)
summary(fit.pcb8)

r2.pcb8 <- summary(fit.pcb8)$r.squared
pval.pcb8 <- summary(fit.pcb8)$coefficients[2, 4]

ggplot(df, aes(x = invT, y = log(PCB8))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text",
           x = min(df$invT),
           y = max(log(df$PCB8) + 0.4, na.rm = TRUE),
           hjust = 0,
           label = paste0("R² = ", round(r2.pcb8, 3),
                          "\np = ", signif(pval.pcb8, 3))) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB8)") +
  theme_minimal()

fit.pcb15 <- lm(log(PCB15) ~ invT, data = df)
summary(fit.pcb15)

r2.pcb15 <- summary(fit.pcb15)$r.squared
pval.pcb15 <- summary(fit.pcb15)$coefficients[2, 4]

ggplot(df, aes(x = invT, y = log(PCB15))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text",
           x = min(df$invT),
           y = max(log(df$PCB15) + 0.4, na.rm = TRUE),
           hjust = 0,
           label = paste0("R² = ", round(r2.pcb15, 3),
                          "\np = ", signif(pval.pcb15, 3))) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB15)") +
  theme_minimal()

fit.pcb20 <- lm(log(PCB20.28) ~ invT, data = df)
summary(fit.pcb20)

r2.pcb20 <- summary(fit.pcb20)$r.squared
pval.pcb20 <- summary(fit.pcb20)$coefficients[2, 4]

ggplot(df, aes(x = invT, y = log(PCB20.28))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text",
           x = min(df$invT),
           y = max(log(df$PCB20.28) + 0.4, na.rm = TRUE),
           hjust = 0,
           label = paste0("R² = ", round(r2.pcb20, 3),
                          "\np = ", signif(pval.pcb20, 3))) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB20+28)") +
  theme_minimal()

fit.pcb31 <- lm(log(PCB31) ~ invT, data = df)
summary(fit.pcb31)

r2.pcb31 <- summary(fit.pcb31)$r.squared
pval.pcb31 <- summary(fit.pcb31)$coefficients[2, 4]

ggplot(df, aes(x = invT, y = log(PCB31))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text",
           x = min(df$invT),
           y = max(log(df$PCB31) + 0.4, na.rm = TRUE),
           hjust = 0,
           label = paste0("R² = ", round(r2.pcb31, 3),
                          "\np = ", signif(pval.pcb31, 3))) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB31)") +
  theme_minimal()

# Plots for each site
df$location <- ace$location2

df2 <- df %>%
  filter(location %in% c("HS", "South")) %>%
  mutate(location = factor(location, levels = c("HS", "South")))

# PCB 8
stats <- df2 %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB8) ~ invT, data = .x)
    s <- summary(fit)
    
    tibble(
      r2 = s$r.squared,
      pval = s$coefficients[2, 4],
      slope = s$coefficients[2, 1],
      label = sprintf("β = %.2f\nR² = %.3f\np = %.2e",
                      s$coefficients[2, 1],
                      s$r.squared,
                      s$coefficients[2, 4]),
      x = min(.x$invT),
      y = max(log(.x$PCB8), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB8), na.rm = TRUE) - min(log(.x$PCB8), na.rm = TRUE))
    )
  }) %>%
  ungroup()

ggplot(df2, aes(x = invT, y = log(PCB8))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -29)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB8)") +
  theme_minimal()

# PCB 15
stats <- df2 %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB15) ~ invT, data = .x)
    s <- summary(fit)
    
    tibble(
      r2 = s$r.squared,
      pval = s$coefficients[2, 4],
      slope = s$coefficients[2, 1],
      label = sprintf("β = %.2f\nR² = %.3f\np = %.2e",
                      s$coefficients[2, 1],
                      s$r.squared,
                      s$coefficients[2, 4]),
      x = min(.x$invT),
      y = max(log(.x$PCB15), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB15), na.rm = TRUE) - min(log(.x$PCB15), na.rm = TRUE))
    )
  }) %>%
  ungroup()

ggplot(df2, aes(x = invT, y = log(PCB15))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -30.5)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 15)") +
  theme_minimal()

# PCB 20+28
stats <- df2 %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB20.28) ~ invT, data = .x)
    s <- summary(fit)
    
    tibble(
      r2 = s$r.squared,
      pval = s$coefficients[2, 4],
      slope = s$coefficients[2, 1],
      label = sprintf("β = %.2f\nR² = %.3f\np = %.2e",
                      s$coefficients[2, 1],
                      s$r.squared,
                      s$coefficients[2, 4]),
      x = min(.x$invT),
      y = max(log(.x$PCB8), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB20.28), na.rm = TRUE) - min(log(.x$PCB20.28), na.rm = TRUE))
    )
  }) %>%
  ungroup()

ggplot(df2, aes(x = invT, y = log(PCB20.28))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -28)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 20+28)") +
  theme_minimal()

# PCB 31
stats <- df2 %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB31) ~ invT, data = .x)
    s <- summary(fit)
    
    tibble(
      r2 = s$r.squared,
      pval = s$coefficients[2, 4],
      slope = s$coefficients[2, 1],
      label = sprintf("β = %.2f\nR² = %.3f\np = %.2e",
                      s$coefficients[2, 1],
                      s$r.squared,
                      s$coefficients[2, 4]),
      x = min(.x$invT),
      y = max(log(.x$PCB31), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB31), na.rm = TRUE) - min(log(.x$PCB31), na.rm = TRUE))
    )
  }) %>%
  ungroup()

ggplot(df2, aes(x = invT, y = log(PCB31))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -29)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 31)") +
  theme_minimal()

# Transform slope to log10 to comparison purposes
slope_log10 <- stats$slope / log(10)

# Plot time series



# Temperature normalization -----------------------------------------------
# Beta 4 from eq. 1 Martinez et al., Intracity occurrence and distribution
# of airborne PCB congeners in Chicago. Science of total environment.
# https://doi.org/10.1016/j.scitotenv.2021.151505

beta.4 <- data.frame(
  PCB = c("PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31"),
  beta.4 = c(-1.8, -1.9, -1.7, -1.8, -1.9)
)

beta_vec <- setNames(beta.4$beta.4, beta.4$PCB)

T <- air.temp$air_temp

ace_pp_288 <- ace_pp %>%
  mutate(
    PCB8_288     = PCB8     * 10^(beta_vec["PCB8"]     * 1000 * (1/288 - 1/T)),
    PCB15_288    = PCB15    * 10^(beta_vec["PCB15"]    * 1000 * (1/288 - 1/T)),
    PCB18.30_288 = PCB18.30 * 10^(beta_vec["PCB18.30"] * 1000 * (1/288 - 1/T)),
    PCB20.28_288 = PCB20.28 * 10^(beta_vec["PCB20.28"] * 1000 * (1/288 - 1/T)),
    PCB31_288    = PCB31    * 10^(beta_vec["PCB31"]    * 1000 * (1/288 - 1/T))
  )


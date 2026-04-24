# Partial pressure calculations, temperature effects, and 288 K normalization

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("lubridate")
  install.packages("fuzzyjoin")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(fuzzyjoin)
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
# Check order btw concentration and meteorology data
all(ace$date == air.temp$date)

R <- 8.2057e-5 # [m3/mol/K]
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

# Plots for each site
# Add 1/temp to ace_pp
ace_pp_temp <- ace_pp %>%
  mutate(invT = 1000 / air.temp$air_temp)

# Add locations to ace_pp_temp
ace_pp_temp$location <- ace$location2

# PCB 8
stats <- ace_pp_temp %>%
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

p.pp.pcb8 <- ggplot(ace_pp_temp, aes(x = invT, y = log(PCB8))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -30)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 8)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb8

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB8_Pp_CDF_HS.png", plot = p.pp.pcb8,
       width = 12, height = 6, dpi = 500)

# PCB 15
stats <- ace_pp_temp %>%
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

p.pp.pcb15 <- ggplot(ace_pp_temp, aes(x = invT, y = log(PCB15))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1) +
  coord_cartesian(ylim = c(-37, -31)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 15)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb15

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB15_Pp_CDF_HS.png", plot = p.pp.pcb15,
       width = 12, height = 6, dpi = 500)

# PCB 18.30
stats <- ace_pp_temp %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB18.30) ~ invT, data = .x)
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
      y = max(log(.x$PCB18.30), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB18.30), na.rm = TRUE) - min(log(.x$PCB18.30), na.rm = TRUE))
    )
  }) %>%
  ungroup()

p.pp.pcb18 <- ggplot(ace_pp_temp, aes(x = invT, y = log(PCB18.30))) +
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
  labs(x = "1000 / T (1/K)", y = "ln(PCB 18+30)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb18

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB18_Pp_CDF_HS.png", plot = p.pp.pcb18,
       width = 12, height = 6, dpi = 500)

# PCB 20+28
stats <- ace_pp_temp %>%
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
      y = max(log(.x$PCB20.28), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB20.28), na.rm = TRUE) - min(log(.x$PCB20.28), na.rm = TRUE))
    )
  }) %>%
  ungroup()

p.pp.pcb20 <- ggplot(ace_pp_temp, aes(x = invT, y = log(PCB20.28))) +
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
  labs(x = "1000 / T (1/K)", y = "ln(PCB 20+28)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb20

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB2028_Pp_CDF_HS.png", plot = p.pp.pcb20,
       width = 12, height = 6, dpi = 500)

# PCB 31
stats <- ace_pp_temp %>%
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

p.pp.pcb31 <- ggplot(ace_pp_temp, aes(x = invT, y = log(PCB31))) +
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
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb31

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB31_Pp_CDF_HS.png", plot = p.pp.pcb31,
       width = 12, height = 6, dpi = 500)

# Transform slope to log10 to comparison purposes
slope_log10 <- stats$slope / log(10)

# Same analyzis but not during any activities
act.data <- read.csv("Data/RemediationProject/activities_distance.csv")

act.data <- act.data %>%
  mutate(
    DateStart = mdy(DateStart),
    DateEnd   = mdy(DateEnd))

# Need to select observations when no activities were reported
act_idle <- act.data %>%
  filter(Activity == "Idle")

act_active <- act.data %>%
  filter(Activity != "Idle")

idle_data <- ace_pp_temp %>%
  rowwise() %>%
  filter(
    any(date >= act_idle$DateStart & date <= act_idle$DateEnd)
  ) %>%
  ungroup()

idle_data <- idle_data %>%
  rowwise() %>%
  filter(
    !any(date >= act_active$DateStart & date <= act_active$DateEnd)
  ) %>%
  ungroup()

# Plots
# PCB8
stats <- idle_data %>%
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

p.pp.pcb8 <- ggplot(idle_data, aes(x = invT, y = log(PCB8))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1
  ) +
  coord_cartesian(ylim = c(-37, -30)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 8)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb8

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB8_Pp_CDF_HS_NoActivities.png", plot = p.pp.pcb8,
       width = 12, height = 6, dpi = 500)

# PCB 15
stats <- idle_data %>%
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

p.pp.pcb15 <- ggplot(idle_data, aes(x = invT, y = log(PCB15))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1
  ) +
  coord_cartesian(ylim = c(-37, -33)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 15)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb15

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB15_Pp_CDF_HS_NoActivities.png", plot = p.pp.pcb15,
       width = 12, height = 6, dpi = 500)

# PCB 18.30
stats <- idle_data %>%
  group_by(location) %>%
  group_modify(~{
    fit <- lm(log(PCB18.30) ~ invT, data = .x)
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
      y = max(log(.x$PCB18.30), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB18.30), na.rm = TRUE) - min(log(.x$PCB18.30), na.rm = TRUE))
    )
  }) %>%
  ungroup()

p.pp.pcb18 <- ggplot(idle_data, aes(x = invT, y = log(PCB18.30))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1
  ) +
  coord_cartesian(ylim = c(-37, -30)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 18+30)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb18

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB18.30_Pp_CDF_HS_NoActivities.png", plot = p.pp.pcb18,
       width = 12, height = 6, dpi = 500)

# PCB 20.28
stats <- idle_data %>%
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
      y = max(log(.x$PCB20.28), na.rm = TRUE) -
        0.05 * (max(log(.x$PCB20.28), na.rm = TRUE) - min(log(.x$PCB20.28), na.rm = TRUE))
    )
  }) %>%
  ungroup()

p.pp.pcb20 <- ggplot(idle_data, aes(x = invT, y = log(PCB20.28))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1
  ) +
  coord_cartesian(ylim = c(-37, -31.5)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 20+28)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb20

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB20.28_Pp_CDF_HS_NoActivities.png", plot = p.pp.pcb20,
       width = 12, height = 6, dpi = 500)

# PCB 31
stats <- idle_data %>%
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

p.pp.pcb31 <- ggplot(idle_data, aes(x = invT, y = log(PCB31))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location) +
  geom_text(
    data = stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1
  ) +
  coord_cartesian(ylim = c(-37, -31.5)) +
  labs(x = "1000 / T (1/K)", y = "ln(PCB 31)") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))

# see plot
p.pp.pcb31

# save plot
ggsave("Output/Plots/PartialPressure/AcePCB31_Pp_CDF_HS_NoActivities.png", plot = p.pp.pcb31,
       width = 12, height = 6, dpi = 500)







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


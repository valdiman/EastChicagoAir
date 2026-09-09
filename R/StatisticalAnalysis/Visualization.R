# Data visualization 

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("tidyr")
  install.packages("plotly")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(plotly) # 3D plots
}

# Read data ---------------------------------------------------------------
dataset <- read.csv("Data/FinalDataset/DatasetV03.csv")

# Format data -------------------------------------------------------------
dataset <- dataset %>%
  mutate(
    date = as.Date(date),
    activity = factor(activity, levels = c("Idle", "Construction", "Dredging")),
    HistoricalSourceWind_South = factor(HistoricalSourceWind_South,
                                        levels = c("NonSource", "Source")),
    HistoricalSourceWind_HS = factor(HistoricalSourceWind_HS,
                                     levels = c("NonSource", "Source")),
    ConstructionSourceWind_South = factor(ConstructionSourceWind_South,
                                          levels = c("NoConstruction", "NonSource", "Source")),
    ConstructionSourceWind_HS = factor(ConstructionSourceWind_HS,
                                       levels = c("NoConstruction", "NonSource", "Source")),
    DredgingSourceWind_South = factor(DredgingSourceWind_South,
                                      levels = c("NoDredging", "NonSource", "Source")),
    DredgingSourceWind_HS = factor(DredgingSourceWind_HS,
                                   levels = c("NoDredging", "NonSource", "Source"))
  )


# 2-D plots ---------------------------------------------------------------
plot_dat <- dataset %>%
  select(date, activity, PCB8_South, PCB8_HS) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = recode(
      location,
      PCB8_South = "South",
      PCB8_HS = "HS"
    )
  ) %>%
  filter(!is.na(PCB))

ggplot(plot_dat,
       aes(x = activity, y = PCB)) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.55,
    size = 2
  ) +
  facet_wrap(~ location) +
  scale_y_log10() +
  labs(
    x = "Activity",
    y = "PCB8 concentration") +
  theme_bw(base_size = 13)

plot2_dat <- dataset %>%
  select(
    date,
    activity,
    PCB8_South,
    PCB8_HS,
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS
  ) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    HistoricalSourceWind = case_when(
      location == "PCB8_South" ~ as.character(HistoricalSourceWind_South),
      location == "PCB8_HS"    ~ as.character(HistoricalSourceWind_HS)
    ),
    location = recode(
      location,
      PCB8_South = "South",
      PCB8_HS = "HS"
    )
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(HistoricalSourceWind)
  )

ggplot(
  plot2_dat,
  aes(
    x = activity,
    y = PCB,
    fill = HistoricalSourceWind
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = HistoricalSourceWind),
    position = position_jitterdodge(
      jitter.width = 0.10,
      dodge.width = 0.8
    ),
    alpha = 0.45,
    size = 1.7
  ) +
  facet_wrap(~ location) +
  scale_y_log10() +
  labs(
    x = "Activity",
    y = "PCB8 concentration",
    fill = "Historical source wind",
    color = "Historical source wind") +
  theme_bw(base_size = 13)

plot3_dat <- dataset %>%
  select(
    date,
    activity,
    wind_speed,
    PCB8_South,
    PCB8_HS,
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS
  ) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    HistoricalSourceWind = case_when(
      location == "PCB8_South" ~ as.character(HistoricalSourceWind_South),
      location == "PCB8_HS"    ~ as.character(HistoricalSourceWind_HS)
    ),
    location = recode(
      location,
      PCB8_South = "South",
      PCB8_HS = "HS"
    )
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(wind_speed),
    !is.na(HistoricalSourceWind)
  )

ggplot(
  plot3_dat,
  aes(
    x = wind_speed,
    y = PCB,
    color = HistoricalSourceWind
  )
) +
  geom_point(
    alpha = 0.35,
    size = 1.5
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 0.9
  ) +
  facet_grid(
    location ~ activity
  ) +
  scale_y_log10() +
  labs(
    x = "Wind speed",
    y = "PCB8 concentration",
    color = "Historical source wind") +
  theme_bw(base_size = 13)

ggplot(
  plot3_dat,
  aes(
    x = HistoricalSourceWind,
    y = wind_speed,
    fill = HistoricalSourceWind
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.3,
    size = 1.3
  ) +
  facet_grid(location ~ activity) +
  labs(
    x = "Historical source wind",
    y = "Wind speed") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

plot5_dat <- dataset %>%
  select(
    date,
    activity,
    air_temp,
    PCB8_South,
    PCB8_HS,
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS
  ) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    HistoricalSourceWind = case_when(
      location == "PCB8_South" ~ as.character(HistoricalSourceWind_South),
      location == "PCB8_HS"    ~ as.character(HistoricalSourceWind_HS)
    ),
    location = recode(
      location,
      PCB8_South = "South",
      PCB8_HS = "HS"
    )
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(air_temp),
    !is.na(HistoricalSourceWind)
  )

ggplot(
  plot5_dat,
  aes(
    x = air_temp,
    y = PCB,
    color = HistoricalSourceWind
  )
) +
  geom_point(
    alpha = 0.35,
    size = 1.5
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 0.9
  ) +
  facet_grid(location ~ activity) +
  scale_y_log10() +
  labs(
    x = "Air temperature",
    y = "PCB8 concentration",
    color = "Historical source wind") +
  theme_bw(base_size = 13)

ggplot(
  plot5_dat,
  aes(
    x = activity,
    y = air_temp,
    fill = activity
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.25,
    size = 1.2
  ) +
  facet_wrap(~ location) +
  labs(
    x = "Activity",
    y = "Air temperature (K)") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

pcb_long <- dataset %>%
  select(
    date,
    activity,
    
    PCB8_South, PCB8_HS,
    PCB15_South, PCB15_HS,
    PCB18.30_South, PCB18.30_HS,
    PCB20.28_South, PCB20.28_HS,
    PCB31_South, PCB31_HS
  ) %>%
  pivot_longer(
    cols = starts_with("PCB"),
    names_to = c("PCB", "location"),
    names_pattern = "(PCB(?:8|15|18\\.30|20\\.28|31))_(South|HS)",
    values_to = "concentration"
  ) %>%
  mutate(
    PCB = factor(
      PCB,
      levels = c(
        "PCB8",
        "PCB15",
        "PCB18.30",
        "PCB20.28",
        "PCB31"
      )
    ),
    location = factor(
      location,
      levels = c("South", "HS")
    )
  ) %>%
  filter(!is.na(concentration))

ggplot(
  pcb_long,
  aes(
    x = activity,
    y = concentration,
    fill = activity
  )
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55,
    alpha = 0.75
  ) +
  geom_jitter(
    aes(color = activity),
    width = 0.12,
    alpha = 0.25,
    size = 0.8
  ) +
  facet_grid(
    PCB ~ location,
    scales = "free_y"
  ) +
  scale_y_log10() +
  labs(
    x = "Activity",
    y = "PCB concentration",
    fill = "Activity",
    color = "Activity"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

pcb_long_cov <- dataset %>%
  select(
    date,
    activity,
    air_temp,
    wind_speed,
    wind_direction,
    water_temp,
    flow_cfs,
    
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    
    PCB8_South, PCB8_HS,
    PCB15_South, PCB15_HS,
    PCB18.30_South, PCB18.30_HS,
    PCB20.28_South, PCB20.28_HS,
    PCB31_South, PCB31_HS
  ) %>%
  pivot_longer(
    cols = matches(
      "^PCB(8|15|18\\.30|20\\.28|31)_(South|HS)$"
    ),
    names_to = c("PCB", "location"),
    names_pattern =
      "(PCB(?:8|15|18\\.30|20\\.28|31))_(South|HS)",
    values_to = "concentration"
  ) %>%
  mutate(
    air_temp_C = air_temp - 273.15,
    
    HistoricalSourceWind = case_when(
      location == "South" ~
        as.character(HistoricalSourceWind_South),
      location == "HS" ~
        as.character(HistoricalSourceWind_HS)
    ),
    
    PCB = factor(
      PCB,
      levels = c(
        "PCB8", "PCB15", "PCB18.30",
        "PCB20.28", "PCB31"
      )
    ),
    
    location = factor(
      location,
      levels = c("South", "HS")
    )
  ) %>%
  filter(
    !is.na(concentration),
    !is.na(air_temp_C)
  )

pcb_long_cov <- pcb_long_cov %>%
  mutate(
    HistoricalSourceWind = factor(
      HistoricalSourceWind,
      levels = c("NonSource", "Source")
    )
  )

ggplot(
  pcb_long_cov,
  aes(
    x = air_temp_C,
    y = concentration,
    color = activity
  )
) +
  geom_point(
    alpha = 0.20,
    size = 0.8
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.9
  ) +
  facet_grid(
    PCB ~ location,
    scales = "free_y"
  ) +
  scale_y_log10() +
  labs(
    x = "Air temperature (°C)",
    y = "PCB concentration",
    color = "Activity") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

idle_wind <- pcb_long_cov %>%
  filter(
    activity == "Idle",
    !is.na(wind_direction),
    !is.na(wind_speed),
    concentration > 0
  )

ggplot(
  idle_wind,
  aes(
    x = wind_direction,
    y = wind_speed,
    color = log10(concentration)
  )
) +
  geom_point(
    alpha = 0.65,
    size = 1.5
  ) +
  facet_grid(
    PCB ~ location
  ) +
  scale_x_continuous(
    breaks = c(0, 90, 180, 270, 360),
    limits = c(0, 360)
  ) +
  labs(
    x = "Wind direction (degrees)",
    y = "Wind speed",
    color = expression(log[10](PCB))) +
  theme_bw(base_size = 12)

idle_pcb8 <- idle_wind %>%
  filter(PCB == "PCB8")

idle_pcb8 <- idle_pcb8 %>%
  group_by(location) %>%
  mutate(
    pcb_group = case_when(
      concentration >= quantile(concentration, 0.90, na.rm = TRUE) ~
        "High (top 10%)",
      
      concentration >= quantile(concentration, 0.75, na.rm = TRUE) ~
        "Elevated (75–90%)",
      
      TRUE ~ "Typical (<75%)"
    )
  ) %>%
  ungroup()

ggplot(
  idle_pcb8,
  aes(
    x = wind_direction,
    y = wind_speed,
    color = pcb_group
  )
) +
  geom_point(
    alpha = 0.75,
    size = 2
  ) +
  coord_polar(
    theta = "x",
    start = 0,
    direction = 1
  ) +
  facet_wrap(~ location) +
  scale_x_continuous(
    limits = c(0, 360),
    breaks = c(0, 90, 180, 270),
    labels = c("N", "E", "S", "W")
  ) +
  labs(
    x = NULL,
    y = "Wind speed",
    color = "PCB8 level") +
  theme_bw(base_size = 12)

idle_pcb8 %>%
  count(location, HistoricalSourceWind, pcb_group) %>%
  group_by(location, HistoricalSourceWind) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  filter(pcb_group == "High (top 10%)") %>%
  select(
    location,
    HistoricalSourceWind,
    n,
    proportion
  )

idle_all <- idle_wind %>%
  group_by(PCB, location) %>%
  mutate(
    high_PCB =
      concentration >=
      quantile(
        concentration,
        0.90,
        na.rm = TRUE
      )
  ) %>%
  ungroup()

high_summary <- idle_all %>%
  count(
    PCB,
    location,
    HistoricalSourceWind,
    high_PCB
  ) %>%
  group_by(
    PCB,
    location,
    HistoricalSourceWind
  ) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  filter(high_PCB) %>%
  select(
    PCB,
    location,
    HistoricalSourceWind,
    n,
    proportion
  )

high_summary

ggplot(
  high_summary,
  aes(
    x = PCB,
    y = proportion,
    color = HistoricalSourceWind,
    group = HistoricalSourceWind
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ location) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Proportion classified as high PCB",
    color = "Historical source wind",
    title = "High PCB concentrations during Idle conditions",
    subtitle = "High = top 10% of concentrations within each PCB and location"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom"
  )

idle_sample_conditions <- dataset %>%
  filter(activity == "Idle") %>%
  select(
    date,
    air_temp,
    wind_speed,
    PCB8_South,
    PCB8_HS,
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS
  ) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "location",
    values_to = "PCB8"
  ) %>%
  mutate(
    HistoricalSourceWind = case_when(
      location == "PCB8_South" ~
        as.character(HistoricalSourceWind_South),
      location == "PCB8_HS" ~
        as.character(HistoricalSourceWind_HS)
    ),
    
    location = recode(
      location,
      PCB8_South = "South",
      PCB8_HS = "HS"
    ),
    
    air_temp_C = air_temp - 273.15
  ) %>%
  filter(
    !is.na(PCB8),
    !is.na(HistoricalSourceWind)
  )

idle_sample_conditions %>%
  group_by(location, HistoricalSourceWind) %>%
  summarise(
    n = n(),
    mean_temp_C = mean(air_temp_C, na.rm = TRUE),
    median_temp_C = median(air_temp_C, na.rm = TRUE),
    mean_wind_speed = mean(wind_speed, na.rm = TRUE),
    median_wind_speed = median(wind_speed, na.rm = TRUE),
    .groups = "drop"
  )

idle_south_pcb8 <- pcb_long_cov %>%
  filter(
    activity == "Idle",
    location == "South",
    PCB == "PCB8",
    !is.na(concentration),
    !is.na(air_temp_C),
    !is.na(wind_speed),
    !is.na(HistoricalSourceWind)
  )

m_idle_south_pcb8 <- lm(
  log10(concentration) ~
    HistoricalSourceWind +
    air_temp_C +
    wind_speed,
  data = idle_south_pcb8
)

summary(m_idle_south_pcb8)

install.packages("ggeffects")
library(ggeffects)

pred <- ggpredict(
  m_idle_south_pcb8,
  terms = "HistoricalSourceWind"
)

pred
plot(pred)

par(mfrow = c(2, 2))
plot(m_idle_south_pcb8)
par(mfrow = c(1, 1))

ggplot(
  idle_south_pcb8,
  aes(
    x = air_temp_C,
    y = residuals(m_idle_south_pcb8)
  )
) +
  geom_point(alpha = 0.35) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  labs(
    x = "Air temperature (°C)",
    y = "Model residual",
    title = "Residuals versus air temperature"
  ) +
  theme_bw()

ggplot(
  idle_south_pcb8,
  aes(
    x = wind_speed,
    y = residuals(m_idle_south_pcb8)
  )
) +
  geom_point(alpha = 0.35) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  labs(
    x = "Wind speed",
    y = "Model residual",
    title = "Residuals versus wind speed"
  ) +
  theme_bw()

m_idle_south_pcb8_quad <- lm(
  log10(concentration) ~
    HistoricalSourceWind +
    air_temp_C +
    I(air_temp_C^2) +
    wind_speed,
  data = idle_south_pcb8
)

anova(
  m_idle_south_pcb8,
  m_idle_south_pcb8_quad
)

AIC(
  m_idle_south_pcb8,
  m_idle_south_pcb8_quad
)

summary(m_idle_south_pcb8_quad)

10^coef(m_idle_south_pcb8_quad)[
  "HistoricalSourceWindSource"
]

10^confint(m_idle_south_pcb8_quad)[
  "HistoricalSourceWindSource",
]

library(dplyr)
library(tidyr)
library(purrr)
library(broom)

idle_models <- pcb_long_cov %>%
  filter(
    activity == "Idle",
    concentration > 0,
    !is.na(concentration),
    !is.na(air_temp_C),
    !is.na(wind_speed),
    !is.na(HistoricalSourceWind)
  ) %>%
  group_by(PCB, location) %>%
  nest() %>%
  mutate(
    model = map(
      data,
      ~ lm(
        log10(concentration) ~
          HistoricalSourceWind +
          air_temp_C +
          I(air_temp_C^2) +
          wind_speed,
        data = .x
      )
    )
  )

source_effects <- idle_models %>%
  mutate(
    tidy_model = map(
      model,
      ~ tidy(.x, conf.int = TRUE)
    )
  ) %>%
  select(PCB, location, tidy_model) %>%
  unnest(tidy_model) %>%
  filter(
    term == "HistoricalSourceWindSource"
  ) %>%
  mutate(
    ratio = 10^estimate,
    ratio_low = 10^conf.low,
    ratio_high = 10^conf.high,
    percent_change = (ratio - 1) * 100,
    percent_low = (ratio_low - 1) * 100,
    percent_high = (ratio_high - 1) * 100
  )

source_effects %>%
  select(
    PCB,
    location,
    ratio,
    ratio_low,
    ratio_high,
    percent_change,
    p.value
  )

ggplot(
  source_effects,
  aes(
    x = ratio,
    y = PCB
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = 2
  ) +
  geom_errorbarh(
    aes(
      xmin = ratio_low,
      xmax = ratio_high
    ),
    height = 0.15
  ) +
  geom_point(
    size = 3
  ) +
  facet_wrap(~ location) +
  labs(
    x = "Adjusted Source / NonSource concentration ratio",
    y = NULL,
    title = "Historical-source wind association during Idle",
    subtitle =
      "Adjusted for air temperature (quadratic) and wind speed"
  ) +
  theme_bw(base_size = 13)

dredge_pcb <- pcb_long_cov %>%
  filter(activity == "Dredging") %>%
  mutate(
    DredgingSourceWind = case_when(
      location == "South" ~
        as.character(dataset$DredgingSourceWind_South[
          match(date, dataset$date)
        ]),
      
      location == "HS" ~
        as.character(dataset$DredgingSourceWind_HS[
          match(date, dataset$date)
        ])
    ),
    DredgingSourceWind = factor(
      DredgingSourceWind,
      levels = c("NonSource", "Source")
    )
  ) %>%
  filter(
    !is.na(concentration),
    !is.na(DredgingSourceWind)
  )

DredgingSourceWind = case_when(
  location == "South" ~
    as.character(DredgingSourceWind_South),
  
  location == "HS" ~
    as.character(DredgingSourceWind_HS)
),

dredging_wind_alignment_deg = case_when(
  location == "South" ~
    dredging_wind_alignment_South_deg,
  
  location == "HS" ~
    dredging_wind_alignment_HS_deg
),

dredging_distance_m = case_when(
  location == "South" ~
    dredging_distance_to_South_m,
  
  location == "HS" ~
    dredging_distance_to_HS_m
),

DredgingSourceWind = factor(
  DredgingSourceWind,
  levels = c("NonSource", "Source")
)

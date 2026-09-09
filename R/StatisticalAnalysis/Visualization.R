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


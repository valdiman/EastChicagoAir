# Data visualization 

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("tidyr")
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

ggplot(dataset, aes(x = activity, y = PCB8_South, fill = activity)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    x = "Harbor activity",
    y = "PCB8 concentration",
    title = "PCB8 concentrations at South by harbor activity"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = activity, y = PCB8_HS, fill = activity)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    x = "Harbor activity",
    y = "PCB8 concentration",
    title = "PCB8 concentrations at HS by harbor activity"
  ) +
  theme_minimal()

pcb_long <- dataset %>%
  select(date, activity, PCB8_South, PCB8_HS) %>%
  pivot_longer(
    cols = c(PCB8_South, PCB8_HS),
    names_to = "Location",
    values_to = "PCB8"
  )

ggplot(pcb_long, aes(x = activity, y = PCB8, fill = Location)) +
  geom_boxplot(na.rm = TRUE, position = position_dodge()) +
  labs(
    x = "Harbor activity",
    y = "PCB8 concentration",
    title = "PCB8 concentrations by activity and monitoring location"
  ) +
  theme_minimal()

d_south <- dataset %>%
  select(
    date,
    activity,
    ConstructionSourceWind_South,
    PCB8_South
  ) %>%
  filter(
    !is.na(ConstructionSourceWind_South),
    !is.na(PCB8_South)
  )

ggplot(
  d_south,
  aes(
    x = ConstructionSourceWind_South,
    y = PCB8_South,
    fill = ConstructionSourceWind_South
  )
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5,
    size = 2
  ) +
  labs(
    x = "Construction source wind at South",
    y = "PCB8 concentration",
    title = "PCB8 Concentrations at South by Historical Source Wind"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Data visualization 

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("ggplot")
}

# Library
{
  library(ggplot2)
}

# Read data
dataset <- read.csv("Data/FinalDataset/DatasetV01.csv")

dataset <- dataset %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  )

dataset <- dataset %>%
  mutate(
    across(
      c(Construction, Dredging, Idle),
      ~ factor(.x, levels = c(0, 1))
    ),
    SourceWind_South = factor(
      SourceWind_South,
      levels = c("NonSource", "Source")
    ),
    SourceWind_HS = factor(
      SourceWind_HS,
      levels = c("NonSource", "Source")
    )
  )

# Plot
# Time
ggplot(dataset, aes(date, PCB8_South)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(dataset, aes(date, PCB8_HS)) +
  geom_line() +
  geom_point() +
  theme_bw()

# Wind speed
ggplot(dataset, aes(wind_speed, PCB8_South)) +
  geom_point(alpha = 0.6) +
  theme_bw()

ggplot(dataset, aes(wind_speed, PCB8_HS)) +
  geom_point(alpha = 0.6) +
  theme_bw()

# Air temperature
ggplot(dataset, aes(invT, PCB8_South)) +
  geom_point(alpha = 0.6) +
  theme_bw()

ggplot(dataset, aes(invT, PCB8_HS)) +
  geom_point(alpha = 0.6) +
  theme_bw()

# Water temperature
ggplot(dataset, aes(water_temp, PCB8_South)) +
  geom_point(alpha = 0.6) +
  theme_bw()

ggplot(dataset, aes(water_temp, PCB8_HS)) +
  geom_point(alpha = 0.6) +
  theme_bw()

# Water flow
ggplot(dataset, aes(flow_abs, PCB8_South)) +
  geom_point(alpha = 0.6) +
  theme_bw()

ggplot(dataset, aes(flow_abs, PCB8_HS)) +
  geom_point(alpha = 0.6) +
  theme_bw()

# Water turb
ggplot(dataset, aes(turb_FNU, PCB8_South)) +
  geom_point(alpha = 0.6) +
  theme_bw()

ggplot(dataset, aes(turb_FNU, PCB8_HS)) +
  geom_point(alpha = 0.6) +
  theme_bw()

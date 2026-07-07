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

dataset <- read.csv("Data/FinalDataset/DatasetV01.csv")
dataset$date <- as.Date(dataset$date)
dataset <- dataset %>%
  mutate(
    Activity = factor(
      Activity,
      levels = c("Idle", "Construction", "Dredging")
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

ggplot(dataset, aes(date, PCB8_South)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(dataset, aes(date, PCB8_HS)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(dataset, aes(date, DailyDredgeVolume_yd3)) +
  geom_step() +
  theme_bw()

ggplot(dataset, aes(date, turb_dredge_mean)) +
  geom_line() +
  geom_point(size = 0.6) +
  theme_bw()



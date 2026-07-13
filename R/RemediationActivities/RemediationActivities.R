# Remediation activities per day
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# Dates where obtained from the water quality monitoring turbidity
# https://indianaharbor.evs.anl.gov/data/water/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("purr")
  install.packages("lubridate")
}

# Library
{
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
}

# Read activity data ------------------------------------------------------
remediation_activities <- read.csv(
  "Data/RemediationActivities/activities_distanceV4.csv")

# Change date format
remediation_activities <- remediation_activities %>%
  mutate(
    DateStart = as.Date(DateStart, "%m/%d/%Y"),
    DateEnd   = as.Date(DateEnd, "%m/%d/%Y")
  )

# Expand to daily records
daily_long <- remediation_activities %>%
  mutate(
    date = map2(DateStart, DateEnd, seq.Date, by = "day")
  ) %>%
  unnest(date) %>%
  mutate(
    Year = year(date)
  )

# Activity table
activity_daily <- daily_long %>%
  group_by(date) %>%
  summarise(
    Construction = as.integer(any(Activity == "Construction")),
    Dredging     = as.integer(any(Activity == "Dredging")),
    Idle         = as.integer(any(Activity == "Idle")),
    .groups = "drop"
  )

# Distances
distance_wide <- daily_long %>%
  filter(Units == "m") %>%
  mutate(
    Variable = paste(Location, ReferencePoint, "m", sep = "_")
  ) %>%
  select(date, Variable, Value) %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  )

# Volumes
# Annual volume by Year and Location
annual_volume <- daily_long %>%
  filter(Units == "yd3") %>%
  group_by(Year, Location) %>%
  summarise(
    AnnualVolume = first(Value),
    .groups = "drop"
  )

# Number of unique dredging days
active_days <- daily_long %>%
  filter(
    Activity == "Dredging",
    Units == "m"
  ) %>%
  distinct(
    Year,
    Location,
    date
  ) %>%
  group_by(
    Year,
    Location
  ) %>%
  summarise(
    ActiveDays = n(),
    .groups = "drop"
  )

# Compute daily production
volume_daily <- annual_volume %>%
  left_join(
    active_days,
    by = c("Year", "Location")
  ) %>%
  mutate(
    DailyVolume = AnnualVolume / ActiveDays
  )

# Attach daily and annual volumes
volume_long <- daily_long %>%
  filter(Units == "yd3") %>%
  left_join(
    volume_daily,
    by = c("Year", "Location")
  )

# Daily volume
volume_daily_wide <- volume_long %>%
  mutate(
    Variable = paste0(Location, "_Daily_yd3")
  ) %>%
  select(
    date,
    Variable,
    DailyVolume
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = DailyVolume
  )

# Annual volume
volume_annual_wide <- volume_long %>%
  mutate(
    Variable = paste0(Location, "_Annual_yd3")
  ) %>%
  select(
    date,
    Variable,
    AnnualVolume
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = AnnualVolume
  )

# Final daily dataset
daily_activities <- activity_daily %>%
  left_join(distance_wide, by = "date") %>%
  left_join(volume_daily_wide, by = "date") %>%
  left_join(volume_annual_wide, by = "date") %>%
  arrange(date)


# Export data
write.csv(daily_activities, "Data/RemediationActivities/all_activity_daily.csv",
          row.names = FALSE)


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
}

# Library
{
  library(dplyr)
  library(tidyr)
  library(purrr)
}

# Read activity data ------------------------------------------------------
# Revised data
remediation_activities <- read.csv("Data/RemediationActivities/activities_distanceV3.csv")

# Convert dates
remediation_activities <- remediation_activities %>%
  mutate(
    DateStart = as.Date(DateStart, "%m/%d/%Y"),
    DateEnd   = as.Date(DateEnd, "%m/%d/%Y")
  )

# Expand activities to daily records
daily_activities <- remediation_activities %>%
  mutate(
    Date = map2(DateStart, DateEnd, seq.Date, by = "day"),
    n_days = lengths(Date),
    Value = case_when(
      Units == "yd3" ~ Value / n_days,  # convert total volume to daily volume
      TRUE ~ Value
    )
  ) %>%
  unnest(Date) %>%
  select(
    Date,
    Activity,
    Location,
    Metric,
    ReferencePoint,
    Value,
    Units
  )

# Create complete daily sequence
all_dates <- tibble(
  Date = seq(
    min(remediation_activities$DateStart),
    max(remediation_activities$DateEnd),
    by = "day"
  )
)

# Add idle days
daily_activities <- all_dates %>%
  left_join(daily_activities, by = "Date") %>%
  mutate(
    Activity = replace_na(Activity, "Idle")
  ) %>%
  arrange(Date)

# Export data
write.csv(daily_activities, "Data/RemediationActivities/all_activity_daily.csv",
          row.names = FALSE)


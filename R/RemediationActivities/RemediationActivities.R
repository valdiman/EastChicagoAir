# Remediation activities per day
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# https://indianaharbor.evs.anl.gov/data/ambient/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("purr")
}

# Library
{
  library(dplyr)
  library(purrr)
}

# Read data ---------------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))
# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")
# Get unique date values
ace_dates <- ace[!duplicated(ace$date), "date", drop = FALSE]

# Read activity data ------------------------------------------------------
# Revised data
act.data <- read.csv("Data/RemediationActivities/activities_distanceV2.csv")

# Clean activity data
activities_clean <- act.data %>%
  mutate(
    DateStart = as.Date(DateStart, "%m/%d/%Y"),
    DateEnd   = as.Date(DateEnd, "%m/%d/%Y")
  ) %>%
  distinct(DateStart, DateEnd, Activity)

# Create full daily timeline
all_dates <- data.frame(
  date = seq(min(activities_clean$DateStart),
             max(activities_clean$DateEnd),
             by = "day")
)

# Assign activity (priority: Idle > Dredging > Construction)
get_activity <- function(d, df) {
  if (any(df$Activity == "Idle" & d >= df$DateStart & d <= df$DateEnd)) {
    "Idle"
  } else if (any(df$Activity == "Dredging" & d >= df$DateStart & d <= df$DateEnd)) {
    "Dredging"
  } else if (any(df$Activity == "Construction" & d >= df$DateStart & d <= df$DateEnd)) {
    "Construction"
  } else {
    "Idle"  # default
  }
}

activity_daily <- all_dates %>%
  mutate(Activity = map_chr(date, get_activity, df = activities_clean))

# Export data
write.csv(activity_daily, "Data/RemediationProject/activity_dailyV2.csv",
          row.names = FALSE)


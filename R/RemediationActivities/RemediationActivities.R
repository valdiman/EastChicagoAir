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
remediation_activities <- read.csv("Data/RemediationActivities/activities_distanceV2.csv")

remediation_activities <- remediation_activities %>%
  mutate(
    DateStart = as.Date(DateStart, "%m/%d/%Y"),
    DateEnd   = as.Date(DateEnd, "%m/%d/%Y"),
    Duration_days = as.numeric(DateEnd - DateStart) + 1,
    DailyDredgeVolume_yd3 = if_else(
      Metric == "Volume",
      Value / Duration_days,
      NA_real_
    )
  )

activity_daily <- data.frame(
  date = seq(
    min(remediation_activities$DateStart),
    max(remediation_activities$DateEnd),
    by = "day"
  )
)

activity_daily$Activity <- "Idle"
activity_daily$Distance_South_m <- NA_real_
activity_daily$Distance_HS_m <- NA_real_
activity_daily$DailyDredgeVolume_yd3 <- NA_real_

for(i in seq_len(nrow(activity_daily))){
  
  d <- activity_daily$date[i]
  
  active <- remediation_activities[
    remediation_activities$DateStart <= d &
      remediation_activities$DateEnd >= d,
  ]
  
  ## Activity
  if(any(active$Activity == "Dredging")){
    activity_daily$Activity[i] <- "Dredging"
  } else if(any(active$Activity == "Construction")){
    activity_daily$Activity[i] <- "Construction"
  } else {
    activity_daily$Activity[i] <- "Idle"
  }
  
  ## South distance
  south <- active %>%
    filter(
      Metric == "Distance",
      ReferencePoint == "South"
    )
  
  if (nrow(south) > 0 && any(!is.na(south$Value))) {
    activity_daily$Distance_South_m[i] <- min(south$Value, na.rm = TRUE)
  } else {
    activity_daily$Distance_South_m[i] <- NA_real_
  }
  
  ## HS distance
  hs <- active %>%
    filter(
      Metric == "Distance",
      ReferencePoint == "HS"
    )
  
  if (nrow(hs) > 0 && any(!is.na(hs$Value))) {
    activity_daily$Distance_HS_m[i] <- min(hs$Value, na.rm = TRUE)
  } else {
    activity_daily$Distance_HS_m[i] <- NA_real_
  }
  
  ## Daily dredged volume
  vol <- active %>%
    filter(
      Metric == "Volume"
    )
  
  if(nrow(vol) > 0){
    activity_daily$DailyDredgeVolume_yd3[i] <- sum(vol$DailyDredgeVolume_yd3, na.rm = TRUE)
  }
  
}

activity_daily$Activity <- factor(
  activity_daily$Activity,
  levels = c("Idle", "Construction", "Dredging")
)

# Export data
write.csv(activity_daily, "Data/RemediationActivities/all_activity_daily.csv",
          row.names = FALSE)

# Match air concentration dates
activity_daily2 <- activity_daily %>%
  semi_join(
    ace_dates,
    by = "date")

# Export data
write.csv(activity_daily2, "Data/RemediationActivities/activity_daily.csv",
          row.names = FALSE)


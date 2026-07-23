
# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("geosphere")
  install.packages("tidyverse")
  install.packages("lubridate")
}

# Library
{
  library(tidyverse)
  library(lubridate)
  library(geosphere)
}


years <- c(2012:2020, 2024)

dredge_turb <- map_dfr(years, ~{
  read_csv(
    paste0("Data/ACE/waterquality_", .x, ".csv"),
    col_select = c(
      ReadingDate,
      DredgeContribution,
      Buoy1Latitude,
      Buoy1Longitude,
      Buoy2Latitude,
      Buoy2Longitude
    ),
    col_types = cols(
      ReadingDate = col_character(),
      DredgeContribution = col_character(),
      Buoy1Latitude = col_character(),
      Buoy1Longitude = col_character(),
      Buoy2Latitude = col_character(),
      Buoy2Longitude = col_character()
    ),
    show_col_types = FALSE
  )
})

clean_num <- function(x) {
  x <- suppressWarnings(readr::parse_number(x))
  x[!is.finite(x)] <- NA_real_
  x
}

clean_lat <- function(x) {
  x <- clean_num(x)
  x[!is.na(x) & (x < -90 | x > 90)] <- NA_real_
  x
}

clean_lon <- function(x) {
  x <- clean_num(x)
  x[!is.na(x) & (x < -180 | x > 180)] <- NA_real_
  x
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else median(x)
}

south <- c(-87.487138, 41.647095)  # lon, lat
hs    <- c(-87.487700, 41.641241)  # lon, lat

dredge_turb <- dredge_turb %>%
  mutate(
    ReadingDate = mdy_hm(ReadingDate),
    date = as.Date(ReadingDate),
    DredgeContribution = clean_num(DredgeContribution),
    
    Buoy1Latitude  = clean_lat(Buoy1Latitude),
    Buoy1Longitude = clean_lon(Buoy1Longitude),
    Buoy2Latitude  = clean_lat(Buoy2Latitude),
    Buoy2Longitude = clean_lon(Buoy2Longitude)
  ) %>%
  filter(!is.na(date), date != as.Date("2024-08-28")) %>%
  mutate(
    mid_lat = if_else(
      !is.na(Buoy1Latitude) & !is.na(Buoy2Latitude),
      (Buoy1Latitude + Buoy2Latitude) / 2,
      NA_real_
    ),
    mid_lon = if_else(
      !is.na(Buoy1Longitude) & !is.na(Buoy2Longitude),
      (Buoy1Longitude + Buoy2Longitude) / 2,
      NA_real_
    )
  ) %>%
  mutate(
    distance_to_South = if_else(
      !is.na(mid_lat) & !is.na(mid_lon),
      distHaversine(cbind(mid_lon, mid_lat), south),
      NA_real_
    ),
    distance_to_HS = if_else(
      !is.na(mid_lat) & !is.na(mid_lon),
      distHaversine(cbind(mid_lon, mid_lat), hs),
      NA_real_
    )
  )

dredge_daily <- dredge_turb %>%
  group_by(date) %>%
  summarize(
    n_obs = sum(!is.na(DredgeContribution)),
    
    turb_dredge_mean = if (n_obs > 0)
      mean(DredgeContribution, na.rm = TRUE)
    else
      NA_real_,
    
    turb_dredge_min = if (n_obs > 0)
      min(DredgeContribution, na.rm = TRUE)
    else
      NA_real_,
    
    turb_dredge_max = if (n_obs > 0)
      max(DredgeContribution, na.rm = TRUE)
    else
      NA_real_,
    
    distance_to_South = safe_median(distance_to_South),
    distance_to_HS    = safe_median(distance_to_HS),
    
    .groups = "drop"
  )

# Read the cleaned dredging schedule
dredging_events <- read_csv(
  "Data/RemediationActivities/dredging_activities_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(
    DateStart = ymd(DateStart),
    DateEnd = ymd(DateEnd),
    TotalVolume_yd3 = as.numeric(TotalVolume_yd3)
  )

# Expand each dredging event to daily rows
dredging_daily <- dredging_events %>%
  mutate(
    n_days = as.integer(DateEnd - DateStart) + 1,
    daily_volume = TotalVolume_yd3 / n_days,
    date = map2(DateStart, DateEnd, ~seq.Date(.x, .y, by = "day"))
  ) %>%
  unnest(date) %>%
  select(date, Location, daily_volume)




# ------------------------------------------------------------
# 1) Construction daily table
# ------------------------------------------------------------
construction_events <- read_csv(
  "Data/RemediationActivities/construction_activities_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(
    DateStart = ymd(DateStart),
    DateEnd   = ymd(DateEnd)
  )

construction_daily <- construction_events %>%
  mutate(
    date = map2(DateStart, DateEnd, ~seq.Date(.x, .y, by = "day"))
  ) %>%
  unnest(date) %>%
  group_by(date) %>%
  summarize(
    construction_present = TRUE,
    construction_distance_to_South = first(DistanceSouth_m),
    construction_distance_to_HS = first(DistanceHS_m),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 2) Dredging schedule daily table
# ------------------------------------------------------------
dredging_events <- read_csv(
  "Data/RemediationActivities/dredging_activities_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(
    DateStart = ymd(DateStart),
    DateEnd   = ymd(DateEnd),
    TotalVolume_yd3 = as.numeric(TotalVolume_yd3)
  )

dredging_schedule_daily <- dredging_events %>%
  mutate(
    n_days = as.integer(DateEnd - DateStart) + 1,
    daily_volume = TotalVolume_yd3 / n_days,
    date = map2(DateStart, DateEnd, ~seq.Date(.x, .y, by = "day"))
  ) %>%
  unnest(date) %>%
  group_by(date) %>%
  summarize(
    dredging_present = TRUE,
    daily_volume = sum(daily_volume, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3) Combine dredging schedule with turbidity-derived daily data
# ------------------------------------------------------------
dredging_daily <- dredge_daily %>%
  inner_join(dredging_schedule_daily, by = "date")

# ------------------------------------------------------------
# 4) Build full calendar from 2001-12-03 onward
# ------------------------------------------------------------
end_date <- max(
  max(construction_daily$date, na.rm = TRUE),
  max(dredging_daily$date, na.rm = TRUE)
)

calendar <- tibble(
  date = seq.Date(as.Date("2001-12-03"), end_date, by = "day")
)

# ------------------------------------------------------------
# 5) Final combined daily table
# ------------------------------------------------------------
all_activity_daily <- calendar %>%
  left_join(construction_daily, by = "date") %>%
  left_join(dredging_daily, by = "date") %>%
  mutate(
    construction_present = replace_na(construction_present, FALSE),
    dredging_present = replace_na(dredging_present, FALSE),
    daily_volume = replace_na(daily_volume, 0),
    activity = case_when(
      construction_present & dredging_present ~ "Both",
      construction_present ~ "Construction",
      dredging_present ~ "Dredging",
      TRUE ~ "Idle"
    )
  ) %>%
  transmute(
    date,
    activity,
    
    daily_volume_yd3 = daily_volume,
    
    turb_dredge_mean,
    turb_dredge_min,
    turb_dredge_max,
    
    dredging_distance_to_South_m = distance_to_South,
    dredging_distance_to_HS_m = distance_to_HS,
    
    construction_distance_to_South_m = construction_distance_to_South,
    construction_distance_to_HS_m = construction_distance_to_HS
  ) %>%
  arrange(date)

write_csv(all_activity_daily, "Data/RemediationActivities/all_activity_dailyV2.csv")










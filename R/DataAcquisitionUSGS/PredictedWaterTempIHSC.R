# Reconstruct Water Temperature for ACE Dataset
# Indiana Harbor Canal, East Chicago, IN
#
# Observed USGS water temperatures are used whenever available.
# Missing temperatures (pre-2011) are estimated using a model
# calibrated against observed USGS temperatures and Daymet air
# temperatures.
#
# Calibrated model:
# temp ~ tair + sin(DOY) + cos(DOY)
#
# Model performance:
# R²   = 0.963
# RMSE = 1.35 °C
# MAE  = 1.07 °C

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dataRetrieval")
  install.packages("dplyr")
  install.packages("sf")
  install.packages("daymetr")
  install.packages("tidyr")
  install.packages("lubridate")
}

# Library
{
  library(dataRetrieval)
  library(dplyr)
  library(sf)
  library(daymetr)
  library(tidyr)
  library(lubridate)
}

# Helper function ---------------------------------------------------------
find_single_col <- function(df, pattern, what = "column") {
  
  matches <- grep(
    pattern,
    names(df),
    ignore.case = TRUE,
    value = TRUE)
  
  if(length(matches) == 0) {
    stop(
      sprintf(
        "No %s found matching '%s'",
        what,
        pattern
      ),
      call. = FALSE)
  }
  
  if(length(matches) > 1) {
    stop(
      sprintf(
        "Multiple %ss found matching '%s': %s",
        what,
        pattern,
        paste(matches, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  matches
}

# Read ACE data -----------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))
# Change forma to date
ace$date <- as.Date(ace$date, origin = "1899-12-30")
# Get unique date values
ace_dates <- ace[!duplicated(ace$date), "date", drop = FALSE]

# Download USGS water temperature -----------------------------------------
site.ihsc <- "04092750"

temp <- read_waterdata_daily(
  monitoring_location_id =
    paste("USGS", site.ihsc, sep = "-"),
  parameter_code = "00010",
  statistic_id = "00003",
  time = c(
    as.character(min(ace$date)),
    as.character(max(ace$date))))

temp_values <- temp %>%
  st_drop_geometry() %>%
  select(time, value) %>%
  group_by(time) %>%
  summarize(value = mean(value, na.rm = TRUE),
            .groups = "drop")

ace <- ace %>%
  left_join(temp_values, by = c("date" = "time")) %>%
  rename(temp = value)

# Download Daymet air temperature -----------------------------------------
lat <- 41.652821
lon <- -87.462739

yr1 <- year(min(ace$date))
yr2 <- year(max(ace$date))

dm_act <- download_daymet("act", lat, lon,
                          yr1, yr2, internal = TRUE,
                          simplify = TRUE)

if(is.list(dm_act) &&
   "data" %in% names(dm_act)) {
  
  dm_act <- dm_act$data
}

wide_act <- dm_act %>%
  pivot_wider(
    names_from = measurement,
    values_from = value)

tmax_col <- find_single_col(
  wide_act,
  "tmax",
  "tmax column")

tmin_col <- find_single_col(
  wide_act,
  "tmin",
  "tmin column")

air <- wide_act %>%
  mutate(
    date =
      as.Date(
        paste0(year, "-01-01")
      ) + yday - 1,
    
    doy = yday(date),
    
    tair =
      (.data[[tmax_col]] +
         .data[[tmin_col]]) / 2
  ) %>%
  select(date, doy, tair)

# Build calibration dataset -----------------------------------------------
ace_cal <- ace_dates %>%
  left_join(
    temp_values,
    by = c("date" = "time")
  ) %>%
  rename(temp = value) %>%
  left_join(
    air %>% select(date, tair),
    by = "date")

cal <- ace_cal %>%
  filter(
    !is.na(temp),
    !is.na(tair)
  ) %>%
  mutate(
    doy = yday(date),
    sin_doy = sin(2*pi*doy/365.25),
    cos_doy = cos(2*pi*doy/365.25))

# Calibrated water temperature model --------------------------------------
fit <- lm(temp ~ tair + sin_doy + cos_doy, data = cal)

summary(fit)

# Predict water temperature for all dates ---------------------------------
all_dates <- air %>%
  mutate(
    sin_doy = sin(2*pi*doy/365.25),
    cos_doy = cos(2*pi*doy/365.25))

all_dates$pred_water <- predict(fit, newdata = all_dates)

# Create final data frame -------------------------------------------------
tempwater_ihsc <- ace_dates %>%
  left_join(
    temp_values,
    by = c("date" = "time")
  ) %>%
  rename(temp = value) %>%
  left_join(
    all_dates %>%
      select(date, pred_water),
    by = "date"
  ) %>%
  transmute(
    date,
    water_temp = coalesce(temp, pred_water))

# Model diagnostics -------------------------------------------------------
cal$pred <- predict(fit, newdata = cal)
rmse <- sqrt(mean((cal$temp - cal$pred)^2))
mae <- mean(abs(cal$temp - cal$pred))

cat("R²   =", summary(fit)$r.squared, "\n")
cat("RMSE =", rmse, "\n")
cat("MAE  =", mae, "\n")

# Save --------------------------------------------------------------------
write.csv(tempwater_ihsc, "Data/USGS/tempwater_ihsc.csv", row.names = FALSE)


# Change in turbidity data
# Turbidity in NTU

# Install packages
{
  install.packages("tidyverse")
  install.packages("lubridate")
}

# Load libraries
{
  library(tidyverse)
  library(lubridate)
}

# Read turbidity data from ACE --------------------------------------------
years <- c(2012:2020, 2024)

dredge_turb <- map_dfr(years, ~{
  read_csv(
    paste0("Data/ACE/waterquality_", .x, ".csv"),
    col_select = c(ReadingDate, DredgeContribution),
    show_col_types = FALSE
  ) %>%
    mutate(
      ReadingDate = as.character(ReadingDate),
      DredgeContribution = as.character(DredgeContribution)
    )
})

# Add daily date
dredge_turb <- dredge_turb %>%
  mutate(
    ReadingDate = mdy_hm(ReadingDate),
    DredgeContribution = as.numeric(DredgeContribution),
    date = as.Date(ReadingDate))

# Final version
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
    .groups = "drop"
  )

# Activity dates
all_dates <- read.csv("Data/RemediationActivities/all_activity_daily.csv")
all_dates$date <- as.Date(all_dates$date, origin = "1899-12-30")

# Merge data
turb_dredge_ihsc <- all_dates %>%
  select(date) %>%
  left_join(
    dredge_daily,
    by = "date") %>%
  transmute(
    date,
    turb_dredge_mean,
    turb_dredge_min,
    turb_dredge_max)

# Save
write.csv(turb_dredge_ihsc, "Data/ACE/tubidity_dredge_ihsc.csv",
          row.names = FALSE)


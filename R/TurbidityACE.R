# Change in turbidity data

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
    show_col_types = FALSE
  ) %>%
    transmute(
      ReadingDate = as.character(ReadingDate),
      DredgeContribution = as.character(DredgeContribution))
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
    dredge_mean = mean(DredgeContribution, na.rm = TRUE),
    dredge_min  = min(DredgeContribution, na.rm = TRUE),
    dredge_max  = max(DredgeContribution, na.rm = TRUE),
    n_obs       = sum(!is.na(DredgeContribution)),
    .groups = "drop")

# Read ACE concentration data ---------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")

ace <- ace.raw %>%
  filter(location != 0)

ace$date <- as.Date(ace$date, origin = "1899-12-30")

# Merge data
dredge_ihsc <- ace %>%
  select(date) %>%
  left_join(
    dredge_daily,
    by = "date") %>%
  transmute(
    date,
    dredge_mean,
    dredge_min,
    dredge_max)

# Save
write.csv(dredge_ihsc, "Data/ACE/dredge_ihsc.csv", row.names = FALSE)





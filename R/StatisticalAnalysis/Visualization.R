# Data visualization 

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot2")
  install.packages("tidyr")
  install.packages("plotly")
  install.packages("ggeffects")
  install.packages("purrr")
  install.packages("broom")
  install.packages("openair")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(plotly)      # Interactive / 3D plots
  library(ggeffects)   # Model predictions
  library(purrr)       # Functional programming / model iteration
  library(broom)       # Tidy model outputs
  library(openair)     # Pollution roses / wind analyses
}

# Read data ---------------------------------------------------------------
dataset <- read.csv("Data/FinalDataset/DatasetV03.csv")

# Format data -------------------------------------------------------------
# Source concepts are kept separate:
#
#   HistoricalSourceWind_*
#       Persistent historical contaminated-source pathway.
#       Applies regardless of the remediation activity occurring that day.
#       NonSource & Source
#
#   ConstructionSourceWind_*
#       Additional construction source pathway.
#       Applies only during construction.
#       NoConstruction (= Idle & Dredging), for Construction: Source & NonSource
#
#   DredgingSourceWind_*
#       Additional dredging source pathway.
#       Uses the GPS-derived daily dredging location and wind alignment.
#       NoDredging (= Idle & Construction), for Dredging: Source & NonSource
#
# Activity remains separate:
#   Idle / Construction / Dredging

dataset <- dataset %>%
  mutate(
    date = as.Date(date),
    activity = factor(activity, levels = c("Idle", "Construction", "Dredging")),
    HistoricalSourceWind_South = factor(HistoricalSourceWind_South,
                                        levels = c("NonSource", "Source")),
    HistoricalSourceWind_HS = factor(HistoricalSourceWind_HS,
                                     levels = c("NonSource", "Source")),
    ConstructionSourceWind_South = factor(ConstructionSourceWind_South,
                                          levels = c("NoConstruction", "NonSource", "Source")),
    ConstructionSourceWind_HS = factor(ConstructionSourceWind_HS,
                                       levels = c("NoConstruction", "NonSource", "Source")),
    DredgingSourceWind_South = factor(DredgingSourceWind_South,
                                      levels = c("NoDredging", "NonSource", "Source")),
    DredgingSourceWind_HS = factor(DredgingSourceWind_HS,
                                   levels = c("NoDredging", "NonSource", "Source"))
  )

# Activity Plots ----------------------------------------------------------
# (i) Plot activities
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

# Prepare data
plot_dat <- dataset %>%
  select(
    date,
    activity,
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS"))
  ) %>%
  pivot_longer(
    cols = c(
      all_of(paste0(PCB, "_South")),
      all_of(paste0(PCB, "_HS"))
    ),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = recode(
      location,
      !!paste0(PCB, "_South") := "South",
      !!paste0(PCB, "_HS") := "HS"
    )
  ) %>%
  filter(!is.na(PCB))

ggplot(plot_dat, aes(x = activity, y = PCB)) +
  geom_boxplot(outlier.shape = NA, width = 0.55) +
  geom_jitter(width = 0.12, alpha = 0.55, size = 2) +
  facet_wrap(~ location) +
  scale_y_log10() +
  labs(x = "Activity", y = paste0(PCB, " concentration")) +
  theme_bw(base_size = 13)

# (ii) Plot activities
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

plot2_dat <- dataset %>%
  select(
    date,
    activity,
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = if_else(
      grepl("_South$", location),
      "South",
      "HS"),
    HistoricalSourceWind = if_else(
      location == "South",
      as.character(HistoricalSourceWind_South),
      as.character(HistoricalSourceWind_HS)),
    location = factor(
      location,
      levels = c("South", "HS")),
    HistoricalSourceWind = factor(
      HistoricalSourceWind,
      levels = c("NonSource", "Source"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(HistoricalSourceWind))

bplot.act.PCB <- ggplot(
  plot2_dat,
  aes(x = activity, y = PCB,
    fill = HistoricalSourceWind)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    outlier.shape = NA) +
  geom_point(
    aes(color = HistoricalSourceWind),
    position = position_jitterdodge(
      jitter.width = 0.10,
      dodge.width = 0.8),
    alpha = 0.45,
    size = 1.7) +
  facet_wrap(~ location) +
  scale_y_log10() +
  labs(
    x = "Activity",
    y = paste0(PCB, " concentration"),
    fill = "Historical source wind",
    color = "Historical source wind") +
  theme_bw(base_size = 13)

bplot.act.PCB

ggsave(paste0("Output/Plots/Concentrations/Activities/BPActivities_",
              PCB, ".png"), plot = bplot.act.PCB, width = 12,
       height = 8, dpi = 500)

# Temperature Plots -------------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

temp_dat <- dataset %>%
  select(
    date,
    activity,
    air_temp,
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    # Location
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"),
    # Temperature in Celsius
    air_temp_C = air_temp - 273.15,
    
    # Activity-specific source wind
    ActivitySourceWind = case_when(
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)),
    location = factor(
      location,
      levels = c("South", "HS")),
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(air_temp_C),
    !is.na(ActivitySourceWind))

plot.temp.PCB <- ggplot(
  temp_dat,
  aes(x = air_temp_C, y = PCB, color = ActivitySourceWind)) +
  geom_point(alpha = 0.40, size = 1.6) +
  facet_grid(location ~ activity) +
  scale_y_log10() +
  labs(x = "Air temperature (°C)", y = paste0(PCB, " concentration"),
       color = "Source wind") +
  theme_bw(base_size = 13)

plot.temp.PCB

ggsave(paste0("Output/Plots/Concentrations/Temperature/BPActivities_",
              PCB, ".png"), plot = plot.temp.PCB, width = 12,
       height = 8, dpi = 500)

# Wind Speed Plots --------------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

wind_dat <- dataset %>%
  select(
    date,
    activity,
    wind_speed,
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    # Location
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"),
    # Activity-specific source wind
    ActivitySourceWind = case_when(
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)),
    location = factor(
      location,
      levels = c("South", "HS")),
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(wind_speed),
    !is.na(ActivitySourceWind))

plot.wind.PCB <- ggplot(
  wind_dat,
  aes(x = wind_speed, y = PCB, color = ActivitySourceWind)) +
  geom_point(alpha = 0.40, size = 1.6) +
  facet_grid(location ~ activity) +
  scale_y_log10() +
  labs(x = "Wind speed", y = paste0(PCB, " concentration"),
       color = "Source wind") +
  theme_bw(base_size = 13)

plot.wind.PCB

ggsave(paste0("Output/Plots/Concentrations/WindSpeed/WindSpeed_",
              PCB, ".png"), plot = plot.wind.PCB, width = 12,
       height = 8, dpi = 500)

# Wind Rose Style Plots ---------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB20.28"

# All activities
pcb_activity_both <- dataset %>%
  select(
    wind_direction,
    wind_speed,
    activity,
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS"))
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"),
    location = factor(
      location,
      levels = c("South", "HS"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(wind_direction),
    !is.na(wind_speed)
  ) %>%
  transmute(
    wd = wind_direction,
    ws = wind_speed,
    PCB,
    activity,
    location)

PCB_upper <- ceiling(max(pcb_activity_both$PCB, na.rm = TRUE))

PCB_breaks <- c(0, 5, 10, 20, 40, 80, PCB_upper)

wrplot_PCB_activity <- pollutionRose(
  pcb_activity_both,
  pollutant = "PCB",
  type = c(
    "location",
    "activity"),
  angle = 30,
  breaks = PCB_breaks,
  statistic = "prop.count",
  paddle = FALSE,
  key.title = paste0(PCB, " (pg/m³)"),
  key.position = "right")

# Save plot
ggsave(paste0("Output/Plots/Concentrations/WindRoseStyle/",PCB, "WindRose",
              "_Activity_Location.png"), plot = wrplot_PCB_activity$plot,
       width = 12, height = 12, dpi = 500, bg = "white")

# Function: activity-specific PCB pollution rose --------------------------
make_activity_windrose <- function(
    dataset,
    PCB,
    activity,
    output_dir = "Output/Plots/Concentrations/WindRoseStyle"
) {
  
  # Select the appropriate source-wind variables
  
  source_vars <- switch(
    activity,
    
    "Idle" = c(
      "HistoricalSourceWind_South",
      "HistoricalSourceWind_HS"),
    
    "Construction" = c(
      "ConstructionSourceWind_South",
      "ConstructionSourceWind_HS"),
    
    "Dredging" = c(
      "DredgingSourceWind_South",
      "DredgingSourceWind_HS"),
    
    stop("Activity must be Idle, Construction, or Dredging."))
  
  # PCB column names
  
  pcb_south <- paste0(PCB, "_South")
  pcb_hs <- paste0(PCB, "_HS")
  
  # Build dataset
  
  plot_dat <- dataset %>%
    
    filter(activity == !!activity) %>%
    
    select(
      wind_direction,
      wind_speed,
      all_of(c(
        pcb_south,
        pcb_hs,
        source_vars))
    ) %>%
    
    pivot_longer(
      cols = all_of(c(
        pcb_south,
        pcb_hs)),
      names_to = "location",
      values_to = "PCB"
    ) %>%
    
    mutate(
      
      # Location
      location = if_else(
        endsWith(location, "_South"), "South", "HS"),
      
      # Appropriate source-wind variable
      SourceWind = if_else(
        location == "South",
        as.character(.data[[source_vars[1]]]),
        as.character(.data[[source_vars[2]]])),
      
      location = factor(location, levels = c("South", "HS")),
      
      SourceWind = factor(
        SourceWind,
        levels = c("NonSource", "Source"))
    ) %>%
    
    filter(
      !is.na(PCB),
      !is.na(wind_direction),
      !is.na(wind_speed),
      !is.na(SourceWind)
    ) %>%
    
    transmute(wd = wind_direction, ws = wind_speed, PCB, location, SourceWind)
  
  # Concentration breaks
  
  PCB_upper <- ceiling(max(plot_dat$PCB, na.rm = TRUE))
  
  PCB_breaks <- c(0, 5, 10, 20, 40, 80, PCB_upper)
  
  # Remove duplicated/non-increasing breaks
  PCB_breaks <- sort(unique(PCB_breaks[PCB_breaks <= PCB_upper]))
  
  if (length(PCB_breaks) < 2) {
    stop(paste0("Not enough valid concentration breaks for ", PCB,
                " during ", activity))
  }
  
  # Pollution rose
  
  plot <- pollutionRose(plot_dat, pollutant = "PCB",
                        type = c("location", "SourceWind"),
                        angle = 30, breaks = PCB_breaks,
                        statistic = "prop.count", paddle = FALSE,
                        key.title = paste0(PCB, " (pg/m³)"),
                        key.position = "right")
  
  # Save
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  file_name <- paste0(PCB, "WindRose", activity, ".png")
  
  ggsave(file.path(output_dir, file_name), plot = plot$plot, width = 12,
         height = 12, dpi = 500, bg = "white")
  
  # Return object
  
  return(plot)
}

# Create the plots
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

wrplot_idle <- make_activity_windrose(
  dataset = dataset,
  PCB = PCB,
  activity = "Idle")

wrplot_construction <- make_activity_windrose(
  dataset = dataset,
  PCB = PCB,
  activity = "Construction")

wrplot_dredging <- make_activity_windrose(
  dataset = dataset,
  PCB = PCB,
  activity = "Dredging")

# Seasonality: day of year -----------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

season_dat <- dataset %>%
  select(
    date,
    day_of_year,
    activity,
    
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"),
    
    ActivitySourceWind = case_when(
      
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)),
    
    location = factor(
      location,
      levels = c("South", "HS")),
    
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(day_of_year),
    !is.na(ActivitySourceWind))

plot.doy.PCB <- ggplot(
  season_dat,
  aes(
    x = day_of_year,
    y = PCB,
    color = ActivitySourceWind
  )
) +
  geom_point(
    alpha = 0.40,
    size = 1.6
  ) +
  facet_grid(
    location ~ activity
  ) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = c(
      1, 32, 60, 91, 121, 152,
      182, 213, 244, 274, 305, 335
    ),
    labels = c(
      "Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug",
      "Sep", "Oct", "Nov", "Dec"
    )
  ) +
  labs(
    x = "Time of year",
    y = paste0(PCB, " concentration"),
    color = "Source wind"
  ) +
  theme_bw(base_size = 13)

plot.doy.PCB

# Save plot
ggsave(paste0("Output/Plots/Concentrations/Day_of_year/", PCB, "DayOfYear",
              ".png"), plot = plot.doy.PCB, width = 16, height = 8,
       dpi = 500, bg = "white")

# Water temperature -------------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

water_temp_dat <- dataset %>%
  select(
    date,
    activity,
    water_temp,
    
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS"))),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"),
    
    ActivitySourceWind = case_when(
      
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)),
    
    location = factor(
      location,
      levels = c("South", "HS")),
    
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source"))
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(water_temp),
    !is.na(ActivitySourceWind))

plot.water.temp.PCB <- ggplot(
  water_temp_dat,
  aes(
    x = water_temp,
    y = PCB,
    color = ActivitySourceWind
  )
) +
  geom_point(
    alpha = 0.40,
    size = 1.6
  ) +
  facet_grid(
    location ~ activity
  ) +
  scale_y_log10() +
  labs(
    x = "Water temperature (°C)",
    y = paste0(PCB, " concentration"),
    color = "Source wind"
  ) +
  theme_bw(base_size = 13)

plot.water.temp.PCB

# Save plot
ggsave(paste0("Output/Plots/Concentrations/WaterTemperature/", PCB,
              "WaterTemperature_", ".png"), plot = plot.water.temp.PCB,
       width = 12, height = 8, dpi = 500, bg = "white")

# Water flow (log10) Plots ------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

flow_dat <- dataset %>%
  select(
    date,
    activity,
    flow_cfs,
    
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS")
    )),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"
    ),
    
    ActivitySourceWind = case_when(
      
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)
    ),
    
    location = factor(
      location,
      levels = c("South", "HS")
    ),
    
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source")
    )
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(flow_cfs),
    !is.na(ActivitySourceWind)
  )

plot.flow.PCB <- ggplot(
  flow_dat,
  aes(
    x = flow_cfs,
    y = PCB,
    color = ActivitySourceWind
  )
) +
  geom_point(
    alpha = 0.40,
    size = 1.6
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_grid(
    location ~ activity
  ) +
  scale_y_log10() +
  labs(
    x = "Flow (cfs)",
    y = paste0(PCB, " concentration"),
    color = "Source wind"
  ) +
  theme_bw(base_size = 13)

plot.flow.PCB

ggsave(
  paste0(
    "Output/Plots/Concentrations/WaterFlow/", PCB, "FlowCFS",
    ".png"
  ),
  plot = plot.flow.PCB,
  width = 12,
  height = 8,
  dpi = 500,
  bg = "white"
)

# Turbidity Plots ---------------------------------------------------------
# Select PCB
# PCB <- "PCB8"
# PCB <- "PCB15"
# PCB <- "PCB18.30"
# PCB <- "PCB20.28"
# PCB <- "PCB31"

PCB <- "PCB31"

turb_dat <- dataset %>%
  select(
    date,
    activity,
    turb_FNU,
    
    all_of(paste0(PCB, "_South")),
    all_of(paste0(PCB, "_HS")),
    
    HistoricalSourceWind_South,
    HistoricalSourceWind_HS,
    
    ConstructionSourceWind_South,
    ConstructionSourceWind_HS,
    
    DredgingSourceWind_South,
    DredgingSourceWind_HS
  ) %>%
  pivot_longer(
    cols = all_of(c(
      paste0(PCB, "_South"),
      paste0(PCB, "_HS")
    )),
    names_to = "location",
    values_to = "PCB"
  ) %>%
  mutate(
    location = if_else(
      endsWith(location, "_South"),
      "South",
      "HS"
    ),
    
    ActivitySourceWind = case_when(
      activity == "Idle" &
        location == "South" ~
        as.character(HistoricalSourceWind_South),
      
      activity == "Idle" &
        location == "HS" ~
        as.character(HistoricalSourceWind_HS),
      
      activity == "Construction" &
        location == "South" ~
        as.character(ConstructionSourceWind_South),
      
      activity == "Construction" &
        location == "HS" ~
        as.character(ConstructionSourceWind_HS),
      
      activity == "Dredging" &
        location == "South" ~
        as.character(DredgingSourceWind_South),
      
      activity == "Dredging" &
        location == "HS" ~
        as.character(DredgingSourceWind_HS)
    ),
    
    location = factor(
      location,
      levels = c("South", "HS")
    ),
    
    ActivitySourceWind = factor(
      ActivitySourceWind,
      levels = c("NonSource", "Source")
    )
  ) %>%
  filter(
    !is.na(PCB),
    !is.na(turb_FNU),
    !is.na(ActivitySourceWind)
  )

plot.turb.PCB <- ggplot(
  turb_dat,
  aes(
    x = turb_FNU,
    y = PCB,
    color = ActivitySourceWind
  )
) +
  geom_point(
    alpha = 0.40,
    size = 1.6
  ) +
  facet_grid(
    location ~ activity
  ) +
  scale_y_log10() +
  labs(
    x = "Turbidity (FNU)",
    y = paste0(PCB, " concentration"),
    color = "Source wind"
  ) +
  theme_bw(base_size = 13)

plot.turb.PCB

ggsave(
  paste0(
    "Output/Plots/Concentrations/Turbidity/", PCB,"TurbidityFNU",
    ".png"
  ),
  plot = plot.turb.PCB,
  width = 12,
  height = 8,
  dpi = 500,
  bg = "white"
)


# Concentration Analysis - ACE data
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# https://indianaharbor.evs.anl.gov/data/ambient/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot2")
}

# Library
{
  library(dplyr)
  library(ggplot2)
}

# Read data ---------------------------------------------------------------
ace.raw <- read.csv("Data/Air/EastChicago/ACE/ACEDataV02.csv")
# Remove blanks cells
ace <- subset(ace.raw, !grepl("0", location))

# ACE Data ----------------------------------------------------------------
# Convert numeric date to Date format assuming Excel-style serial number
# ccvs file needs to be this format for the date XXXXX (e.g., 41188)
ace$date <- as.Date(ace$date, origin = "1899-12-30")
ace <- ace %>%
  mutate(
    PCB8_unc_label = factor(PCB8_unc, labels = c("< DL", "≥ DL")),
    PCB15_unc_label = factor(PCB15_unc, labels = c("< DL", "≥ DL")),
    PCB18.30_unc_label = factor(PCB18.30_unc, labels = c("< DL", "≥ DL")),
    PCB20.28_unc_label = factor(PCB20.28_unc, labels = c("< DL", "≥ DL")),
    PCB31_unc_label = factor(PCB31_unc, labels = c("< DL", "≥ DL"))
  )

# Both sites --------------------------------------------------------------
# Combining South locations into one
ace$location2 <- ifelse(grepl("South", ace$location), "South", "HS")
ace$location2 <- factor(ace$location2)

# Read activity data ------------------------------------------------------
activity_daily <- read.csv("Data/RemediationActivities/activity_dailyV2.csv")
# Change forma to date
activity_daily$date <- as.Date(activity_daily$date, origin = "1899-12-30")

# Plot
# PCB 8
p.pcb8 <- ggplot(ace, aes(x = date, y = PCB8)) +
  geom_tile(data = activity_daily,
            aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_point(aes(color = location2, shape = PCB8_unc_label),
             size = 2) +
  scale_fill_manual(name = "Project phase",
                    values = c("Idle" = "grey90",
                               "Construction" = "lightgreen",
                               "Dredging" = "#F4A6B7")) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4",
                                "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection",
                     values = c("< DL" = 19, "≥ DL" = 1),
                     na.translate = FALSE) +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration (pg/m3)") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                   size = 7, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "right", legend.key = element_blank())

p.pcb8

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB8_CDF_HSV2.png", plot = p.pcb8,
       width = 12, height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(ace, aes(x = date, y = PCB15)) +
  geom_tile(data = activity_daily,
            aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_point(aes(color = location2, shape = PCB15_unc_label),
             size = 2) +
  scale_fill_manual(name = "Project phase",
                    values = c("Idle" = "grey90",
                               "Construction" = "lightgreen",
                               "Dredging" = "#F4A6B7")) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4",
                                "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection",
                     values = c("< DL" = 19, "≥ DL" = 1),
                     na.translate = FALSE) +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration (pg/m3)") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                   size = 7, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "right", legend.key = element_blank())

# See plot
p.pcb15

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB15_CDF_HSV2.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
p.pcb18 <- ggplot(ace, aes(x = date, y = PCB18.30)) +
  geom_tile(data = activity_daily,
            aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_point(aes(color = location2, shape = PCB18.30_unc_label),
             size = 2) +
  scale_fill_manual(name = "Project phase",
                    values = c("Idle" = "grey90",
                               "Construction" = "lightgreen",
                               "Dredging" = "#F4A6B7")) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4",
                                "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection",
                     values = c("< DL" = 19, "≥ DL" = 1),
                     na.translate = FALSE) +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration (pg/m3)") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                   size = 7, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "right", legend.key = element_blank())

# See plot
p.pcb18

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB18_CDF_HSV2.png", plot = p.pcb18,
       width = 12, height = 4, dpi = 500)

# PCB 20+28
p.pcb20 <- ggplot(ace, aes(x = date, y = PCB20.28)) +
  geom_tile(data = activity_daily,
            aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_point(aes(color = location2, shape = PCB20.28_unc_label),
             size = 2) +
  scale_fill_manual(name = "Project phase",
                    values = c("Idle" = "grey90",
                               "Construction" = "lightgreen",
                               "Dredging" = "#F4A6B7")) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4",
                                "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection",
                     values = c("< DL" = 19, "≥ DL" = 1),
                     na.translate = FALSE) +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration (pg/m3)") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                   size = 7, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "right", legend.key = element_blank())

# See plot
p.pcb20

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB20_CDF_HSV2.png", plot = p.pcb20,
       width = 12, height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(ace, aes(x = date, y = PCB31)) +
  geom_tile(data = activity_daily,
            aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_point(aes(color = location2, shape = PCB31_unc_label),
             size = 2) +
  scale_fill_manual(name = "Project phase",
                    values = c("Idle" = "grey90",
                               "Construction" = "lightgreen",
                               "Dredging" = "#F4A6B7")) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4",
                                "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection",
                     values = c("< DL" = 19, "≥ DL" = 1),
                     na.translate = FALSE) +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration (pg/m3)") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                   size = 7, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "right", legend.key = element_blank())

# See plot
p.pcb31

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB31_CDF_HSV2.png", plot = p.pcb31,
       width = 12, height = 4, dpi = 500)

# Individual sites --------------------------------------------------------
# South
#PCB 8
p.pcb8 <- ggplot(subset(ace, location2 == "South"),
                 aes(x = date, y = PCB8)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB8_unc_label, fill = PCB8_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration @ CDF (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb8  

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB8_CDFV2.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(subset(ace, location2 == "South"),
                  aes(x = date, y = PCB15)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB15_unc_label, fill = PCB15_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration @ CDF (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb15

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB15_CDFV2.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
p.pcb18 <- ggplot(subset(ace, location2 == "South"),
                  aes(x = date, y = PCB18.30)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB18.30_unc_label, fill = PCB18.30_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration @ CDF (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb18

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB18_CDFV2.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB 20+28
p.pcb20 <- ggplot(subset(ace, location2 == "South"),
                  aes(x = date, y = PCB20.28)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB20.28_unc_label, fill = PCB20.28_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration @ CDF (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb20

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB20_CDFV2.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(subset(ace, location2 == "South"),
                  aes(x = date, y = PCB31)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB31_unc_label, fill = PCB31_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration @ CDF (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb31

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB31_CDFV2.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)

# HS
p.pcb8 <- ggplot(subset(ace, location2 == "HS"),
                 aes(x = date, y = PCB8)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB8_unc_label, fill = PCB8_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration @ HS (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb8

ggsave("Output/Plots/Concentrations/AcePCB8_HSV2.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(subset(ace, location2 == "HS"),
                  aes(x = date, y = PCB15)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB15_unc_label, fill = PCB15_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration @ HS (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb15

ggsave("Output/Plots/Concentrations/AcePCB15_HSV2.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18
p.pcb18 <- ggplot(subset(ace, location2 == "HS"),
                  aes(x = date, y = PCB18.30)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB18.30_unc_label, fill = PCB18.30_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration @ HS (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb18

ggsave("Output/Plots/Concentrations/AcePCB18_HSV2.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB20.28
p.pcb20 <- ggplot(subset(ace, location2 == "HS"),
                  aes(x = date, y = PCB20.28)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB20.28_unc_label, fill = PCB20.28_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration @ HS (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb20

ggsave("Output/Plots/Concentrations/AcePCB20_HSV2.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(subset(ace, location2 == "HS"),
                  aes(x = date, y = PCB31)) +
  geom_tile(data = activity_daily, aes(x = date, y = 0, fill = Activity),
            height = Inf, inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(name = "Project phase",
                    values = c("Dredging" = "#F4A6B7",
                               "Construction" = "lightgreen",
                               "Idle" = "white")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB31_unc_label, fill = PCB31_unc_label),
    color = "black", size = 2.5, stroke = 0.75, na.rm = TRUE) +
  scale_shape_manual(values = c("< DL" = 22, "≥ DL" = 21),
                     na.translate = FALSE) +
  scale_fill_manual(values = c("< DL" = NA, "≥ DL" = "#E69F00"),
                    na.translate = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration @ HS (pg/m3)") +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   color = "black", angle = 60,
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.position = "right",
        legend.key = element_blank())

# See plot
p.pcb31

ggsave("Output/Plots/Concentrations/AcePCB31_HSV2.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)



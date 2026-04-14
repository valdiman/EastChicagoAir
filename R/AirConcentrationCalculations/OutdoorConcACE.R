# Concentration Analysis - ACE data
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# https://indianaharbor.evs.anl.gov/data/ambient/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("ggnewscale")
}

# Library
{
  library(dplyr)
  library(scales)
  library(ggplot2)
  library(tidyr)
  library(ggnewscale)
}

# Read data ---------------------------------------------------------------
ace <- read.csv("Data/Air/IHSC/ACEData.csv")

# ACE Data ----------------------------------------------------------------
# Remove blanks cells
ace.1 <- subset(ace, !grepl("0", location))
# Change units to pg/m3 from ng/m3
ace.1 <- ace.1 %>%
  mutate(across(starts_with("PCB") & !ends_with("_unc"), ~ . / 1000))
ace.1 <- ace.1 %>%
  mutate(location = factor(location,levels = c("South", "South_CDF", "HS","Con_South")))  # Explicit factor levels
# Convert numeric date to Date format assuming Excel-style serial number
# ccvs file needs to be this format for the date XXXXX (e.g., 41188)
ace.1$date <- as.Date(ace.1$date, origin = "1899-12-30")
ace.1 <- ace.1 %>%
  mutate(
    PCB8_unc_label = factor(PCB8_unc, labels = c("≤ DL", "> DL")),
    PCB15_unc_label = factor(PCB15_unc, labels = c("≤ DL", "> DL")),
    PCB18.30_unc_label = factor(PCB18.30_unc, labels = c("≤ DL", "> DL")),
    PCB20.28_unc_label = factor(PCB20.28_unc, labels = c("≤ DL", "> DL")),
    PCB31_unc_label = factor(PCB31_unc, labels = c("≤ DL", "> DL"))
  )

# Both sites --------------------------------------------------------------
# Combining South locations into one
ace.1$location2 <- ifelse(grepl("South", ace.1$location), "South", "HS")
ace.1$location2 <- factor(ace.1$location2)

# Plots
#PCB 8
p.pcb8 <- ggplot(ace.1, aes(x = date, y = PCB8)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01","2014-05-01","2015-05-01",
        "2016-09-01","2017-09-01","2018-06-01","2019-06-01",
        "2020-10-01","2024-09-01","2003-07-01","2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31","2013-08-31","2014-07-31","2015-08-31",
        "2016-11-30","2017-12-31","2018-09-30","2019-10-31",
        "2020-12-31","2024-11-30","2010-12-31","2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction", "Phase 2 Dike Project")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c(
      "Dredging" = "#F4A6B7",
      "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  geom_point(aes(color = location2, shape = PCB8_unc_label), size = 2) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4", "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection", values = c("≤ DL" = 19, "> DL" = 1)) +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration (ng/m3)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(shape = 16)),
    shape = guide_legend(override.aes = list(color = "black")))

# See plot
p.pcb8

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB8_CDF_HS.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(ace.1, aes(x = date, y = PCB15)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01","2014-05-01","2015-05-01",
        "2016-09-01","2017-09-01","2018-06-01","2019-06-01",
        "2020-10-01","2024-09-01","2003-07-01","2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31","2013-08-31","2014-07-31","2015-08-31",
        "2016-11-30","2017-12-31","2018-09-30","2019-10-31",
        "2020-12-31","2024-11-30","2010-12-31","2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction", "Phase 2 Dike Project")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c(
      "Dredging" = "#F4A6B7",
      "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  geom_point(aes(color = location2, shape = PCB15_unc_label), size = 2) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4", "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection", values = c("≤ DL" = 19, "> DL" = 1)) +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration (ng/m3)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(shape = 16)),
    shape = guide_legend(override.aes = list(color = "black")))

# See plot
p.pcb15

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB15_CDF_HS.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
p.pcb18 <- ggplot(ace.1, aes(x = date, y = PCB18.30)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01","2014-05-01","2015-05-01",
        "2016-09-01","2017-09-01","2018-06-01","2019-06-01",
        "2020-10-01","2024-09-01","2003-07-01","2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31","2013-08-31","2014-07-31","2015-08-31",
        "2016-11-30","2017-12-31","2018-09-30","2019-10-31",
        "2020-12-31","2024-11-30","2010-12-31","2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction", "Phase 2 Dike Project")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c(
      "Dredging" = "#F4A6B7",
      "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  geom_point(aes(color = location2, shape = PCB18.30_unc_label), size = 2) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4", "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection", values = c("≤ DL" = 19, "> DL" = 1)) +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration (ng/m3)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(shape = 16)),
    shape = guide_legend(override.aes = list(color = "black")))

# See plot
p.pcb18

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB18_CDF_HS.png", plot = p.pcb18,
       width = 12, height = 4, dpi = 500)

# PCB 20+28
p.pcb20 <- ggplot(ace.1, aes(x = date, y = PCB20.28)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01","2014-05-01","2015-05-01",
        "2016-09-01","2017-09-01","2018-06-01","2019-06-01",
        "2020-10-01","2024-09-01","2003-07-01","2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31","2013-08-31","2014-07-31","2015-08-31",
        "2016-11-30","2017-12-31","2018-09-30","2019-10-31",
        "2020-12-31","2024-11-30","2010-12-31","2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction", "Phase 2 Dike Project")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c(
      "Dredging" = "#F4A6B7",
      "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  geom_point(aes(color = location2, shape = PCB20.28_unc_label), size = 2) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4", "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection", values = c("≤ DL" = 19, "> DL" = 1)) +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration (ng/m3)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(shape = 16)),
    shape = guide_legend(override.aes = list(color = "black")))

# See plot
p.pcb20

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB20_CDF_HS.png", plot = p.pcb20,
       width = 12, height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(ace.1, aes(x = date, y = PCB31)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01","2014-05-01","2015-05-01",
        "2016-09-01","2017-09-01","2018-06-01","2019-06-01",
        "2020-10-01","2024-09-01","2003-07-01","2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31","2013-08-31","2014-07-31","2015-08-31",
        "2016-11-30","2017-12-31","2018-09-30","2019-10-31",
        "2020-12-31","2024-11-30","2010-12-31","2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction", "Phase 2 Dike Project")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c(
      "Dredging" = "#F4A6B7",
      "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  geom_point(aes(color = location2, shape = PCB31_unc_label), size = 2) +
  scale_color_manual(name = "Location",
                     values = c("South" = "#00BFC4", "HS" = "#E69F00")) +
  scale_shape_manual(name = "Detection", values = c("≤ DL" = 19, "> DL" = 1)) +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration (ng/m3)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(shape = 16)),
    shape = guide_legend(override.aes = list(color = "black")))

# See plot
p.pcb31

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB31_CDF_HS.png", plot = p.pcb31,
       width = 12, height = 4, dpi = 500)

# Individual sites --------------------------------------------------------
# South
#PCB 8
p.pcb8 <- ggplot(subset(ace.1, location2 == "South"),
                 aes(x = date, y = PCB8)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
        "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
      "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB8_unc_label, fill = PCB8_unc_label),
    color = "black", size = 2.5, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration @ CDF (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb8

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB8_CDF.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(subset(ace.1, location2 == "South"),
                  aes(x = date, y = PCB15)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB15_unc_label, fill = PCB15_unc_label),
    color = "black", size = 2.5, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration @ CDF (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7,color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb15

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB15_CDF.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
p.pcb18 <- ggplot(subset(ace.1, location2 == "South"),
                  aes(x = date, y = PCB18.30)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB18.30_unc_label, fill = PCB18.30_unc_label),
    color = "black", size = 2.5, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration @ CDF (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7,color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb18

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB18_CDF.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB 20+28
p.pcb20 <- ggplot(subset(ace.1, location2 == "South"),
                  aes(x = date, y = PCB20.28)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB20.28_unc_label, fill = PCB20.28_unc_label),
    color = "black", size = 2.5, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration @ CDF (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb20

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB20_CDF.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(subset(ace.1, location2 == "South"),
                  aes(x = date, y = PCB31)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB31_unc_label, fill = PCB31_unc_label),
    color = "black", size = 2.5, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration @ CDF (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb31

# Export plot
ggsave("Output/Plots/Concentrations/AcePCB31_CDF.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)

# HS
p.pcb8 <- ggplot(subset(ace.1, location2 == "HS"),
                 aes(x = date, y = PCB8)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB8_unc_label, fill = PCB8_unc_label),
    color = "black", size = 2, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 8 concentration @ HS (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb8

ggsave("Output/Plots/Concentrations/AcePCB8_HS.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(subset(ace.1, location2 == "HS"),
                  aes(x = date, y = PCB15)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB15_unc_label, fill = PCB15_unc_label),
    color = "black", size = 2, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 15 concentration @ HS (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb15

ggsave("Output/Plots/Concentrations/AcePCB15_HS.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18
p.pcb18 <- ggplot(subset(ace.1, location2 == "HS"),
                  aes(x = date, y = PCB18.30)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB18.30_unc_label, fill = PCB18.30_unc_label),
    color = "black", size = 2, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 18+30 concentration @ HS (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb18

ggsave("Output/Plots/Concentrations/AcePCB18_HS.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB20.28
p.pcb20 <- ggplot(subset(ace.1, location2 == "HS"),
                  aes(x = date, y = PCB20.28)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB20.28_unc_label, fill = PCB20.28_unc_label),
    color = "black", size = 2, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 20+28 concentration @ HS (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb20

ggsave("Output/Plots/Concentrations/AcePCB20_HS.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(subset(ace.1, location2 == "HS"),
                  aes(x = date, y = PCB31)) +
  geom_rect(
    data = data.frame(
      xmin = as.Date(c(
        "2012-11-01","2013-04-01", "2014-05-01", "2015-05-01",
        "2016-09-01", "2017-09-01", "2018-06-01", "2019-06-01",
        "2020-10-01", "2024-09-01", "2003-07-01", "2021-07-01")),
      xmax = as.Date(c(
        "2012-12-31", "2013-08-31", "2014-07-31", "2015-08-31",
        "2016-11-30", "2017-12-31", "2018-09-30", "2019-10-31",
        "2020-12-31", "2024-11-30", "2010-12-31", "2024-06-30")),
      band = c(rep("Dredging", 10), "CDF Dike Construction",
               "Phase 2 Dike Project")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
    inherit.aes = FALSE, alpha = 0.3) +
  scale_fill_manual(
    name = "Project phase",
    values = c("Dredging" = "#F4A6B7", "CDF Dike Construction" = "lightgreen",
               "Phase 2 Dike Project" = "lightblue")) +
  ggnewscale::new_scale_fill() +
  geom_point(
    aes(shape = PCB31_unc_label, fill = PCB31_unc_label),
    color = "black", size = 2, stroke = 0.75) +
  scale_shape_manual(values = c("≤ DL" = 22, "> DL" = 21)) +
  scale_fill_manual(values = c("≤ DL" = NA, "> DL" = "#E69F00")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() +
  labs(x = "", y = "PCB 31 concentration @ HS (ng/m3)") +
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black",
                               angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank())

# See plot
p.pcb31

ggsave("Output/Plots/Concentrations/AcePCB31_HS.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)



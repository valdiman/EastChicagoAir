# concentration Analysis
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# https://indianaharbor.evs.anl.gov/data/ambient/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("dplyr")
install.packages("ggplot")

# Library
{
  library(dplyr)
  library(scales)
  library(ggplot2)
  library(tidyr)
}

# Read data ---------------------------------------------------------------
ace <- read.csv("Data/ACEData.csv")

# ACE Data ----------------------------------------------------------------
# Remove blanks cells
ace.1 <- subset(ace, !grepl("0", location))
# Change units to pg/m3 from ng/m3
ace.1 <- ace.1 %>%
  mutate(across(starts_with("PCB") & !ends_with("_unc"), ~ . / 1000))
ace.1 <- ace.1 %>%
  mutate(location = factor(location, levels = c("South", "South_CDF", "HS")))  # Explicit factor levels
# Convert numeric date to Date format assuming Excel-style serial number
ace.1$date <- as.Date(ace.1$date, origin = "1899-12-30")
ace.1 <- ace.1 %>%
  mutate(
    PCB8_unc_label = factor(PCB8_unc, labels = c("≤ DL", "> DL")),
    PCB15_unc_label = factor(PCB15_unc, labels = c("≤ DL", "> DL")),
    PCB18.30_unc_label = factor(PCB18.30_unc, labels = c("≤ DL", "> DL")),
    PCB20.28_unc_label = factor(PCB20.28_unc, labels = c("≤ DL", "> DL")),
    PCB31_unc_label = factor(PCB31_unc, labels = c("≤ DL", "> DL"))
  )

# Plots -------------------------------------------------------------------
#PCB 8
p.pcb8 <- ggplot(ace.1, aes(x = as.Date(date), y = PCB8)) +
  geom_point(aes(shape = PCB8_unc_label, fill = location,
                 color = location), size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_shape_manual(values = c("≤ DL" = 21, "> DL" = 1)) +
  theme_bw() +
  labs(x  = "", y = "PCB 8 concentration (ng/m3)", fill = "Location", color = "Location") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()
  ) +
  guides(
    shape = "none",  # Remove the shape legend for DL uncertainty
    fill = guide_legend(override.aes = list(shape = 1, size = 3)),
    color = guide_legend(override.aes = list(shape = 1, size = 3))
  )

# See plot
p.pcb8

# Linear regression analysis
lr.PCB8 <- lm(
  log10(PCB8) ~ date,
  data = ace.1,
  subset = PCB8 > 0 & !is.na(PCB8) & !is.na(date)
)
summary(lr.PCB8)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB8.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
p.pcb15 <- ggplot(ace.1, aes(x = as.Date(date), y = PCB15)) +
  geom_point(aes(shape = PCB15_unc_label, fill = location,
                 color = location), size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_shape_manual(values = c("≤ DL" = 21, "> DL" = 1)) +
  theme_bw() +
  labs(x  = "", y = "PCB 15 concentration (ng/m3)", fill = "Location", color = "Location") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()
  ) +
  guides(
    shape = "none",  # Remove the shape legend for DL uncertainty
    fill = guide_legend(override.aes = list(shape = 1, size = 3)),
    color = guide_legend(override.aes = list(shape = 1, size = 3))
  )

# See plot
p.pcb15

# Linear regression analysis
lr.PCB15 <- lm(
  log10(PCB15) ~ date,
  data = ace.1,
  subset = PCB15 > 0 & !is.na(PCB8) & !is.na(date)
)
summary(lr.PCB15)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB15.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
p.pcb18 <- ggplot(ace.1, aes(x = as.Date(date), y = PCB18.30)) +
  geom_point(aes(shape = PCB18.30_unc_label, fill = location,
                 color = location), size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_shape_manual(values = c("≤ DL" = 21, "> DL" = 1)) +
  theme_bw() +
  labs(x  = "", y = "PCBs 18 + 30 concentration (ng/m3)", fill = "Location", color = "Location") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()
  ) +
  guides(
    shape = "none",  # Remove the shape legend for DL uncertainty
    fill = guide_legend(override.aes = list(shape = 1, size = 3)),
    color = guide_legend(override.aes = list(shape = 1, size = 3))
  )

# See plot
p.pcb18

# Linear regression analysis
lr.PCB18 <- lm(
  log10(PCB18.30) ~ date,
  data = ace.1,
  subset = PCB18.30 > 0 & !is.na(PCB18.30) & !is.na(date)
)
summary(lr.PCB18)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB18.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB 20+28
p.pcb20 <- ggplot(ace.1, aes(x = as.Date(date), y = PCB20.28)) +
  geom_point(aes(shape = PCB20.28_unc_label, fill = location,
                 color = location), size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_shape_manual(values = c("≤ DL" = 21, "> DL" = 1)) +
  theme_bw() +
  labs(x  = "", y = "PCBs 20 + 28 concentration (ng/m3)", fill = "Location", color = "Location") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()
  ) +
  guides(
    shape = "none",  # Remove the shape legend for DL uncertainty
    fill = guide_legend(override.aes = list(shape = 1, size = 3)),
    color = guide_legend(override.aes = list(shape = 1, size = 3))
  )

# See plot
p.pcb20

# Linear regression analysis
lr.PCB20 <- lm(
  log10(PCB20.28) ~ date,
  data = ace.1,
  subset = PCB20.28 > 0 & !is.na(PCB20.28) & !is.na(date)
)
summary(lr.PCB20)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB20.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
p.pcb31 <- ggplot(ace.1, aes(x = as.Date(date), y = PCB31)) +
  geom_point(aes(shape = PCB31_unc_label, fill = location,
                 color = location), size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue", "HS" = "#E69F00")) +
  scale_shape_manual(values = c("≤ DL" = 21, "> DL" = 1)) +
  theme_bw() +
  labs(x  = "", y = "PCB 31 concentration (ng/m3)", fill = "Location", color = "Location") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle = 60, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.key = element_blank()
  ) +
  guides(
    shape = "none",  # Remove the shape legend for DL uncertainty
    fill = guide_legend(override.aes = list(shape = 1, size = 3)),
    color = guide_legend(override.aes = list(shape = 1, size = 3))
  )

# See plot
p.pcb31

# Linear regression analysis
lr.PCB31 <- lm(
  log10(PCB31) ~ date,
  data = ace.1,
  subset = PCB31 > 0 & !is.na(PCB31) & !is.na(date)
)
summary(lr.PCB31)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AcePCB31.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)

# Concentration Analysis
# Both ACE and AESOP PAS-PUF data
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

# Read Excel data ---------------------------------------------------------
ace <- read.csv("Data/ACEData.csv")
aesop <- read.csv("Output/Data/csv/ConcentrationPUF.csv")

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
names(ace.1)[names(ace.1) == "location"] <- "source"

# AESOP data --------------------------------------------------------------
# Use only MERRA and Ampleman
aesop_V2 <- aesop %>% filter(Meteo %in% c("MERRA", "Ampleman"))
aesop_V2$DateDeploy <- as.Date(aesop_V2$DateDeploy,
                             origin = "1899-12-30")
aesop_V2$DateCollect <- as.Date(aesop_V2$DateCollect,
                                origin = "1899-12-30")
aesop_V2$source <- "aesop"

# Both data set -----------------------------------------------------------
# PCB 8
# aesop
aesop.PCB8 <- aesop_V2 %>%
  select("source", "DateCollect", "PCB8")

# ace
ace.PCB8 <- data.frame(source = ace.1$source,
                       DateCollect = ace.1$date,
                       PCB8 = ace.1$PCB8)

# Combine the datasets
combined_data <- rbind(aesop.PCB8, ace.PCB8)

plot.pcb8 <- ggplot(combined_data, aes(x = DateCollect, y = PCB8,
                                       color = source)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  labs(x = "", y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"),
                     labels = c("South" = "ACE (South)",
                                "South_CDF" = "ACE (South CDF)",
                                "HS" = "ACE (HS)",
                                "aesop" = "AESOP"))

# See plot
plot.pcb8

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB8.png", plot = plot.pcb8, width = 12,
       height = 4, dpi = 500)

# PCB 15
# aesop
aesop.PCB15 <- aesop_V2 %>%
  select("source", "DateCollect", "PCB15")

# ace
ace.PCB15 <- data.frame(source = ace.1$source,
                        DateCollect = ace.1$date,
                        PCB15 = ace.1$PCB15)

# Combine the datasets
combined_data <- rbind(aesop.PCB15, ace.PCB15)

p.pcb15 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB15,
                                     color = source)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  labs(x = "", y = "PCB 15 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"),
                     labels = c("South" = "ACE (South)",
                                "South_CDF" = "ACE (South CDF)",
                                "HS" = "ACE (HS)",
                                "aesop" = "AESOP"))

# See plot
p.pcb15

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB15.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
# aesop
aesop.PCB18 <- aesop_V2 %>%
  select("source", "DateCollect", "PCB18.30")

# ace
ace.PCB18 <- data.frame(source = ace.1$source,
                       DateCollect = ace.1$date,
                       PCB18.30 = ace.1$PCB18.30)

# Combine the datasets
combined_data <- rbind(aesop.PCB18, ace.PCB18)

p.pcb18 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB18.30,
                                     color = source)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  labs(x = "", y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"),
                     labels = c("South" = "ACE (South)",
                                "South_CDF" = "ACE (South CDF)",
                                "HS" = "ACE (HS)",
                                "aesop" = "AESOP"))

# See plot
p.pcb18

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB18.png", plot = p.pcb18, width = 12,
       height = 4, dpi = 500)

# PCB 20+28
# aesop
aesop.PCB20 <- aesop_V2 %>%
  select("source", "DateCollect", "PCB20.28")

# ace
ace.PCB20 <- data.frame(source = ace.1$source,
                        DateCollect = ace.1$date,
                        PCB20.28 = ace.1$PCB20.28)

# Combine the datasets
combined_data <- rbind(aesop.PCB20, ace.PCB20)

p.pcb20 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB20.28,
                                     color = source)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  labs(x = "", y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"),
                     labels = c("South" = "ACE (South)",
                                "South_CDF" = "ACE (South CDF)",
                                "HS" = "ACE (HS)",
                                "aesop" = "AESOP"))

# See plot
p.pcb20

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB20.png", plot = p.pcb20, width = 12,
       height = 4, dpi = 500)

# PCB 31
# aesop
aesop.PCB31 <- aesop_V2 %>%
  select("source", "DateCollect", "PCB31")

# ace
ace.PCB31 <- data.frame(source = ace.1$source,
                        DateCollect = ace.1$date,
                        PCB31 = ace.1$PCB31)

# Combine the datasets
combined_data <- rbind(aesop.PCB31, ace.PCB31)

p.pcb31 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB31,
                                     color = source)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  labs(x = "", y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"),
                     labels = c("South" = "ACE (South)",
                                "South_CDF" = "ACE (South CDF)",
                                "HS" = "ACE (HS)",
                                "aesop" = "AESOP"))

# See plot
p.pcb31

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB31.png", plot = p.pcb31, width = 12,
       height = 4, dpi = 500)

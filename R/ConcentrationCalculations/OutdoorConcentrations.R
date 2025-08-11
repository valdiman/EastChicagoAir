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

# AESOP data --------------------------------------------------------------
# Use only MERRA and Ampleman
aesop_V2 <- aesop %>% filter(Meteo %in% c("MERRA", "Ampleman"))
aesop_V2$DateDeploy <- as.Date(aesop_V2$DateDeploy,
                             origin = "1899-12-30")
aesop_V2$DateCollect <- as.Date(aesop_V2$DateCollect,
                                origin = "1899-12-30")

# Both data set -----------------------------------------------------------
# PCB 8
# aesop
aesop.PCB8 <- aesop_V2 %>%
  select("DateCollect", "PCB8")
aesop.PCB8$source <- "aesop"
aesop.PCB8 <- aesop.PCB8 %>%
  select("source", "DateCollect", "PCB8")

# ace
ace.PCB8 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB8 = ace.1$PCB8)
ace.PCB8_subset <- ace.PCB8[, c("location", "DateCollect", "PCB8")]
names(ace.PCB8)[names(ace.PCB8) == "location"] <- "source"

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
  select("DateCollect", "PCB15")
aesop.PCB15$location <- "aesop"
aesop.PCB15 <- aesop.PCB15 %>%
  select("location", "DateCollect", "PCB15")

# ace
ace.PCB15 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB15 = ace.1$PCB15)
ace.PCB15_subset <- ace.PCB15[, c("location", "DateCollect", "PCB15")]
# Combine the datasets
combined_data <- rbind(aesop.PCB15, ace.PCB15_subset)

p.pcb15 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB15, color = location)) +
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
                                "HS" = "#E69F00", "aesop" = "red"))

# See plot
p.pcb15

# Save plot in folder
ggsave("Output/Plots/Concentrations/AceAesopPCB15.png", plot = p.pcb15, width = 12,
       height = 4, dpi = 500)

# PCB 18+30
# aesop
aesop.PCB18 <- aesop_V2 %>%
  select("DateCollect", "PCB18.30")
names(aesop.PCB18$PCB18.30) <- "PCB18"

aesop.PCB18$location <- "aesop"
aesop.PCB18 <- aesop.PCB18 %>%
  select("location", "DateCollect", "PCB18.30")
# Convert to date
aesop.PCB18$DateCollect <- as.Date(aesop.PCB18$DateCollect,
                                   origin = "1899-12-30")

ace.PCB18 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                        PCB18 = ace.1$PCB18.30)
ace.PCB18_subset <- ace.PCB18[, c("location", "DateCollect", "PCB18")]
# Combine the datasets
combined_data <- rbind(aesop.PCB18, ace.PCB18_subset)

ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB18, color = location)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  theme_bw() +
  labs(x = "", y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"))

lr.PCB18 <- lm(
  log10(PCB18) ~ DateCollect,
  data = combined_data,
  subset = PCB18 > 0 & !is.na(PCB18) & !is.na(DateCollect)
)

summary(lr.PCB18)

# PCB 20+28
aesop.PCB28.merra <- aesop.PCB28 %>% 
  filter(Meteo == "MERRA")
aesop.PCB28.merra$location <- "aesop"
ace.PCB28 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB28 = ace.1$PCB20.28)
# Select relevant columns
aesop.PCB28_subset <- aesop.PCB28.merra[, c("location", "DateCollect", "PCB28")]
ace.PCB28_subset <- ace.PCB28[, c("location", "DateCollect", "PCB28")]
# Combine the datasets
combined_data <- rbind(aesop.PCB28_subset, ace.PCB28_subset)

ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB28, color = location)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCBs 20 + 28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"))
  
# PCB 31
aesop.PCB31.merra <- aesop.PCB31 %>% 
  filter(Meteo == "MERRA")
aesop.PCB31.merra$location <- "aesop"
ace.PCB31 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                        PCB31 = ace.1$PCB31)
# Select relevant columns
aesop.PCB31_subset <- aesop.PCB31.merra[, c("location", "DateCollect", "PCB31")]
ace.PCB31_subset <- ace.PCB31[, c("location", "DateCollect", "PCB31")]
# Combine the datasets
combined_data <- rbind(aesop.PCB31_subset, ace.PCB31_subset)

plot.pcb31 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB31, color = location)) +
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
                                "HS" = "#E69F00", "aesop" = "red"))

# See plot
plot.pcb31

# Save plot in folder
ggsave("Output/Plots/Concentrations/PCB31.png", plot = plot.pcb31, width = 12,
       height = 4, dpi = 500)  

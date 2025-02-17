# concentration Analysis
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/
# https://indianaharbor.evs.anl.gov/data/ambient/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot")

# Library
{
  library(readxl) # to read excel files
  library(dplyr)
  library(scales)
  library(ggplot2)
  library(tidyr)
}

# Read Excel data ---------------------------------------------------------
ace <- data.frame(read_xlsx("Data/ACEData.xlsx", sheet = "ace"))
aesop <- data.frame(read_xlsx("Output/Data/excel/Concentration_Calculations.xlsx",
                          sheet = "Concentration_ng_m3"))

# ACE Data ----------------------------------------------------------------
# Remove blanks cells
ace.1 <- subset(ace, !grepl("0", location))
# Change units to ng/m3 from ng/m3
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

#PCB 8
ggplot(ace.1, aes(x = as.Date(date), y = PCB8)) +
  geom_point(aes(shape = PCB8_unc_label, fill = location,
                 color = location), size = 2.5, stroke = 1.3) +
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

# PCB 15
ggplot(ace.1, aes(x = as.Date(date), y = PCB15)) +
  geom_point(aes(shape = PCB15_unc_label, fill = location,
                 color = location), size = 2.5, stroke = 1.3) +
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

# PCB 18+30
ggplot(ace.1, aes(x = as.Date(date), y = PCB18.30)) +
  geom_point(aes(shape = PCB18.30_unc_label, fill = location,
                 color = location), size = 2.5, stroke = 1.3) +
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

# PCB 20+28
ggplot(ace.1, aes(x = as.Date(date), y = PCB20.28)) +
  geom_point(aes(shape = PCB20.28_unc_label, fill = location,
                 color = location), size = 2.5, stroke = 1.3) +
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

# PCB 31
ggplot(ace.1, aes(x = as.Date(date), y = PCB31)) +
  geom_point(aes(shape = PCB31_unc_label, fill = location,
                 color = location), size = 2.5, stroke = 1.3) +
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

# AESOP data --------------------------------------------------------------
# tPCB
aesop.tPCB <- rowSums(aesop[, 4:176], na.rm = TRUE)
aesop.tPCB <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, tPCB = aesop.tPCB)

ggplot(aesop.tPCB, aes(x = Meteo, y = tPCB)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "tPCB concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Use only MERRA and Ampleman
aesop_V2 <- aesop %>% filter(Meteo %in% c("MERRA", "Ampleman"))

# tPCB
aesop_V2.tPCB <- rowSums(aesop_V2[, 4:176], na.rm = TRUE)
aesop_V2.tPCB <- data.frame(DateDploy = aesop_V2$DateDeploy,
                            DateCollect = aesop_V2$DateCollect,
                            tPCB = aesop_V2.tPCB)

# Convert DateCollect to Date format
aesop_V2.tPCB$DateCollect <- as.Date(aesop_V2.tPCB$DateCollect)

ggplot(subset(aesop_V2.tPCB), aes(x = DateCollect, y = tPCB)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "tPCB concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.tPCB <- lm(tPCB ~ DateCollect, data = subset(aesop_V2.tPCB))
summary(lr.tPCB)


# Need to change to aesop_V2
# PCB8
aesop.PCB8 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB8 = aesop$PCB8)

ggplot(aesop.PCB8, aes(x = Meteo, y = PCB8)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 8 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB8$DateCollect <- as.Date(aesop.PCB8$DateCollect)

ggplot(subset(aesop.PCB8, Meteo == "MERRA"), aes(x = DateCollect, y = PCB8)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB8 <- lm(log10(PCB8) ~ DateCollect, data = subset(aesop.PCB8, Meteo == "MERRA"))
summary(lr.PCB8)

# PCB11
aesop.PCB11 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB11 = aesop$PCB11)

ggplot(aesop.PCB11, aes(x = Meteo, y = PCB11)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 11 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB11$DateCollect <- as.Date(aesop.PCB11$DateCollect)

ggplot(subset(aesop.PCB11, Meteo == "MERRA"), aes(x = DateCollect, y = PCB11)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCB 11 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB11 <- lm(log10(PCB11) ~ DateCollect, data = subset(aesop.PCB11, Meteo == "MERRA"))
summary(lr.PCB11)

# PCB15
aesop.PCB15 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB15 = aesop$PCB15)

ggplot(aesop.PCB15, aes(x = Meteo, y = PCB15)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 15 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB15$DateCollect <- as.Date(aesop.PCB15$DateCollect)

ggplot(subset(aesop.PCB15, Meteo == "MERRA"), aes(x = DateCollect, y = PCB15)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCB 15 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB15 <- lm(log10(PCB15) ~ DateCollect, data = subset(aesop.PCB15, Meteo == "MERRA"))
summary(lr.PCB15)

# PCB18+30
aesop.PCB18 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB18 = aesop$PCB18.30)

ggplot(aesop.PCB18, aes(x = Meteo, y = PCB18)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCBs 18 + 30 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB18$DateCollect <- as.Date(aesop.PCB18$DateCollect)

ggplot(subset(aesop.PCB18, Meteo == "MERRA"), aes(x = DateCollect, y = PCB18)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB18 <- lm(log10(PCB18) ~ DateCollect, data = subset(aesop.PCB18, Meteo == "MERRA"))
summary(lr.PCB18)

# PCB20+28
aesop.PCB28 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB28 = aesop$PCB20.28)

ggplot(aesop.PCB28, aes(x = Meteo, y = PCB28)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 28 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB28$DateCollect <- as.Date(aesop.PCB28$DateCollect)

ggplot(subset(aesop.PCB28, Meteo == "MERRA"), aes(x = DateCollect, y = PCB28)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB28 <- lm(log10(PCB28) ~ DateCollect, data = subset(conc.PCB28, Meteo == "MERRA"))
summary(lr.PCB28)

# PCB31
aesop.PCB31 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB31 = aesop$PCB31)

ggplot(aesop.PCB31, aes(x = Meteo, y = PCB31)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 31 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB31$DateCollect <- as.Date(aesop.PCB31$DateCollect)

ggplot(subset(aesop.PCB31, Meteo == "MERRA"), aes(x = DateCollect, y = PCB31)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB31 <- lm(log10(PCB31) ~ DateCollect, data = subset(aesop.PCB31, Meteo == "MERRA"))
summary(lr.PCB31)

# PCB52
aesop.PCB52 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB52 = aesop$PCB52)

ggplot(aesop.PCB52, aes(x = Meteo, y = PCB52)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 52 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB52$DateCollect <- as.Date(aesop.PCB52$DateCollect)

ggplot(subset(aesop.PCB52, Meteo == "MERRA"), aes(x = DateCollect, y = PCB52)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCB 52 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB52 <- lm(log10(PCB52) ~ DateCollect, data = subset(aesop.PCB52, Meteo == "MERRA"))
summary(lr.PCB52)

# PCB61+70+74+76
aesop.PCB61 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, PCB61 = aesop$PCB61.70.74.76)

ggplot(aesop.PCB61, aes(x = Meteo, y = PCB61)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCBs 61+70+74+76 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.PCB61$DateCollect <- as.Date(aesop.PCB61$DateCollect)

ggplot(subset(aesop.PCB61, Meteo == "MERRA"), aes(x = DateCollect, y = PCB61)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "PCBs 61+70+74+76 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.PCB61 <- lm(log10(PCB61) ~ DateCollect, data = subset(aesop.PCB61, Meteo == "MERRA"))
summary(lr.PCB61)

# Both data set -----------------------------------------------------------
# PCB 8
# aesop
aesop.PCB8 <- aesop_V2 %>%
  select("DateCollect", "PCB8")
aesop.PCB8$location <- "aesop"
aesop.PCB8 <- aesop.PCB8 %>%
  select("location", "DateCollect", "PCB8")
# ace
ace.PCB8 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB8 = ace.1$PCB8)
ace.PCB8_subset <- ace.PCB8[, c("location", "DateCollect", "PCB8")]
# Combine the datasets
combined_data <- rbind(aesop.PCB8, ace.PCB8_subset)

plot.pcb8 <- ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB8,
                                       color = location)) +
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
                                "HS" = "#E69F00", "aesop" = "red"))

# See plot
plot.pcb8

# Save plot in folder
ggsave("Output/Plots/Concentrations/PCB8V02.png", plot = plot.pcb8, width = 12,
       height = 4, dpi = 500)  

# PCB 15
aesop.PCB15.merra <- aesop.PCB15 %>% 
  filter(Meteo == "MERRA")
aesop.PCB15.merra$location <- "aesop"
ace.PCB15 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB15 = ace.1$PCB15)
# Select relevant columns
aesop.PCB15_subset <- aesop.PCB15.merra[, c("location", "DateCollect", "PCB15")]
ace.PCB15_subset <- ace.PCB15[, c("location", "DateCollect", "PCB15")]
# Combine the datasets
combined_data <- rbind(aesop.PCB15_subset, ace.PCB15_subset)

ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB15, color = location)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
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

# PCB 18+30
aesop.PCB18.merra <- aesop.PCB18 %>% 
  filter(Meteo == "MERRA")
aesop.PCB18.merra$location <- "aesop"
ace.PCB18 <- data.frame(location = ace.1$location, DateCollect = ace.1$date,
                       PCB18 = ace.1$PCB18.30)
# Select relevant columns
aesop.PCB18_subset <- aesop.PCB18.merra[, c("location", "DateCollect", "PCB18")]
ace.PCB18_subset <- ace.PCB18[, c("location", "DateCollect", "PCB18")]
# Combine the datasets
combined_data <- rbind(aesop.PCB18_subset, ace.PCB18_subset)

ggplot(combined_data, aes(x = as.Date(DateCollect), y = PCB18, color = location)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCBs 18 + 30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("South" = "#00BFC4", "South_CDF" = "blue",
                                "HS" = "#E69F00", "aesop" = "red"))

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

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
aesop <- data.frame(read_xlsx("Output/Data/excel/Concentration_Calculations.xlsx",
                          sheet = "Concentration_ng_m3"))
ace <- data.frame(read_xlsx("Data/ACEData.xlsx", sheet = "ace"))

# AESOP data --------------------------------------------------------------
# tPCB
aesop.tPCB <- rowSums(aesop[, 4:176], na.rm = TRUE)
aesop.tPCB <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                        DateCollect = aesop$DateCollect, aesop.tPCB)
colnames(aesop.tPCB)[colnames(aesop.tPCB) == "aesop.tPCB"] <- "tPCB"

ggplot(aesop.tPCB, aes(x = Meteo, y = tPCB)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "tPCB concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
aesop.tPCB$DateCollect <- as.Date(aesop.tPCB$DateCollect)

ggplot(subset(aesop.tPCB, Meteo == "MERRA"), aes(x = DateCollect, y = tPCB)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(y = "tPCB concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

lr.tPCB <- lm(log10(tPCB) ~ DateCollect, data = subset(aesop.tPCB, Meteo == "MERRA"))
summary(lr.tPCB)

# PCB8
aesop.PCB8 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, aesop$PCB8)
colnames(aesop.PCB8)[colnames(aesop.PCB8) == "aesop.PCB8"] <- "PCB8"

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

ggplot(subset(conc.PCB8, Meteo == "MERRA"), aes(x = DateCollect, y = PCB8)) +
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
conc.PCB11 <- aesop$PCB11
conc.PCB11 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, conc.PCB11)
colnames(conc.PCB11)[colnames(conc.PCB11) == "conc.PCB11"] <- "PCB11"

ggplot(conc.PCB11, aes(x = Meteo, y = PCB11)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 11 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB11$DateCollect <- as.Date(conc.PCB11$DateCollect)

ggplot(subset(conc.PCB11, Meteo == "MERRA"), aes(x = DateCollect, y = PCB11)) +
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

lr.PCB11 <- lm(log10(PCB11) ~ DateCollect, data = subset(conc.PCB11, Meteo == "MERRA"))
summary(lr.PCB11)

# PCB15
conc.PCB15 <- aesop$PCB15
conc.PCB15 <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                        DateCollect = conc$DateCollect, conc.PCB15)
colnames(conc.PCB15)[colnames(conc.PCB15) == "conc.PCB15"] <- "PCB15"

ggplot(conc.PCB15, aes(x = Meteo, y = PCB15)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 15 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB15$DateCollect <- as.Date(conc.PCB15$DateCollect)

ggplot(subset(conc.PCB15, Meteo == "MERRA"), aes(x = DateCollect, y = PCB15)) +
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

lr.PCB15 <- lm(log10(PCB15) ~ DateCollect, data = subset(conc.PCB15, Meteo == "MERRA"))
summary(lr.PCB15)

# PCB18+30
conc.PCB18 <- conc$PCB18.30
conc.PCB18 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB18)
colnames(conc.PCB18)[colnames(conc.PCB18) == "conc.PCB18"] <- "PCB18"

ggplot(conc.PCB18, aes(x = Meteo, y = PCB18)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 18 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB18$DateCollect <- as.Date(conc.PCB18$DateCollect)

ggplot(subset(conc.PCB18, Meteo == "MERRA"), aes(x = DateCollect, y = PCB18)) +
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

lr.PCB18 <- lm(log10(PCB18) ~ DateCollect, data = subset(conc.PCB18, Meteo == "MERRA"))
summary(lr.PCB18)

# PCB20+28
conc.PCB28 <- conc$PCB20.28
conc.PCB28 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB28)
colnames(conc.PCB28)[colnames(conc.PCB28) == "conc.PCB28"] <- "PCB28"

ggplot(conc.PCB28, aes(x = Meteo, y = PCB28)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 28 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB28$DateCollect <- as.Date(conc.PCB28$DateCollect)

ggplot(subset(conc.PCB28, Meteo == "MERRA"), aes(x = DateCollect, y = PCB28)) +
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
conc.PCB31 <- conc$PCB31
conc.PCB31 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB31)
colnames(conc.PCB31)[colnames(conc.PCB31) == "conc.PCB31"] <- "PCB31"

ggplot(conc.PCB31, aes(x = Meteo, y = PCB31)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 31 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB31$DateCollect <- as.Date(conc.PCB31$DateCollect)

ggplot(subset(conc.PCB31, Meteo == "MERRA"), aes(x = DateCollect, y = PCB31)) +
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

lr.PCB31 <- lm(log10(PCB31) ~ DateCollect, data = subset(conc.PCB31, Meteo == "MERRA"))
summary(lr.PCB31)

# PCB52
conc.PCB52 <- conc$PCB52
conc.PCB52 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                        DateCollect = conc$DateCollect, conc.PCB52)
colnames(conc.PCB52)[colnames(conc.PCB52) == "conc.PCB52"] <- "PCB52"

ggplot(conc.PCB52, aes(x = Meteo, y = PCB52)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 52 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB52$DateCollect <- as.Date(conc.PCB52$DateCollect)

ggplot(subset(conc.PCB52, Meteo == "MERRA"), aes(x = DateCollect, y = PCB52)) +
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

lr.PCB52 <- lm(log10(PCB52) ~ DateCollect, data = subset(conc.PCB52, Meteo == "MERRA"))
summary(lr.PCB52)

# PCB61+70+74+76
conc.PCB61 <- conc$PCB61.70.74.76
conc.PCB61 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB61)
colnames(conc.PCB61)[colnames(conc.PCB61) == "conc.PCB61"] <- "PCB61"

ggplot(conc.PCB61, aes(x = Meteo, y = PCB61)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCBs 61+70+74+76 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB61$DateCollect <- as.Date(conc.PCB61$DateCollect)

ggplot(subset(conc.PCB61, Meteo == "MERRA"), aes(x = DateCollect, y = PCB61)) +
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

lr.PCB61 <- lm(log10(PCB61) ~ DateCollect, data = subset(conc.PCB61, Meteo == "MERRA"))
summary(lr.PCB61)

# ACE Data ----------------------------------------------------------------
# Remove blanks cells
ace.1 <- subset(ace, !grepl("^1899-", date))
# Change units to ng/m3 from ng/m3
ace.1 <- ace.1 %>%
  mutate(across(starts_with("PCB"), ~ . / 1000))

#PCB 8
ggplot(ace.1, aes(x = as.Date(date), y = PCB8, fill = location)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.1, PCB8 > 0)
lr.PCB8 <- lm(log10(PCB8) ~ date, data = ace.1_filtered)
summary(lr.PCB8)

# PCB 15
ggplot(ace.1, aes(x = as.Date(date), y = PCB15)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 15 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.1, PCB15 > 0)
lr.PCB15 <- lm(log10(PCB15) ~ date, data = ace.1_filtered)
summary(lr.PCB15)

# PCB 18.30
ggplot(ace.1, aes(x = as.Date(date), y = PCB18.30)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.1, PCB18.30 > 0)
lr.PCB18 <- lm(log10(PCB18.30) ~ date, data = ace.1_filtered)
summary(lr.PCB18)

# PCB 20.28
ggplot(ace.1, aes(x = as.Date(date), y = PCB20.28)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.1, PCB20.28 > 0)
lr.PCB20 <- lm(log10(PCB20.28) ~ date, data = ace.1_filtered)
summary(lr.PCB20)

# PCB 31
ggplot(ace.1, aes(x = as.Date(date), y = PCB31)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.1, PCB31 > 0)
lr.PCB31 <- lm(log10(PCB31) ~ date, data = ace.1_filtered)
summary(lr.PCB31)

# Both data set -----------------------------------------------------------
# PCB 8
conc.PCB8$Source <- "MERRA"
ace.1$Source <- "ACE"
# Standardize date columns
conc.PCB8$Date <- conc.PCB8$DateCollect
ace.1$Date <- as.Date(ace.1$date)
# Select relevant columns
conc.PCB8_subset <- conc.PCB8[, c("Date", "PCB8", "Source")]
ace.1_subset <- ace.1[, c("Date", "PCB8", "Source")]
# Combine the datasets
combined_data <- rbind(conc.PCB8_subset, ace.1_subset)

ggplot(combined_data, aes(x = as.Date(Date), y = PCB8, color = Source, fill = Source)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen")) +  # Custom colors
  scale_fill_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen"))  # Matching fill colors

# PCB 15
conc.PCB15$Source <- "MERRA"
ace.1$Source <- "ACE"
# Standardize date columns
conc.PCB15$Date <- conc.PCB15$DateCollect
ace.1$Date <- as.Date(ace.1$date)
# Select relevant columns
conc.PCB15_subset <- conc.PCB15[, c("Date", "PCB15", "Source")]
ace.1_subset <- ace.1[, c("Date", "PCB15", "Source")]
# Combine the datasets
combined_data <- rbind(conc.PCB15_subset, ace.1_subset)

ggplot(combined_data, aes(x = as.Date(Date), y = PCB15, color = Source, fill = Source)) +
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
  scale_color_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen")) +  # Custom colors
  scale_fill_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen"))  # Matching fill colors

# PCB 18.30
conc.PCB18$Source <- "MERRA"
ace.1$Source <- "ACE"
# Standardize date columns
conc.PCB18$Date <- conc.PCB18$DateCollect
ace.1$Date <- as.Date(ace.1$date)
# Select relevant columns
conc.PCB18_subset <- conc.PCB18[, c("Date", "PCB18", "Source")]
ace.1_subset <- ace.1[, c("Date", "PCB18.30", "Source")]
colnames(ace.1_subset)[colnames(ace.1_subset) == "PCB18.30"] <- "PCB18"
# Combine the datasets
combined_data <- rbind(conc.PCB18_subset, ace.1_subset)

ggplot(combined_data, aes(x = as.Date(Date), y = PCB18, color = Source, fill = Source)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCB 18 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen")) +  # Custom colors
  scale_fill_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen"))  # Matching fill colors

# PCB 20.28
conc.PCB28$Source <- "MERRA"
ace.1$Source <- "ACE"
# Standardize date columns
conc.PCB28$Date <- conc.PCB28$DateCollect
ace.1$Date <- as.Date(ace.1$date)
# Select relevant columns
conc.PCB28_subset <- conc.PCB28[, c("Date", "PCB28", "Source")]
ace.1_subset <- ace.1[, c("Date", "PCB20.28", "Source")]
colnames(ace.1_subset)[colnames(ace.1_subset) == "PCB20.28"] <- "PCB28"
# Combine the datasets
combined_data <- rbind(conc.PCB28_subset, ace.1_subset)

ggplot(combined_data, aes(x = as.Date(Date), y = PCB28, color = Source, fill = Source)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen")) +  # Custom colors
  scale_fill_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen"))  # Matching fill colors

# PCB 31
conc.PCB31$Source <- "MERRA"
ace.1$Source <- "ACE"
# Standardize date columns
conc.PCB31$Date <- conc.PCB31$DateCollect
ace.1$Date <- as.Date(ace.1$date)
# Select relevant columns
conc.PCB31_subset <- conc.PCB31[, c("Date", "PCB31", "Source")]
ace.1_subset <- ace.1[, c("Date", "PCB31", "Source")]
# Combine the datasets
combined_data <- rbind(conc.PCB31_subset, ace.1_subset)

ggplot(combined_data, aes(x = as.Date(Date), y = PCB31, color = Source, fill = Source)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3, alpha = 0.7) +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "", y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen")) +  # Custom colors
  scale_fill_manual(values = c("MERRA" = "blue", "ACE" = "darkgreen"))  # Matching fill colors

# East-CDF ----------------------------------------------------------------
ACE.2 <- data.frame(read_xlsx("Data/ACEData.xlsx", sheet = "EAST-CDF"))
# Remove blanks cells
ace.2 <- subset(ACE.2, !grepl("^1899-", date))
# Change units to ng/m3 from ng/m3
ace.2 <- ace.2 %>%
  mutate(across(starts_with("PCB"), ~ . / 1000))

#PCB 8
ggplot(ace.2, aes(x = as.Date(date), y = PCB8)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.2, PCB8 > 0)
lr.PCB8 <- lm(log10(PCB8) ~ date, data = ace.1_filtered)
summary(lr.PCB8)

# PCB 15
ggplot(ace.2, aes(x = as.Date(date), y = PCB15)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 15 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.2, PCB15 > 0)
lr.PCB15 <- lm(log10(PCB15) ~ date, data = ace.1_filtered)
summary(lr.PCB15)

# PCB 18.30
ggplot(ace.2, aes(x = as.Date(date), y = PCB18.30)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.2, PCB18.30 > 0)
lr.PCB18 <- lm(log10(PCB18.30) ~ date, data = ace.1_filtered)
summary(lr.PCB18)

# PCB 20.28
ggplot(ace.2, aes(x = as.Date(date), y = PCB20.28)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.2, PCB20.28 > 0)
lr.PCB20 <- lm(log10(PCB20.28) ~ date, data = ace.1_filtered)
summary(lr.PCB20)

# PCB 31
ggplot(ace.2, aes(x = as.Date(date), y = PCB31)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x  = "", y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 7, color = "black",
                                   angle = 60, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

ace.1_filtered <- subset(ace.2, PCB31 > 0)
lr.PCB31 <- lm(log10(PCB31) ~ date, data = ace.1_filtered)
summary(lr.PCB31)

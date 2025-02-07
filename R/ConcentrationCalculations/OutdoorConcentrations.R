# concentration Analysis
# https://indianaharbor.evs.anl.gov/about-project/timeline/index.cfm
# https://indianaharbor.evs.anl.gov/dredging/

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("readxl")
install.packages("dplyr")

# Library
{
  library(readxl) # to read excel files
  library(dplyr)
  library(scales)
}

# Read Excel data ---------------------------------------------------------
conc <- data.frame(read_xlsx("Output/Data/excel/Concentration_Calculations.xlsx",
                          sheet = "Concentration_ng_m3"))

# Plots -------------------------------------------------------------------
# tPCB
conc.tPCB <- rowSums(conc[, 4:176], na.rm = TRUE)
conc.tPCB <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                        DateCollect = conc$DateCollect, conc.tPCB)
colnames(conc.tPCB)[colnames(conc.tPCB) == "conc.tPCB"] <- "tPCB"

ggplot(conc.tPCB, aes(x = Meteo, y = tPCB)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "tPCB concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.tPCB$DateCollect <- as.Date(conc.tPCB$DateCollect)

ggplot(subset(conc.tPCB, Meteo == "MERRA"), aes(x = DateCollect, y = tPCB)) +
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

lr.tPCB <- lm(log10(tPCB) ~ DateCollect, data = subset(conc.tPCB, Meteo == "MERRA"))
summary(lr.tPCB)

# PCB8
conc.PCB8 <- conc$PCB8
conc.PCB8 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB8)
colnames(conc.PCB8)[colnames(conc.PCB8) == "conc.PCB8"] <- "PCB8"

ggplot(conc.PCB8, aes(x = Meteo, y = PCB8)) +
  geom_boxplot() +
  theme_classic() +
  labs( x = "Meteorology", y = "PCB 8 concentration (ng/m3)") + 
  theme(axis.text.x = element_text(face = "bold", size = 9,
                                   color = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# Convert DateCollect to Date format
conc.PCB8$DateCollect <- as.Date(conc.PCB8$DateCollect)

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

lr.PCB8 <- lm(log10(PCB8) ~ DateCollect, data = subset(conc.PCB8, Meteo == "MERRA"))
summary(lr.PCB8)

# PCB11
conc.PCB11 <- conc$PCB11
conc.PCB11 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
                         DateCollect = conc$DateCollect, conc.PCB11)
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
conc.PCB15 <- conc$PCB15
conc.PCB15 <- data.frame(Meteo = conc$Meteo, DateDploy = conc$DateDeploy,
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




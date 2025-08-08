# Concentration Analysis
# AESOP PAS-PUF data

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
aesop <- read.csv("Output/Data/csv/ConcentrationPUF.csv")

# tPCB
aesop.tPCB <- rowSums(aesop[, 4:176], na.rm = TRUE)
aesop.tPCB <- data.frame(Meteo = aesop$Meteo, DateDploy = aesop$DateDeploy,
                         DateCollect = aesop$DateCollect, tPCB = aesop.tPCB)

# Plot function of meteorological data
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
aesop_V2.tPCB$DateCollect <- as.Date(aesop_V2.tPCB$DateCollect,
                                     origin = "1899-12-30")

p.tpcb <- ggplot(subset(aesop_V2.tPCB), aes(x = DateCollect, y = tPCB)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "tPCB concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", linewidth = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.tpcb

# Linear regression analysis
lr.tPCB <- lm(tPCB ~ DateCollect, data = subset(aesop_V2.tPCB))
summary(lr.tPCB)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesoptPCB.png", plot = p.tpcb, width = 12,
       height = 4, dpi = 500)
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
aesop.PCB8$DateCollect <- as.Date(aesop.PCB8$DateCollect,
                                  origin = "1899-12-30")

p.pcb8 <- ggplot(subset(aesop.PCB8, Meteo == "MERRA"), aes(x = DateCollect, y = PCB8)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCB 8 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))
# See plot
p.pcb8

# Linear regression analysis
lr.PCB8 <- lm(log10(PCB8) ~ DateCollect,
              data = subset(aesop.PCB8, Meteo == "MERRA"))
summary(lr.PCB8)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB8.png", plot = p.pcb8, width = 12,
       height = 4, dpi = 500)

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
aesop.PCB11$DateCollect <- as.Date(aesop.PCB11$DateCollect,
                                  origin = "1899-12-30")

p.pcb11 <- ggplot(subset(aesop.PCB11, Meteo == "MERRA"), aes(x = DateCollect, y = PCB11)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCB 11 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb11

# Linear regression analysis
lr.PCB11 <- lm(log10(PCB11) ~ DateCollect,
               data = subset(aesop.PCB11, Meteo == "MERRA"))
summary(lr.PCB11)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB11.png",
       plot = p.pcb11, width = 12, height = 4, dpi = 500)

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
aesop.PCB15$DateCollect <- as.Date(aesop.PCB15$DateCollect,
                                  origin = "1899-12-30")

p.pcb15 <- ggplot(subset(aesop.PCB15, Meteo == "MERRA"), aes(x = DateCollect, y = PCB15)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCB 15 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb15

# Linear regression analysis
lr.PCB15 <- lm(log10(PCB15) ~ DateCollect,
               data = subset(aesop.PCB15, Meteo == "MERRA"))
summary(lr.PCB15)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB15.png",
       plot = p.pcb15, width = 12, height = 4, dpi = 500)

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
aesop.PCB18$DateCollect <- as.Date(aesop.PCB18$DateCollect,
                                  origin = "1899-12-30")

p.pcb18 <- ggplot(subset(aesop.PCB18, Meteo == "MERRA"), aes(x = DateCollect, y = PCB18)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCBs 18+30 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb18

# Linear regression analysis
lr.PCB18 <- lm(log10(PCB18) ~ DateCollect,
               data = subset(aesop.PCB18, Meteo == "MERRA"))
summary(lr.PCB18)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB18.png",
       plot = p.pcb18, width = 12, height = 4, dpi = 500)

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
aesop.PCB28$DateCollect <- as.Date(aesop.PCB28$DateCollect,
                                   origin = "1899-12-30")

p.pcb28 <- ggplot(subset(aesop.PCB28, Meteo == "MERRA"), aes(x = DateCollect, y = PCB28)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCBs 20+28 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb28
# Linear regression analysis
lr.PCB28 <- lm(log10(PCB28) ~ DateCollect,
               data = subset(aesop.PCB28, Meteo == "MERRA"))
summary(lr.PCB28)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB28.png",
       plot = p.pcb28, width = 12, height = 4, dpi = 500)

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
aesop.PCB31$DateCollect <- as.Date(aesop.PCB31$DateCollect,
                                   origin = "1899-12-30")

p.pcb31 <- ggplot(subset(aesop.PCB31, Meteo == "MERRA"), aes(x = DateCollect, y = PCB31)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCB 31 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb31

# Linear regression analysis
lr.PCB31 <- lm(log10(PCB31) ~ DateCollect,
               data = subset(aesop.PCB31, Meteo == "MERRA"))
summary(lr.PCB31)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB31.png",
       plot = p.pcb31, width = 12, height = 4, dpi = 500)

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
aesop.PCB52$DateCollect <- as.Date(aesop.PCB52$DateCollect,
                                   origin = "1899-12-30")

p.pcb52 <- ggplot(subset(aesop.PCB52, Meteo == "MERRA"), aes(x = DateCollect, y = PCB52)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCB 52 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb52

# Linear regression analysis
lr.PCB52 <- lm(log10(PCB52) ~ DateCollect,
               data = subset(aesop.PCB52, Meteo == "MERRA"))
summary(lr.PCB52)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB52.png",
       plot = p.pcb52, width = 12, height = 4, dpi = 500)

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
aesop.PCB61$DateCollect <- as.Date(aesop.PCB61$DateCollect,
                                   origin = "1899-12-30")

p.pcb61 <- ggplot(subset(aesop.PCB61, Meteo == "MERRA"), aes(x = DateCollect, y = PCB61)) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_bw() +
  theme(aspect.ratio = 1/2) +
  labs(x = "Collection date") +
  labs(y = "PCBs 61+70+74+76 concentration (ng/m3)") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date("2012-10-29"), color = "red", 
             linetype = "dashed", size = 1) + # start of dredging
  theme(axis.text.x = element_text(face = "bold", size = 9, color = "black"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 11))

# See plot
p.pcb61

# Linear regression analysis
lr.PCB61 <- lm(log10(PCB61) ~ DateCollect,
               data = subset(aesop.PCB61, Meteo == "MERRA"))
summary(lr.PCB61)

# Save plot in folder
ggsave("Output/Plots/Concentrations/AesopPCB61.png",
       plot = p.pcb61, width = 12, height = 4, dpi = 500)

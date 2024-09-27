# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

# Library
{
  library(readxl) # to read excel files
  library(dplyr)
  library(ggplot2)
}

# Read Excel a data ---------------------------------------------------------
a.1 <- data.frame(read_xlsx("Output/Data/Sample_aV01.xlsx",
                            sheet = "FinalMassCorrected",
                            col_names = TRUE, col_types = NULL))
a.2 <- data.frame(read_xlsx("Output/Data/Sample_aV02.xlsx",
                            sheet = "FinalMassCorrected",
                            col_names = TRUE, col_types = NULL))

mass.a.1 <- data.frame(t(a.1[176, -1]), Type = "Mass.a.1")
# Move row names to the first column
mass.a.1$RowNames <- rownames(mass.a.1)
rownames(mass.a.1) <- NULL
mass.a.2 <- data.frame(t(a.2[176, -1]), Type = "Mass.a.2")
# Move row names to the first column
mass.a.2$RowNames <- rownames(mass.a.2)
rownames(mass.a.2) <- NULL
mass.combined <- rbind(mass.a.1, mass.a.2)

# Example plot with adjusted width
plot.mass.a <- ggplot(data = mass.combined, aes(x = RowNames, y = X176, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust width here
  theme_bw() +
  theme(aspect.ratio = 25/135,
        axis.text.x = element_text(face = "bold", size = 4, angle = 60, hjust = 1,
                                   color = "black"),
        axis.text.y = element_text(face = "bold", size = 8, color = "black"),
        axis.title.y = element_text(face = "bold", size = 8, color = "black"),
        axis.ticks = element_line(linewidth = 0.6, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top") +
  labs(x = "", y = expression(bold("Mass " *Sigma*"PCB (ng/PUF)"))) +
  scale_fill_manual(values = c("Mass.a.1" = "blue", "Mass.a.2" = "red"))  # Adjust colors here

# See plot
plot.mass.a

# Save plot in folder
ggsave("Output/Plots/Mass_a.png", plot = plot.mass.a, width = 10,
       height = 5, dpi = 300)

plot.mass.a.log10 <- ggplot(data = mass.combined, aes(x = RowNames, y = log10(X176), fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust width here
  theme_bw() +
  theme(aspect.ratio = 25/135,
        axis.text.x = element_text(face = "bold", size = 4, angle = 60, hjust = 1,
                                   color = "black"),
        axis.text.y = element_text(face = "bold", size = 8, color = "black"),
        axis.title.y = element_text(face = "bold", size = 8, color = "black"),
        axis.ticks = element_line(linewidth = 0.6, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top") +
  labs(x = "", y = expression(bold("Mass " *Sigma*"PCB (log 10 ng/PUF)"))) +
  scale_fill_manual(values = c("Mass.a.1" = "blue", "Mass.a.2" = "red"))  # Adjust colors here

# See plot
plot.mass.a.log10

# Save plot in folder
ggsave("Output/Plots/Mass_a_log10.png", plot = plot.mass.a.log10, width = 10,
       height = 5, dpi = 300)

# Read Excel m data ---------------------------------------------------------
m.1 <- data.frame(read_xlsx("Output/Data/Sample_mV01.xlsx",
                            sheet = "FinalMassCorrected",
                            col_names = TRUE, col_types = NULL))
m.2 <- data.frame(read_xlsx("Output/Data/Sample_mV02.xlsx",
                            sheet = "FinalMassCorrected",
                            col_names = TRUE, col_types = NULL))

mass.m.1 <- data.frame(t(m.1[176, -1]), Type = "Mass.m.1")
# Move row names to the first column
mass.m.1$RowNames <- rownames(mass.m.1)
rownames(mass.m.1) <- NULL
mass.m.2 <- data.frame(t(m.2[176, -1]), Type = "Mass.m.2")
# Move row names to the first column
mass.m.2$RowNames <- rownames(mass.m.2)
rownames(mass.m.2) <- NULL
mass.combined <- rbind(mass.m.1, mass.m.2)

# Example plot with adjusted width
plot.mass.m <- ggplot(data = mass.combined, aes(x = RowNames, y = X176, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust width here
  theme_bw() +
  theme(aspect.ratio = 25/135,
        axis.text.x = element_text(face = "bold", size = 4, angle = 60, hjust = 1,
                                   color = "black"),
        axis.text.y = element_text(face = "bold", size = 8, color = "black"),
        axis.title.y = element_text(face = "bold", size = 8, color = "black"),
        axis.ticks = element_line(linewidth = 0.6, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top") +
  labs(x = "", y = expression(bold("Mass " *Sigma*"PCB (ng/PUF)"))) +
  scale_fill_manual(values = c("Mass.m.1" = "blue", "Mass.m.2" = "red"))  # Adjust colors here

# See plot
plot.mass.m

# Save plot in folder
ggsave("Output/Plots/Mass_m.png", plot = plot.mass.m, width = 10,
       height = 5, dpi = 300)

plot.mass.m.log10 <- ggplot(data = mass.combined, aes(x = RowNames, y = log10(X176), fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust width here
  theme_bw() +
  theme(aspect.ratio = 25/135,
        axis.text.x = element_text(face = "bold", size = 4, angle = 60, hjust = 1,
                                   color = "black"),
        axis.text.y = element_text(face = "bold", size = 8, color = "black"),
        axis.title.y = element_text(face = "bold", size = 8, color = "black"),
        axis.ticks = element_line(linewidth = 0.6, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top") +
  labs(x = "", y = expression(bold("Mass " *Sigma*"PCB (log 10 ng/PUF)"))) +
  scale_fill_manual(values = c("Mass.m.1" = "blue", "Mass.m.2" = "red"))  # Adjust colors here

# See plot
plot.mass.m.log10

# Save plot in folder
ggsave("Output/Plots/Mass_m_log10.png", plot = plot.mass.m.log10, width = 10,
       height = 5, dpi = 300)



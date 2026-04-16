# Soil air exchange calculations

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages('dplyr')
  install.packages("tidyverse")
}

# Libraries
{
  library(dplyr)
  library(tidyr)
}

# Read data ---------------------------------------------------------------
pcb.s <- read.csv("Output/Data/Soil/PCBEastChicagoSoil.csv")
pcb.a <- read.csv("Output/Data/Air/ConcentrationPUF.csv")
pc    <- read.csv("Data/PCProperties/PhysicalChemicalProperties.csv")

# Use only MERRA and Ampleman for air
pcb.a <- pcb.a %>%
  filter(Meteo %in% c("MERRA", "Ampleman"))

# Congeners in common
soil_pcb <- names(pcb.s)[grepl("^PCB", names(pcb.s))]
air_pcb  <- names(pcb.a)[grepl("^PCB", names(pcb.a))]
common_pcb <- intersect(soil_pcb, air_pcb)

# Soil subset
pcb.s2 <- pcb.s %>%
  select(Sample.label, Longitude, Latitude, distancefork, toc, all_of(common_pcb)) %>%
  filter(!is.na(toc))

# Mean air concentrations by congener
pcb.a_mean <- pcb.a %>%
  summarise(across(all_of(common_pcb), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "congener", values_to = "Cair_ng_m3")

# Long table
pcb_long <- pcb.s2 %>%
  pivot_longer(
    cols = all_of(common_pcb),
    names_to = "congener",
    values_to = "Csoil_ng_g"
  ) %>%
  left_join(pc, by = "congener") %>%
  left_join(pcb.a_mean, by = "congener")

# Constants ---------------------------------------------------------------
R <- 8.314
T <- 298

# Bulk density for concentration conversion
rho_b_kg_m3 <- 1300
rho_b_kg_L  <- rho_b_kg_m3 / 1000

pcb_long <- pcb_long %>%
  mutate(
    foc = toc / 100,
    
    # Published fugacity capacities
    Za  = 1 / (R * T),
    Kaw = 10^logKaw,
    H   = Kaw * R * T,
    Zw  = 1 / H,
    
    # Soil-water partitioning in the published form
    Koc = 10^(0.94 * logKow + 0.42),   # L/kg
    Kd  = foc * Koc,                   # L/kg
    Zs  = Kd * rho_b_kg_L * Zw,        # mol m^-3 Pa^-1
    
    # Convert measured concentrations to mol/m3
    MW_kg_mol     = MW / 1000,
    Cair_mol_m3   = Cair_ng_m3  * 1e-12 / MW_kg_mol,
    Csoil_mol_m3  = Csoil_ng_g  * 1e-6 * rho_b_kg_m3 / MW_kg_mol,
    
    # Compartment fugacities
    f_air  = Cair_mol_m3 / Za,
    f_soil = Csoil_mol_m3 / Zs,
    
    f_ratio = f_soil / f_air
  )

# Quick checks ------------------------------------------------------------
summary(pcb_long$f_ratio)

pcb_long %>%
  group_by(congener) %>%
  summarise(
    median_f_ratio = median(f_ratio, na.rm = TRUE),
    mean_f_ratio   = mean(f_ratio, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(median_f_ratio)

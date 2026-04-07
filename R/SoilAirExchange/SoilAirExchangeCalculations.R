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

# Check PCBs
soil_pcb <- names(pcb.s)[grep("^PCB", names(pcb.s))]
air_pcb  <- names(pcb.a)[grep("^PCB", names(pcb.a))]
common_pcb <- intersect(soil_pcb, air_pcb)

# Soil and air subsets
pcb.s2 <- pcb.s[, c("Sample.label", "Longitude", "Latitude", "distancefork", "toc", common_pcb)] %>%
  dplyr::filter(!is.na(toc))

pcb.a2 <- pcb.a[, common_pcb]

pcb.a_mean <- data.frame(
  congener = common_pcb,
  Cair_ng_m3 = apply(pcb.a2[, common_pcb], 2, mean, na.rm = TRUE)
)

# Long soil table + properties
pcb_long <- pcb.s2 %>%
  pivot_longer(
    cols = all_of(common_pcb),
    names_to = "congener",
    values_to = "Csoil_ng_g"
  ) %>%
  left_join(pc, by = "congener") %>%
  left_join(pcb.a_mean, by = "congener")

# Physical-chemical calculations ------------------------------------------
R <- 8.314
T <- 298
rho_b <- 1300  # kg/m3

pcb_long <- pcb_long %>%
  mutate(
    foc = toc / 100,
    Koc = 10^(0.94 * logKow + 0.42),
    
    # Convert measured concentrations to molar basis
    Cair_mol_m3  = Cair_ng_m3 * 1e-9 / MW,
    Csoil_mol_m3 = Csoil_ng_g * rho_b * 1e-6 / MW,
    
    # Capacity terms
    Zair  = 1 / (R * T),
    Zsoil = rho_b * foc * Koc,
    
    # Fugacity-like quantities
    f_air  = Cair_mol_m3 / Zair,
    f_soil = Csoil_mol_m3 / Zsoil
  )


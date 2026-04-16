# R codes for download and read data from Chicago Air PCB paper

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages('pangaear')
  install.packages("tidyverse")
}

# Libraries
{
  library(pangaear) # Read data from Pangaea
  library(tidyverse) # data manipulation
}

# Read data from Pangaea and format data ------------------------------
# Citation:
# Martinez, Andres; Awad, Andrew M; Hornbuckle, Keri C (2021): Concentrations of
# individual polychlorinated biphenyl congeners in gas phases in air in Chicago, Il,
# USA in 2009 [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.935207,
# In: Martinez, A et al. (2021): Concentrations of individual polychlorinated
# biphenyl congeners in gas and particle phases in air in Chicago, Il, USA
# in 2009 [dataset bundled publication]. PANGAEA, https://doi.org/10.1594/PANGAEA.935238

# Set cache path to the project folder
pg_cache$cache_path_set(full_path = "Data/Air/Chicago")

# Download original datasets from Pangaea
air.0 <- pg_data(doi = '10.1594/PANGAEA.935238') # entire dataset

# Obtain air PCB concentrations from Pangaea dataset
air.1 <- data.frame(air.0[[3]]$data) # ng/g

# Format concentration data for plotting
# Remove metadata from air PCB concentration dataset
air.1 <- air.1[, (names(air.1) %in% c("Sample.label", "T.air..K...average..Weather.station.mete....",
                                      "PCB..pg.m..3...PCB8..Calculated..see.Comment.",
                                      "PCB..pg.m..3...PCB15..Calculated..see.Comment.",
                                      "PCB..pg.m..3...PCB18.30..Calculated..see.Com....",
                                      "PCB..pg.m..3...PCB20.28..Calculated..see.Com....",
                                      "PCB..pg.m..3...PCB31..Calculated..see.Comment."))]

# Rename column names
colnames(air.1) <- c("sample", "air_temp", "PCB8", "PCB15", "PCB18.30", "PCB20.28", "PCB31")


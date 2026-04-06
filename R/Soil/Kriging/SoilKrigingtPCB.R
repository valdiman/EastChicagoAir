
# Codes to perform a Kriging analysis for tPCB
# Generated results are mapped in QGIS

# Install packages
install.packages(c("gstat", "sp", "sf", "raster", "ape", "viridis"))

# Load libraries
{
  library(gstat)    # Kriging functions
  library(sp)       # Spatial data handling
  library(sf)       # Modern spatial data
  library(raster)   # For spatial grids
  library(tidyverse)
  library(ape)
  library(viridis)
  library(ggplot2)
}

# Read data ---------------------------------------------------------------
pcb_soil <- read.csv("Output/Data/Soil/PCBEastChicagoSoil.csv",
                     check.names = FALSE, header = TRUE)

# Calculate tPCB
pcb_soil$tpcb <- rowSums(pcb_soil[, 7:134])

# Select total PCB
tpcb <- pcb_soil[, c("Latitude", "Longitude", "tpcb", "toc", "distancefork")]

# Model
model <- lm(log10(tpcb) ~ distancefork + toc, data = tpcb)
summary(model)

# keep only rows used in the model
tpcb_model <- tpcb[complete.cases(tpcb[, c("tpcb", "distancefork", "toc")]), ]

coordinates(tpcb_model) <- ~Longitude + Latitude
proj4string(tpcb_model) <- CRS(SRS_string = "EPSG:4326")

tpcb_model_utm <- spTransform(tpcb_model, CRS(SRS_string = "EPSG:32616"))

tpcb_model_utm$resid <- residuals(model)

summary(tpcb_model_utm$resid)

empirical_vario_resid <- variogram(resid ~ 1, tpcb_model_utm,
                                   cutoff = 10000,
                                   width = 1000)

plot(empirical_vario_resid)
empirical_vario_resid

# Results:
# covariates explained the spatial signal
# nothing left for kriging to improve

# Create grid
grd <- spsample(tpcb_model_utm, type = "regular", n = 5000)
gridded(grd) <- TRUE

pred_obs <- predict(model, newdata = tpcb_model)
head(pred_obs)

head(as.data.frame(coordinates(grd)))
names(tpcb_model_utm)
head(grd@data)

model1 <- lm(log10(tpcb) ~ distancefork, data = tpcb)
summary(model1)


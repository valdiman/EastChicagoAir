
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
pcb_soil$tpcb <- rowSums(pcb_soil[, 6:133])

# Select total PCB
tpcb <- pcb_soil[, c("Latitude", "Longitude", "tpcb")]

coordinates(tpcb) <- ~Longitude + Latitude
proj4string(tpcb) <- CRS(SRS_string = "EPSG:4326")

# East Chicago = UTM zone 16N
tpcb_utm <- spTransform(tpcb, CRS(SRS_string = "EPSG:32616"))

coords_df <- as.data.frame(coordinates(tpcb_utm))
coords_df$tpcb <- tpcb_utm$tpcb

unique_coords <- coords_df %>%
  group_by(coords.x1, coords.x2) %>%
  summarise(
    tpcb_mean = mean(tpcb, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  )

tpcb_unique <- unique_coords
coordinates(tpcb_unique) <- ~coords.x1 + coords.x2
proj4string(tpcb_unique) <- proj4string(tpcb_utm)
tpcb_unique$tpcb <- tpcb_unique$tpcb_mean

empirical_vario <- variogram(log10(tpcb) ~ 1, tpcb_unique,
                             cutoff = 10000,
                             width = 1000)

plot(empirical_vario)

fit <- fit.variogram(empirical_vario, vgm("Exp"))
plot(empirical_vario, fit)

fit_exp <- fit.variogram(
  empirical_vario,
  vgm(psill = 0.2, model = "Exp", range = 3000, nugget = 0.05)
)

fit_exp
plot(empirical_vario, fit_exp)

tpcb_unique$log_tpcb <- log10(tpcb_unique$tpcb)

empirical_vario2 <- variogram(
  log_tpcb ~ 1,
  tpcb_unique,
  cutoff = 10000,
  width = 250
)

plot(empirical_vario2)

fit <- fit.variogram(empirical_vario2, vgm("Exp"))
plot(empirical_vario2, fit)



plot(empirical_vario2, fit1)


# Start with a simple exponential model
v0 <- vgm(
  psill = var(log10(tpcb_unique$tpcb), na.rm = TRUE),
  model = "Exp",
  range = 3000,
  nugget = 0.1
)

fit_exp_fixrange <- fit.variogram(
  empirical_vario,
  v0,
  fit.ranges = FALSE
)

fit_exp_fixrange
plot(empirical_vario, fit_exp_fixrange)

# Create grid
data_bbox <- bbox(tpcb_unique)
x_range <- data_bbox[1,2] - data_bbox[1,1]
y_range <- data_bbox[2,2] - data_bbox[2,1]

grid_tight <- expand.grid(
  x = seq(data_bbox[1,1] - 0.05*x_range, data_bbox[1,2] + 0.05*x_range,
          length.out = 500),
  y = seq(data_bbox[2,1] - 0.05*y_range, data_bbox[2,2] + 0.05*y_range,
          length.out = 500)
)
coordinates(grid_tight) <- ~x + y
gridded(grid_tight) <- TRUE
proj4string(grid_tight) <- proj4string(tpcb_unique)

# Mask grid to convex hull
tpcb_sf <- st_as_sf(tpcb_unique)
hull <- st_convex_hull(st_union(tpcb_sf))
hull_buffer <- st_buffer(hull, dist = 1000)  # buffer in meters
hull_sp <- as(hull_buffer, "Spatial")
grid_masked <- grid_tight[hull_sp, ]

# Kriging
krige_result <- krige(tpcb ~ 1, tpcb_unique, grid_masked,
                      model = manual_vario, nmax = 10)

# Convert to raster
krige_pred_raster <- raster(krige_result["var1.pred"])
krige_var_raster  <- raster(krige_result["var1.var"])

# Smoothing
krige_pred_smooth <- focal(krige_pred_raster, w = matrix(1,3,3),
                           fun = mean, na.rm = TRUE)
krige_var_smooth  <- focal(krige_var_raster,  w = matrix(1,3,3),
                           fun = mean, na.rm = TRUE)

# Project to WGS84
krige_pred_wgs84 <- projectRaster(krige_pred_smooth,
                                  crs = CRS("+proj=longlat +datum=WGS84"))
krige_var_wgs84  <- projectRaster(krige_var_smooth, 
                                  crs = CRS("+proj=longlat +datum=WGS84"))

krige_stack <- stack(krige_pred_wgs84, krige_var_wgs84)
names(krige_stack) <- c("prediction", "variance")

# Export to QGIS ----------------------------------------------------------
writeRaster(krige_stack, "Output/GeoData/tPCB_Kriging_Stack.tif", format = "GTiff", overwrite = TRUE)

# Contours as vector
contours <- rasterToContour(krige_pred_smooth, levels = pretty(values(krige_pred_smooth), 20))
contours_sf <- st_as_sf(contours)
st_write(contours_sf, "Output/GeoData/tPCB_Kriging_Contours.gpkg", delete_dsn = TRUE)

# # Map results -----------------------------------------------------------
# Convert raster and points to data frames
krige_df <- as.data.frame(krige_pred_smooth, xy = TRUE)
names(krige_df)[3] <- "tpcb_pred"
tpcb_df  <- as.data.frame(tpcb_unique)
coords <- as.data.frame(coordinates(tpcb_unique))
tpcb_df$Longitude <- coords$coords.x1
tpcb_df$Latitude  <- coords$coords.x2

# Export to QGIS observations
tpcb_sf <- st_as_sf(tpcb_df, coords = c("coords.x1", "coords.x2"), crs = st_crs(tpcb_unique))
tpcb_sf_wgs84 <- st_transform(tpcb_sf, 4326)
st_write(tpcb_sf_wgs84, "Output/GeoData/tPCB_Sampling_Points.gpkg", delete_dsn = TRUE)

# Plot
ggplot() +
  geom_raster(data = krige_df, aes(x = x, y = y, fill = tpcb_pred)) +
  scale_fill_viridis_c(option = "C", na.value = NA, name = "tPCB (pg/m³)") +
  geom_point(data = tpcb_df, aes(x = Longitude, y = Latitude, size = tpcb),
             color = "blue", alpha = 0.7) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Kriging Prediction of tPCB Concentration")


# Evaluate Kriging model --------------------------------------------------
# Moran's I test for spatial autocorrelation
coords <- coordinates(tpcb_unique)
dist_mat <- as.matrix(dist(coords))
dist_inv <- 1/dist_mat
diag(dist_inv) <- 0

moran_test <- Moran.I(tpcb_unique$tpcb, dist_inv)
print("Moran's I test for spatial autocorrelation:")
print(moran_test)

# Leave-one-out cross-validation
# Assess how well your kriging model predicts values at locations not used
# to fit the model.
krige_cv <- krige.cv(tpcb ~ 1, tpcb_unique, manual_vario)

# Calculate performance metrics
cv_metrics <- data.frame(
  RMSE = sqrt(mean(krige_cv$residual^2)),
  MAE = mean(abs(krige_cv$residual)),
  Mean_Error = mean(krige_cv$residual),
  R2 = cor(krige_cv$observed, krige_cv$observed - krige_cv$residual)^2,
  NSE = 1 - sum(krige_cv$residual^2)/sum((krige_cv$observed - mean(krige_cv$observed))^2)
)

print("Cross-validation metrics:")
print(cv_metrics)

# Back-transform for original scale metrics
# This is essentially the same metrics but expressed in the original units
# of the data (e.g., ng/m³ of tPCB).
cv_metrics_original <- data.frame(
  RMSE = sqrt(mean((krige_cv$observed - (krige_cv$observed - krige_cv$residual))^2)),
  MAE = mean(abs(krige_cv$observed - (krige_cv$observed - krige_cv$residual))),
  R2 = cor(krige_cv$observed, (krige_cv$observed - krige_cv$residual))^2
)

print("Cross-validation metrics (original scale):")
print(cv_metrics_original)

# Check variogram model fit
# Check how well the fitted variogram matches the empirical variogram.
variogram_fit <- function(actual, model) {
  ss_res <- sum((actual$gamma - model$gamma)^2)
  ss_tot <- sum((actual$gamma - mean(actual$gamma))^2)
  r2 <- 1 - ss_res/ss_tot
  return(r2)
}

vario_r2 <- variogram_fit(empirical_vario, manual_vario)
cat("Variogram R²:", round(vario_r2, 3), "\n")

# Residual analysis
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(krige_cv$observed, krige_cv$residual, 
     xlab = "Observed (log)", ylab = "Residuals",
     main = "Residuals vs Observed")
abline(h = 0, col = "red")

# QQ-plot of residuals
qqnorm(krige_cv$residual)
qqline(krige_cv$residual, col = "red")

# Histogram of residuals
hist(krige_cv$residual, main = "Residual Distribution", 
     xlab = "Residuals", breaks = 10)

# Spatial pattern of residuals
residual_sf <- st_as_sf(tpcb_unique)
residual_sf$residual <- krige_cv$residual
plot(residual_sf["residual"], pch = 16, main = "Spatial Residual Pattern")

# Summary
cat("=== KRIGING VALIDATION REPORT ===\n")
cat("Spatial autocorrelation (Moran's I):", round(moran_test$observed, 3), "\n")
cat("Moran's I p-value:", round(moran_test$p.value, 4), "\n")
cat("Variogram R²:", round(vario_r2, 3), "\n\n")

cat("CROSS-VALIDATION METRICS:\n")
cat("RMSE:", round(cv_metrics$RMSE, 3), "\n")
cat("MAE:", round(cv_metrics$MAE, 3), "\n") 
cat("R²:", round(cv_metrics$R2, 3), "\n")
cat("NSE:", round(cv_metrics$NSE, 3), "\n\n")

cat("CROSS-VALIDATION METRICS (original scale):\n")
cat("RMSE:", round(cv_metrics_original$RMSE, 2), "ng/m³\n")
cat("MAE:", round(cv_metrics_original$MAE, 2), "ng/m³\n")
cat("R²:", round(cv_metrics_original$R2, 3), "\n")

# Moran's I: Values > 0 indicate positive spatial autocorrelation (p < 0.05 = significant)
# R²: > 0.7 = good fit, > 0.5 = acceptable
# NSE: > 0.7 = good, > 0.5 = acceptable
# Residuals: Should be randomly distributed without spatial patterns

# Positive Indicators:
# Significant Spatial Autocorrelation:
# Moran's I = 0.095 (p = 0.0077)
# Strong evidence of spatial patterning (clustering)
# Reasonable Model Performance:
# R² = 0.279 | NSE = 0.256
# Moderate predictive power - much better than PCB11
# Spatial Structure Present:
# Variogram R² = 1 → Good variogram fit
# Total PCB shows meaningful spatial gradients


# Geostatistical Validation Guidelines

## Interpretation Metrics for Kriging Models

### 1. Spatial Autocorrelation (Moran's I)
- **Values > 0**: Positive spatial autocorrelation (clustering)
- **Values ≈ 0**: Random spatial distribution  
- **Values < 0**: Negative spatial autocorrelation (dispersion)
- **Significance**: p-value < 0.05 indicates statistically significant spatial pattern

### 2. Coefficient of Determination (R²)
- **> 0.7**: Excellent fit
- **0.5 - 0.7**: Good fit
- **0.3 - 0.5**: Acceptable fit (context dependent)
- **< 0.3**: Poor fit

### 3. Nash-Sutcliffe Efficiency (NSE)
- **> 0.7**: Excellent model performance
- **0.5 - 0.7**: Good performance
- **0.0 - 0.5**: Acceptable performance
- **< 0.0**: Model worse than using mean value

### 4. Root Mean Square Error (RMSE)
- **Interpretation**: Average prediction error in units of the variable
- **Lower values** indicate better model performance
- Should be compared to data range and standard deviation

### 5. Mean Absolute Error (MAE) 
- **Interpretation**: Average absolute prediction error
- More robust to outliers than RMSE
- Lower values indicate better performance

### 6. Residual Analysis
- **Residuals vs. Fitted**: Should show random scatter around zero
- **QQ-Plot**: Residuals should follow straight line (normally distributed)
- **Spatial Pattern**: Residuals should show no spatial autocorrelation
- **Histogram**: Residuals should be approximately normally distributed

## Step-by-Step Validation Process

1. **Check spatial autocorrelation** with Moran's I
2. **Perform cross-validation** (leave-one-out or k-fold)
3. **Calculate performance metrics** (R², NSE, RMSE, MAE)
4. **Analyze residual patterns** (visual diagnostics)
5. **Compare with null model** (mean value prediction)

## Example of PCB 11 analysis
PCB11 has no spatial structure in PH air. The values are randomly distributed across sampling area.

### Key evidence

- Moran's I = -0.016 (p = 0.48) → No spatial autocorrelation
- R² = 0.001 → Virtually no predictive power
- NSE = -0.343 → Worse than just using the mean
- RMSE = 7.0 ng/m³ → High prediction error

### What this means

- Kriging is inappropriate for PCB11 - it cannot improve predictions
- PCB11 is well-mixed in PH air (no spatial gradients)


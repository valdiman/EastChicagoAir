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

# Read Excel data ---------------------------------------------------------
a <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group1Andy",
                          col_names = TRUE, col_types = NULL))
m <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group2Maeve",
                          col_names = TRUE, col_types = NULL))

# Select blank data -------------------------------------------------------
a.b <- subset(a, In.Out %in% c("method blank", "BLANK", "Blank, last cell"))
m.b <- subset(m, In.Out == "BLANK")

# Select columns that start with "PCB"
pcb_columns <- grep("^PCB", names(a.b), value = TRUE)
pcb_a.b <- a.b[, pcb_columns]
pcb_m.b <- m.b[, pcb_columns]

# Check normality ---------------------------------------------------------
# (1) a data
# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(pcb_a.b, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(pcb_a.b)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05] # 75% show normality via this test

# Function to create Q-Q plots for each PCB column
plot_qq <- function(column_data, column_name) {
  qqnorm(column_data, main = paste("Q-Q plot of", column_name))
  qqline(column_data, col = "steelblue", lwd = 2)
}

# Apply the function to each PCB column with its name
invisible(lapply(names(pcb_a.b), function(col_name) {
  plot_qq(pcb_a.b[[col_name]], col_name)
}))

# Apply Shapiro-Wilk test to each PCB column log10
shapiro_results_log10 <- lapply(log10(pcb_a.b), shapiro.test)
# Extract the p-values from the results
p_values_log10 <- sapply(shapiro_results_log10, function(x) x$p.value)
names(p_values_log10) <- names(pcb_a.b)
# Select p-values greater than 0.05
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05] # 94% show normality via this test

#Plot
# Apply the function to each column
invisible(lapply(names(pcb_a.b), function(col_name) {
  plot_qq(log10(pcb_a.b[[col_name]]), col_name)
}))

# (2) m data
pcb_m.b <- m.b[, pcb_columns]
# There are 2 zeros due to the different SS use here, PCB14 and PCB128+166
pcb_m.b[pcb_m.b == 0] <- NA

# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(pcb_m.b, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(pcb_m.b)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05] # 42% show normality via this test

# Function to create Q-Q plots for each PCB column
# Revised function to include the column name in the plot
plot_qq <- function(column_data, column_name) {
  qqnorm(column_data, main = paste("Q-Q plot of", column_name))
  qqline(column_data, col = "steelblue", lwd = 2)
}

# Apply the function to each PCB column with its name
invisible(lapply(names(pcb_m.b), function(col_name) {
  plot_qq(pcb_m.b[[col_name]], col_name)
}))

# Apply Shapiro-Wilk test to each PCB column log10
shapiro_results_log10 <- lapply(log10(pcb_m.b), shapiro.test)
# Extract the p-values from the results
p_values_log10 <- sapply(shapiro_results_log10, function(x) x$p.value)
names(p_values_log10) <- names(pcb_m.b)
# Select p-values greater than 0.05
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05] # 96% show normality via this test

#Plot
# Apply the function to each column
invisible(lapply(names(pcb_m.b), function(col_name) {
  plot_qq(log10(pcb_m.b[[col_name]]), col_name)
}))

# Calculate LOQ -----------------------------------------------------------
# Better option to use log10 data to calculate LOQ
# Create LOQ for a, i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10pcb_a.b <- log10(pcb_a.b) # log10 blank data
loq.a <- colMeans(log10pcb_a.b,
                  na.rm = TRUE) + 1.96*sapply(log10pcb_a.b,
                                              sd, na.rm = TRUE)/sqrt(5)
loq.a <- data.frame(t(loq.a))

# Create LOQ for m, i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10pcb_m.b <- log10(pcb_m.b) # log10 blank data
loq.m <- colMeans(log10pcb_m.b,
                  na.rm = TRUE) + 1.96*sapply(log10pcb_m.b,
                                              sd, na.rm = TRUE)/sqrt(9)
loq.m <- data.frame(t(loq.m))

# Samples vs LOQ (masses) -------------------------------------------------
# Select samples from a, subset only PCBs, and convert to numeric
sample.a <- sapply(subset(a, In.Out %in% c("IN", "OUT"))[, pcb_columns],
                   as.numeric)

# Log10 transformation excluding zeros
log10_sample.a <- as.matrix(log10(sample.a))
log10_sample.a[log10_sample.a == -Inf] <- NA

# Perform the comparison and assignment using vectorized operations per column
sample.a.2 <- matrix(0, nrow = nrow(log10_sample.a), ncol = ncol(log10_sample.a))
colnames(sample.a.2) <- colnames(log10_sample.a)

for (j in 1:ncol(log10_sample.a)) {
  sample.a.2[, j] <- ifelse(log10_sample.a[, j] > loq.a[j],
                            10^log10_sample.a[, j], 0)
}


# Here!

# Export results
write.csv(sample.a.2, file = "Output/Data/csv/Sample_aV01.csv",
          row.names = FALSE)

# (ii) Perform the comparison, if below loq => loq/(2)^0.5
a.3 <- ifelse(log10_sample.a > loq.a[[1]], log10_sample.a, loq.a[[1]]/sqrt(2))

# Transform back using 10^, leaving zeros unchanged
sample.a.3 <- ifelse(a.3 == 0, 0, 10^a.3)

# Export results
write.csv(sample.a.3, file = "Output/Data/csv/Sample_aV02.csv",
          row.names = FALSE)


# Select samples from m, subset only PCBs, and convert to numeric
sample.m <- sapply(subset(m, In.Out %in% c("IN", "OUT"))[, pcb_columns],
                   as.numeric)

# Log10 transformation excluding zeros
log10_sample.m <- log10(sample.m)
log10_sample.m[sample.m == 0] <- 0

# Perform the comparison and assignment using vectorized operations
m.2 <- ifelse(log10_sample.m > loq.m[[1]], log10_sample.m, 0)

# Transform back using 10^, leaving zeros unchanged
sample.m.2 <- ifelse(m.2 == 0, 0, 10^m.2)

# Export results
write.csv(sample.m.2, file = "Output/Data/csv/Sample_m.csv",
          row.names = FALSE)

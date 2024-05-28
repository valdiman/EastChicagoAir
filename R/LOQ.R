# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

# Library
library(readxl) # to read excel files
library(dplyr)
library(ggplot2)


# Read Excel data ---------------------------------------------------------
a <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group1Andy",
                          col_names = TRUE, col_types = NULL))
m <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group2Maeve",
                          col_names = TRUE, col_types = NULL))

# Select blank data -------------------------------------------------------
a.b <- subset(a, In.Out %in% c("method blank", "BLANK", "Blank, last cell"))
m.b <- subset(m, In.Out == "BLANK")

# Check normality ---------------------------------------------------------
# Select columns that start with "PCB"
pcb_columns <- grep("^PCB", names(a.b), value = TRUE)
pcb_a.b <- a.b[, pcb_columns]
pcb_m.b <- m.b[, pcb_columns]

# (1) a data
# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(pcb_a.b, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(pcb_a.b)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05]
# 75% show normality via this test

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
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05]
# 94% show normality via this test

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
p_values_normal <- p_values[p_values > 0.05]
# 42% show normality via this test

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
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05]
# 96% show normality via this test

#Plot
# Apply the function to each column
invisible(lapply(names(pcb_m.b), function(col_name) {
  plot_qq(log10(pcb_m.b[[col_name]]), col_name)
}))

# Calculate LOQ -----------------------------------------------------------
# Better option to use log10 data to calculate LOQ
# Create LOQ, i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10pcb_a.b <- log10(pcb_a.b) # log10 blank data
loq.a <- colMeans(log10pcb_a.b,
                  na.rm = TRUE) + 1.96*sapply(log10pcb_a.b,
                                              sd, na.rm = TRUE)/sqrt(5)
loq.a <- data.frame(t(loq.a))

# Create LOQ, i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10pcb_m.b <- log10(pcb_m.b) # log10 blank data
loq.m <- colMeans(log10pcb_m.b,
                  na.rm = TRUE) + 1.96*sapply(log10pcb_m.b,
                                              sd, na.rm = TRUE)/sqrt(9)
loq.m <- data.frame(t(loq.m))

# Samples vs LOQ (masses) -------------------------------------------------
# Select samples
sample.a <- subset(a, In.Out %in% c("IN", "OUT"))
# Select only PCBs
sample.a <- sample.a[, pcb_columns]
sample.a <- sapply(sample.a, as.numeric)
sample.a[sample.a == 0] <- NA
# Log10 transformation with handling of zeros
log10_sample.a <- log10(sample.a)

sample.m <- subset(m, In.Out %in% c("outdoor", "IN"))
# Select only PCBs
sample.m <- sample.m[, pcb_columns]
sample.m <- sapply(sample.m, as.numeric)
sample.m[sample.m == 0] <- NA
# Log10 transformation with handling of zeros
log10_sample.m <- log10(sample.m)

# If samples > loq, then sample, if not 0
# Create matrix to storage comparison
a.2 <- matrix(NA, nrow = dim(sample.a)[1], ncol = dim(sample.a)[2])
# Do comparison
for(i in 1:dim(sample.a)[1]) {
  for(j in 1:dim(sample.a)[2]) {
    if (log10_sample.a[i, j] > loq.a[j]) {
      a.2[i, j] <- sample.a[i, j]
    } else {
      a.2[i, j] <- 0
    }
  }
}









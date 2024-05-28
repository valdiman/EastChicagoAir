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

# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(pcb_a.b, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(pcb_a.b)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05]
# 75% show normality via this test

# Function to create Q-Q plots for each PCB column
plot_qq <- function(column) {
  qqnorm(column, main = paste("Q-Q plot of", deparse(substitute(column))))
  qqline(column, col = "steelblue", lwd = 2)
}

# Apply the function to each column
invisible(lapply(pcb_a.b, plot_qq))

# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(log10(pcb_a.b), shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(pcb_a.b)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05]
# 94% show normality via this test




# Calculate concentrations
# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("readxl")
install.packages("dplyr")

# Library
{
  library(readxl) # to read excel files
  library(dplyr)
}

# Read Excel data ---------------------------------------------------------
a <- data.frame(read_xlsx("Output/Data/Sample_aV02.xlsx", sheet = "FinalMassCorrected",
                          range = "A1:BS176", col_names = FALSE, col_types = NULL))
m <- data.frame(read_xlsx("Output/Data/Sample_mV02.xlsx", sheet = "FinalMassCorrected",
                          range = "A1:Bk176", col_names = FALSE, col_types = NULL))

# Data formatting
a.1 <- as.data.frame(t(a), stringsAsFactors = FALSE)
# Set the first row as the column names
colnames(a.1) <- as.character(unlist(a.1[1,]))
# Remove the first row
a.1 <- a.1[-1,]
# Convert to numeric values
a.1[, 3:176] <- lapply(a.1[, 3:176], as.numeric)
# Convert to date format
a.1$DateDeploy <- as.Date(as.numeric(a.1$DateDeploy), origin = "1899-12-30")
a.1$DateCollect <- as.Date(as.numeric(a.1$DateCollect), origin = "1899-12-30")
# Get only Out locations
a.2 <- filter(a.1, Location == "OUT")

# Data formatting
m.1 <- as.data.frame(t(m), stringsAsFactors = FALSE)
# Set the first row as the column names
colnames(m.1) <- as.character(unlist(m.1[1,]))
# Remove the first row
m.1 <- m.1[-1,]
# Convert to numeric values
m.1[, 3:176] <- lapply(m.1[, 3:176], as.numeric)
# Convert to date format
m.1$DateDeploy <- as.Date(as.numeric(m.1$DateDeploy), origin = "1899-12-30")
m.1$DateCollect <- as.Date(as.numeric(m.1$DateCollect), origin = "1899-12-30")
# Get only Out locations
m.2 <- filter(m.1, Location == "outdoor")




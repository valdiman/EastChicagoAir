# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("reshape2")
install.packages("tidyverse")
install.packages("readxl")

# Library
library(reshape2) # for melt function
library(tidyverse) # data manipulation
library(readxl) # to read excel files

# Read Excel data ---------------------------------------------------------
a <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group1Andy",
                          col_names = TRUE, col_types = NULL))
m <- data.frame(read_xlsx("Data/RawDataRachel.xlsx", sheet = "group2Maeve",
                          col_names = TRUE, col_types = NULL))


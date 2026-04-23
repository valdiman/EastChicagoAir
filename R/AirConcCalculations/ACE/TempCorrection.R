# Code to normalize ACC PCB concentation to 288 K

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("ggnewscale")
}

# Library
{
  library(dplyr)
  library(scales)
  library(ggplot2)
  library(tidyr)
  library(ggnewscale)
}

ace <- read.csv("Data/Air/EastChicago/ACE/ACEData.csv")


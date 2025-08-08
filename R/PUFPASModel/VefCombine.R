# Combine PUF PAS effective volumens in 1 csv file

install.packages('dplyr')

# Load the dplyr package
library(dplyr)

# (1) isd light
# Create an empty list to store the data frames
file_list <- list()

# Loop through the file numbers from 1 to 60
for (i in 1:60) {
  # Create the file name using paste0
  file_name <- paste0("Output/PUFPASModel/Vef/isd_light/VefOHareAirport_isd_light_", i, ".csv")
  
  # Read the CSV file
  file_data <- read.csv(file_name)
  
  # Filter the rows where Type = "Veff"
  file_data_filtered <- file_data %>% filter(Type == "Veff")
  
  # Add the filtered data frame to the list
  file_list[[i]] <- file_data_filtered
}

# Combine all the filtered data frames into one
combined_data <- do.call(rbind, file_list)

# Change column names that start with "X" to "PCB"
colnames(combined_data) <- gsub("^X", "PCB", colnames(combined_data))

# Write the combined data to a new CSV file
write.csv(combined_data, "Output/PUFPASModel/Vef/isd_light/combined_VefOHareAirport_isd_light.csv",
          row.names = FALSE)

# (2) MERRA
# Create an empty list to store the data frames
file_list <- list()

# Loop through the file numbers from 1 to 60
for (i in 1:60) {
  # Create the file name using paste0
  file_name <- paste0("Output/PUFPASModel/Vef/MERRA/VefEastChicago-", i, ".csv")
  
  # Read the CSV file
  file_data <- read.csv(file_name)
  
  # Filter the rows where Type = "Veff"
  file_data_filtered <- file_data %>% filter(Type == "Veff")
  
  # Add the filtered data frame to the list
  file_list[[i]] <- file_data_filtered
}

# Combine all the filtered data frames into one
combined_data <- do.call(rbind, file_list)

# Change column names that start with "X" to "PCB"
colnames(combined_data) <- gsub("^X", "PCB", colnames(combined_data))

# Write the combined data to a new CSV file
write.csv(combined_data, "Output/PUFPASModel/Vef/MERRA/combined_VefEastChicago.csv",
          row.names = FALSE)

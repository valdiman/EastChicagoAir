# R codes for download and read data from East Chicago PCB Soil paper

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages('pangaear')
  install.packages("tidyverse")
}

# Libraries
{
  library(pangaear) # Read data from Pangaea
  library(tidyverse) # data manipulation
}

# Read data from Pangaea and format data ------------------------------
# Citation:
# Martinez, Andres; Hua, Jason; Haque, Ezazul; Thorne, Peter;
# Hornbuckle, Keri C (2022): Dataset of concentrations of
# individual polychlorinated biphenyl congeners and total
# organic carbon in soils from East Chicago, Indiana, USA in
# 2017/2018. PANGAEA, https://doi.pangaea.de/10.1594/PANGAEA.941894

# Set cache path to the project folder
pg_cache$cache_path_set(full_path = "Data/Soil")

# Download original datasets from Pangaea
s.0 <- pg_data(doi = '10.1594/PANGAEA.941881') # soil dataset
b.0 <- pg_data(doi = '10.1594/PANGAEA.941686') # blank dataset
toc.0 <- pg_data(doi = '10.1594/PANGAEA.941884') # toc dataset

# Obtain concentrations from Pangaea dataset
s <- data.frame(s.0[[1]]$data) # ng/g
b <- data.frame(b.0[[1]]$data)
toc <- data.frame(toc.0[[1]]$data)

# Format concentration data for plotting
# Remove metadata from soil dataset
s.1 <- s[, !(names(s) %in% c("Event", "Method.comm"))]
# Rename PCB congener in columns in s.1
names(s.1) <- gsub(".*PCB([0-9\\.]+)\\.$", "PCB\\1", names(s.1))
# Remove and save metadata
s.1.metadata <- subset(s.1, select = c(Sample.label:Distance..m...Distance.to.Fork.))
s.2 <- subset(s.1, select = -c(Sample.label:Distance..m...Distance.to.Fork.))

# Remove metadata from blank dataset
b.1 <- subset(b, select = -c(Event:Date.time.end))
# Rename PCB congener in columns in b.1
names(b.1) <- gsub(".*PCB([0-9\\.]+)\\.$", "PCB\\1", names(b.1))

# Create LOQ, i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
b.1[b.1 == 0] <- NA # Not the best way, but zeros need to be removed for lo10 transformation
log10b.1 <- log10(b.1) # log10 blank data
loq <- colMeans(log10b.1, na.rm = TRUE) + 1.96*sapply(log10b.1, sd, na.rm = TRUE)/sqrt(8)
loq <- data.frame(t(loq))

# Make comparison between s.1 and loq
# If s.1 > loq, then s.1, if not 0
# Create matrix to storage s.1 or loq values in s.3
s.3 <- matrix(NA, nrow = dim(s.2)[1], ncol = dim(s.2)[2])
# Do comparison
for(i in 1:dim(s.2)[1]) {
  for(j in 1:dim(s.2)[2]) {
    if (log10(s.2[i, j]) > loq[j]) {
      s.3[i, j] <- s.2[i, j]
    } else {
      s.3[i, j] <- 0
    }
  }
}

colnames(s.3) <- colnames(s.2) # add PCB names to columns.

# Remove congeners with < 75% detection frequency
s.3 <- s.3[, colMeans(s.3 > 0) >= 0.75]

# Remove the same congeners in loq
loq.2 <- loq %>% select(colnames(s.3))

# Add loq value = (loq/(2)^0.5)), where zeros are in s.2
# Create new matrix to storage final concentrations
s.4 <- data.frame(matrix(NA, nrow = dim(s.3)[1], ncol = dim(s.3)[2]))

for(i in 1:dim(s.4)[1]) {
  for(j in 1:dim(s.4)[2]) {
    if (s.3[i, j] == 0) {
      s.4[i, j] <- 10^(loq.2[j])/sqrt(2)
    } else {
      s.4[i, j] <- s.3[i, j]
    }
  }
}

# Final dataset for concentrations, s.3
colnames(s.4) <- colnames(s.3) # add PCB names to columns.
# Add metadata
s.5 <- cbind(s.1.metadata, s.4)
# Add toc
s.5 <- cbind(s.5 , toc$TOC....)
# Change names
names(s.5)[names(s.5) == "Distance..m...Distance.to.Fork."] <- "distancefork"
names(s.5)[names(s.5) == "toc$TOC...."] <- "toc"

# export data
write.csv(s.5, "Output/Data/Soil/PCBEastChicagoSoil.csv", row.names = FALSE)


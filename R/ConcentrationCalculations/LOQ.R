# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("dplyr")
install.packages("ggplot2")

# Library
{
  library(dplyr)
  library(ggplot2)
}

# Read data ---------------------------------------------------------------
a <- read.csv("Data/Group1A.csv", check.names = FALSE)
m <- read.csv("Data/Group2M.csv", check.names = FALSE)

# Select blank data -------------------------------------------------------
blank.a <- subset(a, In.Out %in% c("methodblank", "BLANK", "Blank, last cell"))
blank.m <- subset(m, In.Out == "BLANK")

# Select columns that start with "PCB"
pcb_columns <- grep("^PCB", names(a), value = TRUE)
blank.a <- blank.a[, pcb_columns]
blank.m <- blank.m[, pcb_columns]

# Check normality ---------------------------------------------------------
# (1) a data
# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(blank.a, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(blank.a)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05] # 75% show normality via this test

# Function to create Q-Q plots for each PCB column
plot_qq <- function(column_data, column_name) {
  qqnorm(column_data, main = paste("Q-Q plot of", column_name))
  qqline(column_data, col = "steelblue", lwd = 2)
}

# Apply the function to each PCB column with its name
invisible(lapply(names(blank.a), function(col_name) {
  plot_qq(blank.a[[col_name]], col_name)
}))

# Apply Shapiro-Wilk test to each PCB column log10
shapiro_results_log10 <- lapply(log10(blank.a), shapiro.test)
# Extract the p-values from the results
p_values_log10 <- sapply(shapiro_results_log10, function(x) x$p.value)
names(p_values_log10) <- names(blank.a)
# Select p-values greater than 0.05
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05] # 94% show normality via this test

#Plot
# Apply the function to each column
invisible(lapply(names(blank.a), function(col_name) {
  plot_qq(log10(blank.a[[col_name]]), col_name)
}))

# (2) m data
# There are 2 zeros due to the different SS use here, PCB14 and PCB128+166
blank.m[blank.m == 0] <- NA

# Apply Shapiro-Wilk test to each PCB column
shapiro_results <- lapply(blank.m, shapiro.test)
# Extract the p-values from the results
p_values <- sapply(shapiro_results, function(x) x$p.value)
names(p_values) <- names(blank.m)
# Select p-values greater than 0.05
p_values_normal <- p_values[p_values > 0.05] # 42% show normality via this test

# Plot
# Apply the function to each PCB column with its name
invisible(lapply(names(blank.m), function(col_name) {
  plot_qq(blank.m[[col_name]], col_name)
}))

# Apply Shapiro-Wilk test to each PCB column log10
shapiro_results_log10 <- lapply(log10(blank.m), shapiro.test)
# Extract the p-values from the results
p_values_log10 <- sapply(shapiro_results_log10, function(x) x$p.value)
names(p_values_log10) <- names(blank.m)
# Select p-values greater than 0.05
p_values_normal_log10 <- p_values_log10[p_values_log10 > 0.05] # 96% show normality via this test

# Plot
# Apply the function to each column
invisible(lapply(names(blank.m), function(col_name) {
  plot_qq(log10(blank.m[[col_name]]), col_name)
}))

# Calculate LOQ -----------------------------------------------------------
# Better option to use log10 data to calculate LOQ
# LOQ are in log10 scale
# Create LOQ for "a", i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10blank.a <- log10(blank.a) # log10 blank data
loq.a <- colMeans(log10blank.a,
                  na.rm = TRUE) + 1.96*sapply(log10blank.a,
                                              sd, na.rm = TRUE)/sqrt(nrow(log10blank.a))
loq.a <- data.frame(t(loq.a))
# Check for Na and -Inf
any(is.na(loq.a))
any(loq.a == -Inf)
# Convert loq.a to a numeric vector
loq.a <- as.numeric(loq.a)

# Create LOQ for "m", i.e., upper 95 CI% (mean + 1.96*sd/(n)^0.5)
log10blank.m <- log10(blank.m) # log10 blank data
# Replace -Inf with NA for PCBs14 and 128+166
log10blank.m[log10blank.m == -Inf] <- NA
loq.m <- colMeans(log10blank.m,
                  na.rm = TRUE) + 1.96*sapply(log10blank.m,
                                              sd, na.rm = TRUE)/sqrt(nrow(log10blank.m))
loq.m <- data.frame(t(loq.m))
# Check for Na and -Inf
any(is.na(loq.m))
any(loq.m == -Inf)
# Convert loq.a to a numeric vector
loq.m <- as.numeric(loq.m)

# Plot LOQ ----------------------------------------------------------------
# Create data frames and add PCB names
loq.a_df <- data.frame(PCB = pcb_columns, LOQ = loq.a, Type = "loq.a")
loq.m_df <- data.frame(PCB = pcb_columns, LOQ = loq.m, Type = "loq.m")
# Combine the data frames
plot_loq_combined <- rbind(loq.a_df, loq.m_df)
plot_loq_combined$PCB <- gsub('\\.', '+', plot_loq_combined$PCB) # replace dot for +
plot_loq_combined$PCB <- factor(plot_loq_combined$PCB,
                                levels = unique(plot_loq_combined$PCB))

# Create the bar plot
plot.loqs <- ggplot(data = plot_loq_combined, aes(x = PCB, y = 10^(LOQ), fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(aspect.ratio = 25/135) +
  xlab(expression("")) +
  ylab(expression(bold("LOQ values (ng/PUF)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8,
                                   color = "black"),
        axis.title.y = element_text(face = "bold", size = 8,
                                    color = "black")) +
  theme(axis.text.x = element_text(face = "bold", size = 4,
                                   angle = 60, hjust = 1,
                                   color = "black")) +
  theme(axis.ticks = element_line(linewidth = 0.6, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  scale_fill_manual(values = c("loq.a" = "blue", "loq.m" = "red"))

# See plot
plot.loqs

# Save plot in folder
ggsave("Output/Plots/LOQs.png", plot = plot.loqs, width = 10,
       height = 5, dpi = 300)

# Samples vs LOQ (masses) -------------------------------------------------
# Select OUT samples from a, subset only PCBs, and convert to numeric
sample.a.0 <- subset(a, In.Out %in% c("OUT"))
sample.a <- sapply(subset(a, In.Out %in% c("OUT"))[, pcb_columns],
                   as.numeric)

# Log10 transformation excluding zeros and replacing with NA
log10sample.a <- as.matrix(log10(ifelse(sample.a == 0, NA, sample.a)))
# Change NaN to NA
log10sample.a[is.nan(log10sample.a)] <- NA
any(is.nan(log10sample.a)) # False
# Change -Inf to NA
log10sample.a[log10sample.a == -Inf] <- NA
any(log10sample.a == -Inf) # true

# Perform the comparison "a"
# (1) below loq -> 0
sample.a.c <- matrix(NA, nrow = nrow(log10sample.a), ncol = ncol(log10sample.a))
colnames(sample.a.c) <- colnames(log10sample.a)
for (j in 1:ncol(sample.a.c)) {
  for (i in 1:nrow(sample.a.c)) {
    if (!is.na(log10sample.a[i, j])) {
      if (log10sample.a[i, j] > loq.a[j]) {
        sample.a.c[i, j] <- 10^log10sample.a[i, j]
      } else {
        sample.a.c[i, j] <- 0
      }
    } else {
      sample.a.c[i, j] <- 0
    }
  }
}

# Add metadata
# Get the column indices for the range of columns in sample.a.0
sample.a.c <- cbind(sample.a.0$In.Out, as.Date(sample.a.0$Date.Placed, origin = "1899-12-30"),
                    sample.a.0$Date.Collected..shipment.date.for.FB.,
                    sample.a.c)
# Rename the first three columns of sample.a.c
colnames(sample.a.c)[1:3] <- c("Location", "DateDeploy", "DateCollect")
# Covert matrix to data.frame
sample.a.c <- as.data.frame(sample.a.c)
# Change Location name to outdoor
sample.a.c$Location <- "outdoor"
# Convert the column to Date format
sample.a.c$DateDeploy <- as.Date(as.numeric(sample.a.c$DateDeploy),
                                  origin = "1899-12-30")
sample.a.c$DateCollect <- as.Date(as.numeric(sample.a.c$DateCollect),
                                 origin = "1899-12-30")
# Export data
write.csv(sample.a.c, file = "Output/Data/csv/MassSample_a.csv",
          row.names = FALSE)

# # (2) below loq -> loq/(2)^0.5
# #sample.a.c.2 <- matrix(NA, nrow = nrow(log10sample.a), ncol = ncol(log10sample.a))
# #colnames(sample.a.c.2) <- colnames(log10sample.a)
# for (j in 1:ncol(sample.a.c.2)) {
#   for (i in 1:nrow(sample.a.c.2)) {
#     if (!is.na(log10sample.a[i, j])) {
#       if (log10sample.a[i, j] > loq.a[j]) {
#         sample.a.c.2[i, j] <- 10^log10sample.a[i, j]
#       } else {
#         sample.a.c.2[i, j] <- 10^(loq.a[j]/sqrt(2))
#       }
#     } else {
#       sample.a.c.2[i, j] <- 10^(loq.a[j]/sqrt(2))
#     }
#   }
# }
# 
# # Add metadata
# sample.a.c.2 <- cbind(sample.a.0[, cols_to_add], sample.a.c.2)
# sample.a.c.2 <- cbind(sample.a.0$In.Out, sample.a.0$Date.Placed,
#                     sample.a.0$Date.Collected..shipment.date.for.FB.,
#                     sample.a.c.2)
# # Rename the first three columns of sample.a.c
# colnames(sample.a.c.2)[1:3] <- c("Location", "DateDeploy", "DateCollect")
# # Convert the column to Date format
# sample.a.c.2$DateDeploy <- as.Date(as.numeric(sample.a.c.2$DateDeploy),
#                                  origin = "1899-12-30")
# sample.a.c.2$DateCollect <- as.Date(as.numeric(sample.a.c.2$DateCollect),
#                                   origin = "1899-12-30")
# 
# # Export results
# # Loq = 0
# write.csv(sample.a.c, file = "Output/Data/csv/Sample_aV01.csv",
#           row.names = FALSE)
# # Loq = loq/(2)^0.5
# write.csv(sample.a.c.2, file = "Output/Data/csv/Sample_aV02.csv",
#           row.names = FALSE)

# Select samples from "m", subset only PCBs, and convert to numeric
sample.m.0 <- subset(m, In.Out %in% c("outdoor"))
sample.m <- sapply(subset(m, In.Out %in% c("outdoor"))[, pcb_columns],
                   as.numeric)
# Log10 transformation excluding zeros and replacing with NA
log10sample.m <- as.matrix(log10(ifelse(sample.m == 0, NA, sample.m)))
# Change NaN to NA
log10sample.m[is.nan(log10sample.m)] <- NA
any(is.nan(log10sample.m)) # False
# Change -Inf to NA
log10sample.m[log10sample.m == -Inf] <- NA
any(log10sample.m == -Inf) # true

# Perform the comparison m
# (1) below loq -> 0
sample.m.c <- matrix(NA, nrow = nrow(log10sample.m), ncol = ncol(log10sample.m))
colnames(sample.m.c) <- colnames(log10sample.m)
for (j in 1:ncol(sample.m.c)) {
  for (i in 1:nrow(sample.m.c)) {
    if (!is.na(log10sample.m[i, j])) {
      if (log10sample.m[i, j] > loq.m[j]) {
        sample.m.c[i, j] <- 10^log10sample.m[i, j]
      } else {
        sample.m.c[i, j] <- 0
      }
    } else {
      sample.m.c[i, j] <- 0
    }
  }
}

# Add metadata
# Get the column indices for the range of columns in sample.a.0
sample.m.c <- cbind(sample.m.0$In.Out, sample.m.0$Date.Placed,
                    sample.m.0$Date.Collected..shipment.date.for.FB.,
                    sample.m.c)
# Rename the first three columns of sample.a.c
colnames(sample.m.c)[1:3] <- c("Location", "DateDeploy", "DateCollect")
# Covert matrix to data.frame
sample.m.c <- as.data.frame(sample.m.c)
# Convert the column to Date format
sample.m.c$DateDeploy <- as.Date(as.numeric(sample.m.c$DateDeploy),
                                 origin = "1899-12-30")
sample.m.c$DateCollect <- as.Date(as.numeric(sample.m.c$DateCollect),
                                  origin = "1899-12-30")
# Export data
write.csv(sample.m.c, file = "Output/Data/csv/MassSample_m.csv",
          row.names = FALSE)

# # (2) below loq -> loq/(2)^0.5
# sample.m.c.2 <- matrix(NA, nrow = nrow(log10sample.m), ncol = ncol(log10sample.m))
# colnames(sample.m.c.2) <- colnames(log10sample.m)
# for (j in 1:ncol(sample.m.c.2)) {
#   for (i in 1:nrow(sample.m.c.2)) {
#     if (!is.na(log10sample.m[i, j])) {
#       if (log10sample.m[i, j] > loq.m[j]) {
#         sample.m.c.2[i, j] <- 10^log10sample.m[i, j]
#       } else {
#         sample.m.c.2[i, j] <- 10^(loq.m[j]/sqrt(2))
#       }
#     } else {
#       sample.m.c.2[i, j] <- 10^(loq.m[j]/sqrt(2))
#     }
#   }
# }
# 
# # Add metadata
# sample.m.c.2 <- cbind(sample.m.0[, cols_to_add], sample.m.c.2)
# sample.m.c.2 <- cbind(sample.m.0$In.Out, sample.m.0$Date.Placed,
#                     sample.m.0$Date.Collected..shipment.date.for.FB.,
#                     sample.m.c.2)
# # Rename the first three columns of sample.a.c
# colnames(sample.m.c.2)[1:3] <- c("Location", "DateDeploy", "DateCollect")
# # Convert the column to Date format
# sample.m.c.2$DateDeploy <- as.Date(as.numeric(sample.m.c.2$DateDeploy),
#                                  origin = "1899-12-30")
# sample.m.c.2$DateCollect <- as.Date(as.numeric(sample.m.c.2$DateCollect),
#                                   origin = "1899-12-30")
# 
# # Export results
# write.csv(sample.m.c, file = "Output/Data/csv/Sample_mV01.csv",
#           row.names = FALSE)
# write.csv(sample.m.c.2, file = "Output/Data/csv/Sample_mV02.csv",
#           row.names = FALSE)


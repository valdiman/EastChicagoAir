# Data visualization 

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("tidyr")
}

# Library
{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
}

# Read data
dataset <- read.csv("Data/FinalDataset/DatasetV01.csv")

dataset <- dataset %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(
    # There is no distance information about the Mixed location
    Mixed_South_m = as.numeric(Mixed_South_m),
    Mixed_HS_m = as.numeric(Mixed_HS_m),
    
    Activity = case_when(
      as.character(Idle) == "1" ~ "Idle",
      as.character(Construction) == "1" ~ "Construction",
      as.character(Dredging) == "1" ~ "Dredging",
      TRUE ~ NA_character_
    ),
    Activity = factor(Activity, levels = c("Idle", "Construction", "Dredging")),
    
    SourceWind_South = factor(SourceWind_South,
                              levels = c("NonSource", "Source")),
    SourceWind_HS = factor(SourceWind_HS,
                           levels = c("NonSource", "Source"))
  ) %>%
  select(-Construction, -Dredging, -Idle)

# Correlations
pcb_response <- "PCB31_HS"
response <- paste0("log10_", pcb_response)

# PCB concentration columns only (exclude uncertainty columns)
pcb_cols <- names(dataset)[grepl("^PCB", names(dataset)) & !grepl("_unc", names(dataset))]

# Create log10 versions of PCB concentrations
dataset <- dataset %>%
  mutate(across(
    all_of(pcb_cols),
    ~ if_else(.x > 0, log10(.x), NA_real_),
    .names = "log10_{.col}"
  ))

# Numeric variables in the dataset
numeric_names <- names(dataset)[sapply(dataset, is.numeric)]

# Use non-PCB numeric variables as-is
non_pcb_vars <- setdiff(numeric_names, pcb_cols)

# Use log10 versions for PCB predictors, but exclude the response itself
pcb_log_vars <- paste0("log10_", setdiff(pcb_cols, pcb_response))

# Candidate predictors
candidate_vars <- c(non_pcb_vars, pcb_log_vars)
candidate_vars <- setdiff(candidate_vars, response)

# Count paired non-missing observations with the response
pair_counts <- sapply(candidate_vars, function(v) {
  sum(complete.cases(dataset[[response]], dataset[[v]]))
})

# Keep variables with at least 20 paired observations
keep_vars <- names(pair_counts[pair_counts >= 20])

# Build correlation table
cor_table <- lapply(keep_vars, function(v) {
  
  x <- dataset[[v]]
  ok <- complete.cases(dataset[[response]], x)
  
  if (sum(ok) < 20 ||
      sd(dataset[[response]][ok]) == 0 ||
      sd(x[ok]) == 0) {
    return(NULL)
  }
  
  test <- cor.test(
    dataset[[response]][ok],
    x[ok],
    method = "pearson"
  )
  
  data.frame(
    Variable = v,
    N = sum(ok),
    Correlation = unname(test$estimate),
    p.value = test$p.value
  )
  
}) %>%
  bind_rows() %>%
  arrange(desc(abs(Correlation))) %>%
  mutate(
    Correlation = round(Correlation, 3),
    p.value = signif(p.value, 3)
  )

cor_table

# Export data
write.csv(cor_table, "Output/Data/CorrelationAnalysis/cor_PCB31_HS.csv",
          row.names = FALSE)

# Top variables for plotting
top_vars <- cor_table %>%
  slice_head(n = 30) %>%
  pull(Variable)

# Long format for faceted plots
plot_df <- dataset %>%
  select(all_of(response), all_of(top_vars)) %>%
  pivot_longer(
    cols = -all_of(response),
    names_to = "Variable",
    values_to = "Value"
  )

# Faceted scatterplots
p <- ggplot(plot_df, aes(x = Value, y = .data[[response]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(
    x = NULL,
    y = paste0("log10(", pcb_response, ")")
  ) +
  theme_bw()

# See plot
p

# Save plot
ggsave("Output/Plots/CorrelationAnalysis/cor_PCB31_HS.png", plot = p,
       width = 10, height = 10, dpi = 500)

# Activities
plot_df <- dataset %>%
  filter(!is.na(.data[[response]]))

ggplot(plot_df, aes(x = Activity, y = .data[[response]])) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(
    x = "Activity",
    y = paste0("log10(", pcb_response, ")")
  ) +
  theme_bw()

# Wind Source
plot_df <- dataset %>%
  filter(
    !is.na(.data[[response]]),
    !is.na(SourceWind_South)
  )

ggplot(plot_df, aes(x = SourceWind_South, y = .data[[response]])) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(
    x = "Wind Direction",
    y = paste0("log10(", pcb_response, ")")
  ) +
  theme_bw()




#
#Clean canvas ----
#clears global environment
rm(list=ls())
#clears the plots
if (!is.null(dev.list())) dev.off() 
try(p_unload(p_loaded(),character.only = TRUE), silent = TRUE) #clears loaded packages
options(scipen = 100) #disable scientific notation for R session
#clears console
cat("\014")

# Load necessary libraries
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(flextable)
library(stringr)

# Load the dataset
data <- fread('C:/Uni_Work/ALY6015/Final Project/Data 2013-2023/New folder/Combined1.csv')

# Check the number of rows in the dataset
cat("Total number of rows in the dataset:", nrow(data), "\n")

# Sample 2% of the dataset for analysis
set.seed(123)  # For reproducibility
sampled_data <- data[sample(.N, as.integer(.N * 0.8))]
cat("Number of rows in the sampled dataset:", nrow(sampled_data), "\n")

# Parse date columns and create derived variables
sampled_data[, open_dt := ymd_hms(open_dt)]
sampled_data[, closed_dt := ymd_hms(closed_dt)]
sampled_data[, time_to_close := as.numeric(difftime(closed_dt, open_dt, units = "days"))]

# Create seasonal variable
sampled_data[, season := case_when(
  month(open_dt) %in% c(12, 1, 2) ~ "Winter",
  month(open_dt) %in% c(3, 4, 5) ~ "Spring",
  month(open_dt) %in% c(6, 7, 8) ~ "Summer",
  month(open_dt) %in% c(9, 10, 11) ~ "Fall"
)]
sampled_data[, season := factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))]

# Clean and preprocess categorical variables
sampled_data[, case_title := as.factor(case_title)]
sampled_data[, source := as.factor(source)]
sampled_data[, neighborhood := str_trim(neighborhood)]
sampled_data[neighborhood == "", neighborhood := NA]  # Replace blanks with NA
########################
# Frequency table for case_title
case_title_freq <- table(sampled_data$case_title)
case_title_freq <- sort(case_title_freq, decreasing = TRUE)
print(head(case_title_freq, 20))  # Top 20 most frequent case titles

# Group Rare Categories into "Other"
threshold <- 50  # Minimum number of occurrences to keep as a separate category

# Create a new column with grouped case titles
sampled_data$case_title <- ifelse(
  sampled_data$case_title %in% names(which(case_title_freq >= threshold)),
  as.character(sampled_data$case_title),
  "Other"
)

# Convert back to factor
sampled_data$case_title <- as.factor(sampled_data$case_title)

# Check the number of levels after grouping
length(unique(sampled_data$case_title))

# Define manual groupings
case_title_mapping <- list(
  "Snow Requests" = c("Request for Snow Plowing", "Request for Snow Plowing (Emergency Responder)"),
  "Trash Issues" = c("Missed Trash", "Missed Trash/Recycling/Yard Waste/Bulk Item", "Illegal Dumping"),
  "Street Lights" = c("Street Light Outages", "Street Light Knock Downs"),
  "Graffiti Removal" = c("Graffiti Removal", "Graffiti: Ward 3", "Graffiti: Ward 10"),
  "Other" = c("Other: Complaint")
)

# Apply the mapping
for (group in names(case_title_mapping)) {
  sampled_data$case_title[sampled_data$case_title %in% case_title_mapping[[group]]] <- group
}
table(sampled_data$case_title)

# Remove text after " -" or ":"
sampled_data$case_title_cleaned <- gsub(" - .*|:.*", "", sampled_data$case_title)

# Convert to factor
sampled_data$case_title_cleaned <- as.factor(sampled_data$case_title_cleaned)

# Check the unique levels
unique(sampled_data$case_title_cleaned)
# Frequency table for cleaned case titles
case_title_freq <- table(sampled_data$case_title_cleaned)
case_title_freq <- sort(case_title_freq, decreasing = TRUE)

# Define threshold for grouping
threshold <- 50  # Minimum occurrences to retain as separate category

# Group rare case titles into "Other"
sampled_data$case_title <- ifelse(
  sampled_data$case_title_cleaned %in% names(which(case_title_freq >= threshold)),
  as.character(sampled_data$case_title_cleaned),
  "Other"
)

# Convert to factor
sampled_data$case_title <- as.factor(sampled_data$case_title)
# Count unique case titles before and after cleaning
cat("Unique case titles (original):", length(unique(sampled_data$case_title)), "\n")
cat("Unique case titles (cleaned):", length(unique(sampled_data$case_title_cleaned)), "\n")
cat("Unique case titles (grouped):", length(unique(sampled_data$case_title)), "\n")

# Frequency of grouped titles
table(sampled_data$case_title)

# Drop rows with missing values in critical columns
sampled_data <- sampled_data[complete.cases(closed_dt, neighborhood, time_to_close)]

# Generate NA summary
na_summary <- data.frame(
  Column = names(sampled_data),
  NA_Count = colSums(is.na(sampled_data))
)
print(na_summary)

# Summary statistics by neighborhood
summary_stats <- sampled_data %>%
  group_by(neighborhood) %>%
  summarize(
    mean_time_to_close = mean(time_to_close, na.rm = TRUE),
    median_time_to_close = median(time_to_close, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_time_to_close))

# Display summary statistics in a table
result <- data.frame(summary_stats)
flextable(result) %>% autofit()

# Visualizations
# 1. Seasonal Distribution of Request Types
library(dplyr)

# Calculate frequencies by season and case_title_grouped
top_cases <- sampled_data %>%
  group_by(season, case_title) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(season, desc(count)) %>%
  group_by(season) %>%
  slice_max(order_by = count, n = 5)

# View the top cases
print(top_cases)
# Filter data to include only the top 5 cases per season
filtered_data <- sampled_data %>%
  semi_join(top_cases, by = c("season", "case_title"))
library(ggplot2)

ggplot(filtered_data, aes(x = season, fill = case_title)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  labs(
    title = "Top 5 Service Request Types by Season",
    x = "Season",
    y = "Number of Requests",
    fill = "Request Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2. Comparing Service type requested
library(dplyr)

# Identify top 8 service types for each season
top_8_per_season <- sampled_data %>%
  group_by(season, case_title) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(season, desc(count)) %>%
  group_by(season) %>%
  slice_head(n = 8)

# Filter the data to keep only top 8 service types per season
filtered_data <- sampled_data %>%
  semi_join(top_8_per_season, by = c("season", "case_title"))


# Facet plot for top 8 service types per season
ggplot(filtered_data, aes(x = case_title, fill = case_title)) +
  geom_bar() +
  facet_wrap(~ season, scales = "free_y") +  # Facet by season with independent y-axis
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Top 8 Service Request Types by Season",
    x = "Request Type",
    y = "Number of Requests",
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "bottom",  # Position the legend at the bottom
    legend.key.size = unit(0.5, "cm"),  # Adjust legend size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

##################

# 3. Neighborhood Disparities in Time to Close
# Calculate average time to close by neighborhood
neighborhood_stats <- sampled_data %>%
  group_by(neighborhood) %>%
  summarize(avg_time_to_close = mean(time_to_close, na.rm = TRUE), .groups = "drop") %>%
  arrange(avg_time_to_close)

# Select top 5 and last 5 neighborhoods
top_5_neighborhoods <- head(neighborhood_stats, 5)$neighborhood
last_5_neighborhoods <- tail(neighborhood_stats, 5)$neighborhood

# Combine top and last 5 neighborhoods
selected_neighborhoods <- c(top_5_neighborhoods, last_5_neighborhoods)
# Filter data for selected neighborhoods
filtered_data <- sampled_data %>%
  filter(neighborhood %in% selected_neighborhoods)
median_data <- filtered_data %>%
  group_by(neighborhood) %>%
  summarize(median_time_to_close = median(time_to_close, na.rm = TRUE)) %>%
  arrange(median_time_to_close)

ggplot(median_data, aes(x = reorder(neighborhood, median_time_to_close), y = median_time_to_close, fill = neighborhood)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Median Time to Close Requests by Neighborhood",
    x = "Neighborhood",
    y = "Median Time to Close (Days)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Linear Regression for Time to Close
model <- lm(time_to_close ~ case_title + neighborhood + source + season, data = sampled_data)
cat("\nModel Summary for Linear Regression:\n")
print(summary(model))

# Prepare data for Outcome 2
outcome2_data <- sampled_data %>%
  # Create season variable if not already present
  mutate(
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    case_title = as.factor(case_title),
    neighborhood = as.factor(neighborhood),
    source_var = as.factor(source)  # Rename source to source_var
  ) %>%
  drop_na(season, case_title, neighborhood, source_var)

# Aggregate case counts by season, case_title, and neighborhood
seasonal_summary <- outcome2_data %>%
  group_by(season, case_title, neighborhood, source_var) %>%
  summarize(count = n(), .groups = "drop")

# Fit linear regression model for seasonal trends
model_outcome2 <- lm(
  count ~ season + case_title + neighborhood + source_var,
  data = seasonal_summary
)
summary(model_outcome2)

library(car)

# Calculate VIF
vif_values <- vif(model_outcome2)
print(vif_values)

# Adding interaction terms
model_outcome2_interaction <- lm(
  count ~ season * case_title + neighborhood + source_var,
  data = seasonal_summary
)
summary(model_outcome2_interaction)


# Chi-Square Test for Season and Request Type
season_table <- table(sampled_data$season, sampled_data$case_title)
chi_sq_test <- chisq.test(season_table, simulate.p.value = TRUE, B = 10000)  # Use Monte Carlo simulation
cat("\nChi-Square Test Results:\n")
print(chi_sq_test)

unique(sampled_data$case_title)

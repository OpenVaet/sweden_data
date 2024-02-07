# Load necessary libraries
library(dplyr)
library(readr)

# File paths
population_file <- 'women_population_by_age_groups_R.csv'
births_file <- 'births_by_years_and_mothers_age_groups.csv'

# Read the data from CSV files
population_data <- read_csv(population_file) %>%
  mutate(age_group = as.character(age_group), year = as.numeric(year), total_population = as.numeric(total_population))

births_data <- read_csv(births_file) %>%
  mutate(age_group = as.character(age_group), year = as.numeric(year), births = as.numeric(births))

# Merge the datasets by age_group and year
merged_data <- merge(population_data, births_data, by = c("age_group", "year"))

# Calculate births per 1000
merged_data <- merged_data %>%
  mutate(births_per_1000 = (births / total_population) * 1000)

# Select only the necessary columns
final_data <- merged_data %>%
  select(age_group, year, births_per_1000)
print(final_data)

# Write the final data to CSV
write_csv(final_data, 'births_per_1000_by_years_mothers_age_groups.csv')

# Split the data by age_group
age_groups <- split(final_data, final_data$age_group)
print(age_groups)

# Initialize a list to store the trends
trends <- list()

# Calculate the linear trend for 2010-2019 for each age_group
for (age_group in names(age_groups)) {
  subset_data <- subset(age_groups[[age_group]], year >= 2010 & year <= 2019)
  fit <- lm(births_per_1000 ~ year, data = subset_data)
  trends[[age_group]] <- fit
}
trend_data <- do.call(rbind, lapply(names(trends), function(age_group) {
  data.frame(age_group = age_group, year = 2010:2022, births_per_1000 = predict(trends[[age_group]], newdata = data.frame(year = 2010:2022)), type = "Trend")
}))

# Calculate the percentage of deviation to the linear trend for 2020, 2021 & 2022
deviations <- data.frame(age_group=character(), year=integer(), deviation=numeric(), stringsAsFactors=FALSE)
for (age_group in names(age_groups)) {
  for (year in 2020:2022) {

    # Predict the value using the linear model
    predicted_value <- predict(trends[[age_group]], newdata = data.frame(year = year))
    
    # Get the actual value
    actual_value <- final_data %>%
      filter(age_group == !!age_group, year == !!year) %>%
      pull(births_per_1000)
    
    # Calculate the deviation
    deviation <- ((actual_value - predicted_value) / predicted_value) * 100
    
    # Append to the deviations data frame
    deviations <- rbind(deviations, data.frame(age_group=age_group, year=year, deviation=deviation))
  }
}
print(final_data)

library(ggplot2)

# Combine actual, trend & deviations data
final_data$type <- "Actual"
final_data$deviation <- NA  # Initialize the column with NAs
deviation_years <- c(2020, 2021, 2022)
final_data$deviation[final_data$year %in% deviation_years] <- deviations$deviation
trend_data$deviation <- NA
print(trend_data)
combined_data <- rbind(final_data, trend_data)

# Define colors for actual and trend lines
combined_data$label <- with(combined_data, paste(age_group, type, sep = ", "))
actual_colors <- c("15-19, Actual" = "blue", "20-24, Actual" = "green", "25-29, Actual" = "brown", "30-48, Actual" = "red")
trend_colors <- c("15-19, Trend" = "lightblue", "20-24, Trend" = "lightgreen", "25-29, Trend" = "grey", "30-48, Trend" = "pink")

# Plot the actual values and linear trend values
p <- ggplot(combined_data, aes(x = year, y = births_per_1000, group = label)) +
  geom_line(aes(color = label), linewidth = 1.4) +
  geom_text(data = subset(combined_data, type == "Actual" & !is.na(deviation)), 
            aes(label = paste0(round(deviation, 2), "%"), color = label), 
            vjust = -0.5, size = 4) +
  scale_color_manual(values = c(actual_colors, trend_colors)) +
  labs(title = "Sweden - Births / 1000 Women in Age, by Age Group 2020, 2021 & 2022 with Linear Trend (2010-2019)", x = "Year", y = "Number of Births", color = "Legend") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 2010:2022)

# Print the plot
print(p)

# Print the deviations
print(deviations)


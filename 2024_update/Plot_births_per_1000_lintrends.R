# ==============================================================================
# Swedish Birth Rate Analysis
# Births per 1000 Women with Linear Trends and ASFR Analysis
# ==============================================================================
# The two scripts :
# 2024_update/Plot_lintrend_break_4_up_to_48.R
# 2024_update/Plot_women_population.R
# Must be executed before - with the data sources names properly replaced if 
# you downloaded news ones to verify the assets.
# Sources at https://openvaet.info => New Nature Study Finds No Correlation
# Between COVID-19 Vaccines And Fertility Issues
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)

# ==============================================================================
# 1. DATA LOADING AND PREPARATION
# ==============================================================================

# File paths - adjust these to your directory structure
population_file <- 'women_population_by_age_groups_R.csv'
births_file <- 'births_by_years_and_mothers_age_groups.csv'

# Read the data from CSV files
population_data <- read_csv(population_file) %>%
  mutate(age_group = as.character(age_group), 
         year = as.numeric(year), 
         total_population = as.numeric(total_population))

births_data <- read_csv(births_file) %>%
  mutate(age_group = as.character(age_group), 
         year = as.numeric(year), 
         births = as.numeric(births))

# Merge the datasets by age_group and year
merged_data <- merge(population_data, births_data, by = c("age_group", "year"))

# Calculate births per 1000 (this is effectively the ASFR * 1000)
merged_data <- merged_data %>%
  mutate(births_per_1000 = (births / total_population) * 1000,
         asfr = births / total_population)  # Age-Specific Fertility Rate

# Select necessary columns
final_data <- merged_data %>%
  select(age_group, year, births_per_1000, asfr, births, total_population)

# Write the final data to CSV
write_csv(final_data, 'births_per_1000_by_years_mothers_age_groups.csv')

# ==============================================================================
# 2. LINEAR TREND ANALYSIS (2010-2019)
# ==============================================================================

# Split the data by age_group
age_groups <- split(final_data, final_data$age_group)

# Initialize lists to store trends and prediction intervals
trends <- list()
prediction_intervals <- list()

# Calculate the linear trend for 2010-2019 for each age_group
for (age_group in names(age_groups)) {
  subset_data <- subset(age_groups[[age_group]], year >= 2010 & year <= 2019)
  fit <- lm(births_per_1000 ~ year, data = subset_data)
  trends[[age_group]] <- fit
  
  # Calculate prediction intervals for 2010-2023
  pred_years <- data.frame(year = 2010:2023)
  pred_interval <- predict(fit, newdata = pred_years, interval = "prediction", level = 0.95)
  
  prediction_intervals[[age_group]] <- data.frame(
    age_group = age_group,
    year = 2010:2023,
    fit = pred_interval[, "fit"],
    lwr = pred_interval[, "lwr"],
    upr = pred_interval[, "upr"]
  )
}

# Combine prediction intervals
pred_interval_data <- do.call(rbind, prediction_intervals)

# Create trend data
trend_data <- pred_interval_data %>%
  select(age_group, year, births_per_1000 = fit) %>%
  mutate(type = "Trend")

# ==============================================================================
# 3. CALCULATE DEVIATIONS FROM TREND
# ==============================================================================

deviations <- data.frame(age_group = character(), 
                        year = integer(), 
                        deviation = numeric(), 
                        actual = numeric(),
                        predicted = numeric(),
                        stringsAsFactors = FALSE)

for (age_group in names(age_groups)) {
  for (year in 2020:2023) {
    # Predict the value using the linear model
    predicted_value <- predict(trends[[age_group]], newdata = data.frame(year = year))
    
    # Get the actual value
    actual_value <- final_data %>%
      filter(age_group == !!age_group, year == !!year) %>%
      pull(births_per_1000)
    
    # Calculate the deviation
    deviation <- ((actual_value - predicted_value) / predicted_value) * 100
    
    # Append to the deviations data frame
    deviations <- rbind(deviations, data.frame(
      age_group = age_group, 
      year = year, 
      deviation = deviation,
      actual = actual_value,
      predicted = predicted_value
    ))
  }
}

# ==============================================================================
# 4. PREPARE DATA FOR PLOTTING
# ==============================================================================

# Add type column to actual data
final_data$type <- "Actual"
final_data$deviation <- NA
deviation_years <- c(2020, 2021, 2022, 2023)

# Add deviations to final_data
for (i in 1:nrow(deviations)) {
  mask <- final_data$age_group == deviations$age_group[i] & 
          final_data$year == deviations$year[i]
  final_data$deviation[mask] <- deviations$deviation[i]
}

# Combine actual and trend data
trend_data$deviation <- NA
combined_data <- rbind(
  final_data %>% select(age_group, year, births_per_1000, type, deviation),
  trend_data
)

# Define color scheme - more professional palette
age_group_colors <- c(
  "15-19" = "#E15759",  # Coral red
  "20-24" = "#4E79A7",  # Steel blue
  "25-29" = "#59A14F",  # Green
  "30-48" = "#F28E2B"   # Orange
)

# Create lighter versions for trend lines
trend_colors <- sapply(age_group_colors, function(col) {
  rgb_col <- col2rgb(col)
  rgb(rgb_col[1], rgb_col[2], rgb_col[3], alpha = 100, maxColorValue = 255)
})

# ==============================================================================
# 5. PLOT 1: BIRTHS PER 1000 WITH TRENDS AND 95% PREDICTION INTERVALS
# ==============================================================================

# Prepare data for plotting with better labels
plot_data <- combined_data %>%
  mutate(line_type = ifelse(type == "Actual", "solid", "dashed"))

# Create the main plot
p1 <- ggplot() +
  # Add 95% prediction interval ribbons
  geom_ribbon(data = pred_interval_data, 
              aes(x = year, ymin = lwr, ymax = upr, fill = age_group),
              alpha = 0.15) +
  
  # Add trend lines (dashed)
  geom_line(data = filter(plot_data, type == "Trend"),
            aes(x = year, y = births_per_1000, color = age_group),
            linetype = "dashed", linewidth = 1.0, alpha = 0.7) +
  
  # Add actual lines (solid)
  geom_line(data = filter(plot_data, type == "Actual"),
            aes(x = year, y = births_per_1000, color = age_group),
            linetype = "solid", linewidth = 1.3) +
  
  # Add points for actual values
  geom_point(data = filter(plot_data, type == "Actual"),
             aes(x = year, y = births_per_1000, color = age_group),
             size = 2.5, shape = 19) +
  
  # Add deviation labels for 2020-2023
  geom_text(data = filter(plot_data, type == "Actual" & !is.na(deviation)),
            aes(x = year, y = births_per_1000, 
                label = sprintf("%.1f%%", deviation),
                color = age_group),
            vjust = -1.2, size = 3.5, fontface = "bold", show.legend = FALSE) +
  
  # Color scales
  scale_color_manual(values = age_group_colors,
                     name = "Age Group",
                     labels = c("15-19", "20-24", "25-29", "30-48")) +
  scale_fill_manual(values = age_group_colors,
                    name = "95% PI",
                    labels = c("15-19", "20-24", "25-29", "30-48")) +
  
  # Axis formatting
  scale_x_continuous(breaks = 2010:2023,
                     limits = c(2010, 2023.5)) +
  scale_y_continuous(labels = comma) +
  
  # Labels and theme
  labs(title = "Sweden: Births per 1,000 Women by Age Group (2010-2023)",
       subtitle = "Linear trend fitted on 2010-2019 data with 95% prediction intervals\nPercentage deviations shown for 2020-2023",
       x = "Year",
       y = "Births per 1,000 Women",
       caption = "Data source: Statistics Sweden\nDashed lines represent linear trend extrapolation from 2010-2019") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 2)),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.3))
  )

# Print the plot
print(p1)

# Save high-resolution version
ggsave("sweden_births_per_1000_with_trends.png", plot = p1, 
       width = 14, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# 6. PLOT 2: AGE-SPECIFIC FERTILITY RATES (ASFR)
# ==============================================================================

# Prepare ASFR data
asfr_data <- final_data %>%
  filter(type == "Actual") %>%
  select(age_group, year, asfr)

# Calculate ASFR trends
asfr_trends <- list()
asfr_pred_intervals <- list()

for (age_group in unique(asfr_data$age_group)) {
  subset_data <- asfr_data %>%
    filter(age_group == !!age_group, year >= 2010, year <= 2019)
  
  fit <- lm(asfr ~ year, data = subset_data)
  asfr_trends[[age_group]] <- fit
  
  # Prediction intervals
  pred_years <- data.frame(year = 2010:2023)
  pred_interval <- predict(fit, newdata = pred_years, interval = "prediction", level = 0.95)
  
  asfr_pred_intervals[[age_group]] <- data.frame(
    age_group = age_group,
    year = 2010:2023,
    fit = pred_interval[, "fit"],
    lwr = pred_interval[, "lwr"],
    upr = pred_interval[, "upr"]
  )
}

asfr_pred_data <- do.call(rbind, asfr_pred_intervals)

# Create ASFR trend lines
asfr_trend_lines <- asfr_pred_data %>%
  select(age_group, year, asfr = fit) %>%
  mutate(type = "Trend")

asfr_data$type <- "Actual"

# Calculate ASFR deviations for 2020-2023
asfr_deviations <- data.frame()
for (age_group in unique(asfr_data$age_group)) {
  for (year in 2020:2023) {
    predicted <- predict(asfr_trends[[age_group]], newdata = data.frame(year = year))
    actual <- asfr_data %>%
      filter(age_group == !!age_group, year == !!year) %>%
      pull(asfr)
    
    deviation <- ((actual - predicted) / predicted) * 100
    
    asfr_deviations <- rbind(asfr_deviations, data.frame(
      age_group = age_group,
      year = year,
      deviation = deviation
    ))
  }
}

# Add deviations to ASFR data
asfr_data$deviation <- NA
for (i in 1:nrow(asfr_deviations)) {
  mask <- asfr_data$age_group == asfr_deviations$age_group[i] & 
          asfr_data$year == asfr_deviations$year[i]
  asfr_data$deviation[mask] <- asfr_deviations$deviation[i]
}

# Create ASFR plot
p2 <- ggplot() +
  # Add 95% prediction interval ribbons
  geom_ribbon(data = asfr_pred_data,
              aes(x = year, ymin = lwr, ymax = upr, fill = age_group),
              alpha = 0.15) +
  
  # Add trend lines (dashed)
  geom_line(data = asfr_trend_lines,
            aes(x = year, y = asfr, color = age_group),
            linetype = "dashed", linewidth = 1.0, alpha = 0.7) +
  
  # Add actual lines (solid)
  geom_line(data = asfr_data,
            aes(x = year, y = asfr, color = age_group),
            linetype = "solid", linewidth = 1.3) +
  
  # Add points for actual values
  geom_point(data = asfr_data,
             aes(x = year, y = asfr, color = age_group),
             size = 2.5, shape = 19) +
  
  # Add deviation labels for 2020-2023
  geom_text(data = filter(asfr_data, !is.na(deviation)),
            aes(x = year, y = asfr,
                label = sprintf("%.1f%%", deviation),
                color = age_group),
            vjust = -1.2, size = 3.5, fontface = "bold", show.legend = FALSE) +
  
  # Color scales
  scale_color_manual(values = age_group_colors,
                     name = "Age Group",
                     labels = c("15-19", "20-24", "25-29", "30-48")) +
  scale_fill_manual(values = age_group_colors,
                    name = "95% PI",
                    labels = c("15-19", "20-24", "25-29", "30-48")) +
  
  # Axis formatting
  scale_x_continuous(breaks = 2010:2023,
                     limits = c(2010, 2023.5)) +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x)) +
  
  # Labels and theme
  labs(title = "Sweden: Age-Specific Fertility Rates (ASFR) by Age Group (2010-2023)",
       subtitle = "Linear trend fitted on 2010-2019 data with 95% prediction intervals\nPercentage deviations shown for 2020-2023",
       x = "Year",
       y = "Age-Specific Fertility Rate (births per woman)",
       caption = "Data source: Statistics Sweden\nASFR = Number of births / Female population in age group\nDashed lines represent linear trend extrapolation from 2010-2019") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 2)),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.3))
  )

# Print the ASFR plot
print(p2)

# Save high-resolution version
ggsave("sweden_asfr_by_age_group.png", plot = p2,
       width = 14, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# 7. OUTPUT SUMMARY STATISTICS
# ==============================================================================

cat("\n=== SUMMARY STATISTICS ===\n\n")

cat("Deviations from Linear Trend (2020-2023):\n")
print(deviations %>%
  arrange(age_group, year) %>%
  mutate(deviation = sprintf("%.2f%%", deviation),
         actual = round(actual, 2),
         predicted = round(predicted, 2)))

cat("\n\nLinear Model Coefficients (2010-2019):\n")
for (age_group in names(trends)) {
  cat(sprintf("\nAge Group: %s\n", age_group))
  print(summary(trends[[age_group]])$coefficients)
  cat(sprintf("R-squared: %.4f\n", summary(trends[[age_group]])$r.squared))
}

cat("\n\nASFR Summary (2023 vs 2019):\n")
asfr_comparison <- final_data %>%
  filter(year %in% c(2019, 2023)) %>%
  select(age_group, year, asfr) %>%
  pivot_wider(names_from = year, values_from = asfr, names_prefix = "ASFR_") %>%
  mutate(
    Change = ASFR_2023 - ASFR_2019,
    Pct_Change = (Change / ASFR_2019) * 100
  )
print(asfr_comparison)

# ==============================================================================
# 8. PLOT 3: TOTAL ASFR (ALL AGES COMBINED - WEIGHTED)
# ==============================================================================

# Calculate total births and total population across all age groups by year
total_data <- merged_data %>%
  group_by(year) %>%
  summarise(
    total_births = sum(births, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    total_asfr = total_births / total_population,
    total_births_per_1000 = (total_births / total_population) * 1000
  )

# Calculate linear trend for total ASFR (2010-2019)
total_subset <- total_data %>% filter(year >= 2010, year <= 2019)
total_fit <- lm(total_asfr ~ year, data = total_subset)

# Generate predictions with 95% intervals
pred_years_total <- data.frame(year = 2010:2023)
total_pred_interval <- predict(total_fit, newdata = pred_years_total, 
                               interval = "prediction", level = 0.95)

total_pred_data <- data.frame(
  year = 2010:2023,
  fit = total_pred_interval[, "fit"],
  lwr = total_pred_interval[, "lwr"],
  upr = total_pred_interval[, "upr"]
)

# Calculate deviations for 2020-2023
total_deviations <- data.frame()
for (year in 2020:2023) {
  predicted <- predict(total_fit, newdata = data.frame(year = year))
  actual <- total_data %>% filter(year == !!year) %>% pull(total_asfr)
  deviation <- ((actual - predicted) / predicted) * 100
  
  total_deviations <- rbind(total_deviations, data.frame(
    year = year,
    deviation = deviation,
    actual = actual,
    predicted = predicted
  ))
}

# Add deviations to total_data
total_data$deviation <- NA
for (i in 1:nrow(total_deviations)) {
  mask <- total_data$year == total_deviations$year[i]
  total_data$deviation[mask] <- total_deviations$deviation[i]
}

# Create the total ASFR plot
p3 <- ggplot() +
  # Add 95% prediction interval ribbon
  geom_ribbon(data = total_pred_data,
              aes(x = year, ymin = lwr, ymax = upr),
              fill = "#4E79A7", alpha = 0.2) +
  
  # Add trend line (dashed)
  geom_line(data = total_pred_data,
            aes(x = year, y = fit),
            linetype = "dashed", linewidth = 1.2, color = "#4E79A7", alpha = 0.7) +
  
  # Add actual line (solid)
  geom_line(data = total_data,
            aes(x = year, y = total_asfr),
            linetype = "solid", linewidth = 1.5, color = "#4E79A7") +
  
  # Add points for actual values
  geom_point(data = total_data,
             aes(x = year, y = total_asfr),
             size = 3.5, shape = 19, color = "#4E79A7") +
  
  # Add deviation labels for 2020-2023
  geom_text(data = filter(total_data, !is.na(deviation)),
            aes(x = year, y = total_asfr,
                label = sprintf("%.1f%%", deviation)),
            vjust = -1.5, size = 4, fontface = "bold", color = "#4E79A7") +
  
  # Axis formatting
  scale_x_continuous(breaks = 2010:2023,
                     limits = c(2010, 2023.5)) +
  scale_y_continuous(labels = function(x) sprintf("%.4f", x)) +
  
  # Labels and theme
  labs(title = "Sweden: Total Age-Specific Fertility Rate (All Ages Combined, 2010-2023)",
       subtitle = "Weighted ASFR across all reproductive age groups (15-48) with 95% prediction interval\nLinear trend fitted on 2010-2019 data; percentage deviations shown for 2020-2023",
       x = "Year",
       y = "Total ASFR (births per woman across all ages)",
       caption = "Data source: Statistics Sweden\nTotal ASFR = Sum of births across all age groups / Sum of female population (ages 15-48)\nDashed line represents linear trend extrapolation from 2010-2019") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 15, 15)
  )

# Print the total ASFR plot
print(p3)

# Save high-resolution version
ggsave("sweden_total_asfr_combined.png", plot = p3,
       width = 14, height = 8, dpi = 300, bg = "white")

# Print total ASFR summary
cat("\n\nTotal ASFR Statistics:\n")
cat("Linear Model Coefficients (2010-2019):\n")
print(summary(total_fit)$coefficients)
cat(sprintf("R-squared: %.4f\n", summary(total_fit)$r.squared))

cat("\n\nTotal ASFR Deviations (2020-2023):\n")
print(total_deviations %>%
  mutate(deviation = sprintf("%.2f%%", deviation),
         actual = sprintf("%.5f", actual),
         predicted = sprintf("%.5f", predicted)))

cat("\n\nTotal ASFR Change Summary:\n")
total_comparison <- total_data %>%
  filter(year %in% c(2010, 2019, 2023)) %>%
  select(year, total_asfr, total_births, total_population)
print(total_comparison)

cat(sprintf("\nChange 2019 to 2023: %.2f%%\n", 
            ((total_data$total_asfr[total_data$year == 2023] - 
              total_data$total_asfr[total_data$year == 2019]) / 
             total_data$total_asfr[total_data$year == 2019]) * 100))

cat(sprintf("Change 2010 to 2023: %.2f%%\n", 
            ((total_data$total_asfr[total_data$year == 2023] - 
              total_data$total_asfr[total_data$year == 2010]) / 
             total_data$total_asfr[total_data$year == 2010]) * 100))

cat("\n=== Analysis Complete ===\n")
cat("Plots saved as:\n")
cat("  - sweden_births_per_1000_with_trends.png\n")
cat("  - sweden_asfr_by_age_group.png\n")
cat("  - sweden_total_asfr_combined.png\n\n")
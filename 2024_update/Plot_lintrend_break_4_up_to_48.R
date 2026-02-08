# Load the data
birth_data <- read.csv("2024_update/BE0101E2_20260208-222622.csv")

# Remove rows with "data not available"
birth_data <- birth_data[!birth_data$`age.of.the.Mother` == "data not available",]

# Replace "49+ years" with "49"
birth_data$`age.of.the.Mother` <- gsub("49\\+ years", "49", birth_data$`age.of.the.Mother`)

# Remove " years" from all entries in the `age.of.the.Mother` column
birth_data$`age.of.the.Mother` <- as.numeric(gsub(" years", "", birth_data$`age.of.the.Mother`))

# Exclude the 14 years & under
birth_data <- birth_data[!is.na(birth_data$`age.of.the.Mother`) & birth_data$`age.of.the.Mother` >= 15,]

# Exclude the 49 years & over
birth_data <- birth_data[!is.na(birth_data$`age.of.the.Mother`) & birth_data$`age.of.the.Mother` <= 49,]

library(dplyr)
library(tidyr)

# Sum the total births for both children sexes for each year
birth_data_grouped <- birth_data %>%
  group_by(region, `age.of.the.Mother`) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE), .groups = "drop")

# Remove the "X" from the year column names
names(birth_data_grouped) <- sub("^X", "", names(birth_data_grouped))

# --- NEW: detect year columns dynamically (supports 2024, 2025, ...)
year_cols <- names(birth_data_grouped)[grepl("^\\d{4}$", names(birth_data_grouped))]
year_cols <- sort(year_cols)

# Create age groups
birth_data_grouped$age_group <- cut(
  birth_data_grouped$`age.of.the.Mother`,
  breaks = c(15, 20, 25, 30, Inf),
  labels = c("15-19", "20-24", "25-29", "30-48"),
  right = FALSE
)

# Aggregate data by year and age group (now includes 2024 automatically if present)
yearly_data <- birth_data_grouped %>%
  group_by(age_group) %>%
  summarise(across(all_of(year_cols), sum, na.rm = TRUE), .groups = "drop")

# Reshape wide -> long and keep >= 2010
data_long <- pivot_longer(yearly_data, cols = -age_group, names_to = "year", values_to = "births")
data_long$year <- as.numeric(data_long$year)
data_long <- data_long %>% filter(year >= 2010)
print(data_long)

# Write the long data to CSV
write.csv(data_long, "2024_update/births_by_years_and_mothers_age_groups.csv", row.names = FALSE)

# Mark actuals
data_long$type <- "Actual"

# Split the data by age_group
age_groups <- split(data_long, data_long$age_group)

# Fit linear trend 2010-2019 for each age_group
trends <- list()
for (ag in names(age_groups)) {
  subset_data <- subset(age_groups[[ag]], year >= 2010 & year <= 2019)
  trends[[ag]] <- lm(births ~ year, data = subset_data)
}

# --- NEW: generate trend up to the latest available year (includes 2024)
max_year <- max(data_long$year, na.rm = TRUE)

trend_data <- do.call(rbind, lapply(names(trends), function(ag) {
  yrs <- 2010:max_year
  data.frame(
    age_group = ag,
    year = yrs,
    births = predict(trends[[ag]], newdata = data.frame(year = yrs)),
    type = "Trend"
  )
}))

# --- NEW: compute deviations for 2020..max_year (includes 2024)
deviations <- data.frame(age_group=character(), year=integer(), deviation=numeric(), stringsAsFactors=FALSE)

for (ag in names(age_groups)) {
  for (yr in 2020:max_year) {
    predicted_value <- predict(trends[[ag]], newdata = data.frame(year = yr))
    actual_value <- data_long %>%
      filter(age_group == ag, year == yr) %>%
      pull(births)

    deviation <- ((actual_value - predicted_value) / predicted_value) * 100
    deviations <- rbind(deviations, data.frame(age_group=ag, year=yr, deviation=deviation))
  }
}

library(ggplot2)

# Combine actual, trend & deviations data
data_long$deviation <- NA
deviation_years <- 2020:max_year
data_long$deviation[data_long$year %in% deviation_years] <- deviations$deviation

trend_data$deviation <- NA
combined_data <- rbind(data_long, trend_data)

# Define colors for actual and trend lines
combined_data$label <- with(combined_data, paste(age_group, type, sep = ", "))
actual_colors <- c("15-19, Actual" = "blue", "20-24, Actual" = "green", "25-29, Actual" = "brown", "30-48, Actual" = "red")
trend_colors  <- c("15-19, Trend"  = "lightblue", "20-24, Trend"  = "lightgreen", "25-29, Trend"  = "grey", "30-48, Trend"  = "pink")

# Plot
p <- ggplot(combined_data, aes(x = year, y = births, group = label)) +
  geom_line(aes(color = label), linewidth = 1.4) +
  geom_text(
    data = subset(combined_data, type == "Actual" & !is.na(deviation)),
    aes(label = paste0(round(deviation, 2), "%"), color = label),
    vjust = -0.5, size = 4
  ) +
  scale_color_manual(values = c(actual_colors, trend_colors)) +
  labs(
    title = paste0("Sweden - Births by Age Group 2020–", max_year, " with Linear Trend (2010-2019)"),
    x = "Year", y = "Number of Births", color = "Legend"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = 2010:max_year)

print(p)
print(deviations)

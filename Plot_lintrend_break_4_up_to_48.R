# Load the data
birth_data <- read.csv("BE0101E2_20240205-204518.csv")

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

# Sum the total births for both children sexes for each year
library(dplyr)
library(tidyr)
birth_data_grouped <- birth_data %>%
  group_by(region, `age.of.the.Mother`) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE))

# Remove the "X" from the year column names
names(birth_data_grouped) <- sub("^X", "", names(birth_data_grouped))

# Create age groups
birth_data_grouped$age_group <- cut(birth_data_grouped$`age.of.the.Mother`,
                                    breaks = c(15, 20, 25, 30, Inf),
                                    labels = c("15-19", "20-24", "25-29", "30-48"),
                                    right = FALSE)

# Aggregate data by year and age group
yearly_data <- birth_data_grouped %>%
  group_by(age_group) %>%
  summarise(across(`2000`:`2022`, sum, na.rm = TRUE))

# Reshape the data from wide to long format & ensure that 'year' is treated as a numeric variable
data_long <- pivot_longer(yearly_data, cols = -age_group, names_to = "year", values_to = "births")
data_long$year <- as.numeric(as.character(data_long$year))
data_long <- data_long %>%
  filter(year >= 2010)
print(data_long)

# Write the yearly_data to a CSV file for re-use
write.csv(data_long, "births_by_years_and_mothers_age_groups.csv", row.names = FALSE)

# Create a new column 'type' to distinguish between actual and trend data
data_long$type <- "Actual"

# Split the data by age_group
age_groups <- split(data_long, data_long$age_group)

# Initialize a list to store the trends
trends <- list()

# Calculate the linear trend for 2010-2019 for each age_group
for (age_group in names(age_groups)) {
  subset_data <- subset(age_groups[[age_group]], year >= 2010 & year <= 2019)
  fit <- lm(births ~ year, data = subset_data)
  trends[[age_group]] <- fit
}
trend_data <- do.call(rbind, lapply(names(trends), function(age_group) {
  data.frame(age_group = age_group, year = 2010:2022, births = predict(trends[[age_group]], newdata = data.frame(year = 2010:2022)), type = "Trend")
}))

# Calculate the percentage of deviation to the linear trend for 2020, 2021 & 2022
deviations <- data.frame(age_group=character(), year=integer(), deviation=numeric(), stringsAsFactors=FALSE)
for (age_group in names(age_groups)) {
  for (year in 2020:2022) {

    # Predict the value using the linear model
    predicted_value <- predict(trends[[age_group]], newdata = data.frame(year = year))
    
    # Get the actual value
    actual_value <- data_long %>%
      filter(age_group == !!age_group, year == !!year) %>%
      pull(births)
    
    # Calculate the deviation
    deviation <- ((actual_value - predicted_value) / predicted_value) * 100
    
    # Append to the deviations data frame
    deviations <- rbind(deviations, data.frame(age_group=age_group, year=year, deviation=deviation))
  }
}

library(ggplot2)

# Combine actual, trend & deviations data
data_long$deviation <- NA  # Initialize the column with NAs
deviation_years <- c(2020, 2021, 2022)
data_long$deviation[data_long$year %in% deviation_years] <- deviations$deviation
trend_data$deviation <- NA
combined_data <- rbind(data_long, trend_data)

# Define colors for actual and trend lines
combined_data$label <- with(combined_data, paste(age_group, type, sep = ", "))
actual_colors <- c("15-19, Actual" = "blue", "20-24, Actual" = "green", "25-29, Actual" = "brown", "30-48, Actual" = "red")
trend_colors <- c("15-19, Trend" = "lightblue", "20-24, Trend" = "lightgreen", "25-29, Trend" = "grey", "30-48, Trend" = "pink")

# Plot the actual values and linear trend values
p <- ggplot(combined_data, aes(x = year, y = births, group = label)) +
  geom_line(aes(color = label), size = 1.4) +
  geom_text(data = subset(combined_data, type == "Actual" & !is.na(deviation)), 
            aes(label = paste0(round(deviation, 2), "%"), color = label), 
            vjust = -0.5, size = 4) +
  scale_color_manual(values = c(actual_colors, trend_colors)) +
  labs(title = "Sweden - Births by Age Group 2020,2021 & 2022 with Linear Trend (2010-2019)", x = "Year", y = "Number of Births", color = "Legend") +
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

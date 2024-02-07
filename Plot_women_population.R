library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)

# Read the data
data <- read_csv("BE0101N1_20240206-141557.csv")

# Clean the data
data$age <- gsub("\\+", "", data$age)
data$age <- gsub(" years", "", data$age)
data$age <- gsub(" year", "", data$age)
data$age <- as.numeric(data$age)

# Filter the data
filtered_data <- data %>%
  filter(sex == "women", age >= 15, age <= 48)

# Define Marital Status Group
filtered_data$marital_status_group <- ifelse(filtered_data$`marital status` %in% c('divorced', 'single', 'widowers/widows'), 'Not Married', 'Married')

# Define Age Group
filtered_data$age_group <- cut(
  filtered_data$age,
  breaks = c(14, 20, 25, 30, 49),
  labels = c('15-19', '20-24', '25-29', '30-48'),
  right = FALSE
)

# Gather yearly data into long format
yearly_data <- filtered_data %>%
  select(marital_status_group, age_group, starts_with("20")) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year))
stats <- yearly_data %>%
  group_by(marital_status_group, age_group, year) %>%
  summarize(population = sum(population, na.rm = TRUE))
stats <- stats %>%
  filter(year >= 2010)
print(stats)

library(ggplot2)
ggplot(stats, aes(x = year, y = population, color = marital_status_group, group = interaction(marital_status_group, age_group))) +
  geom_line(linewidth = 1.4) +
  facet_wrap(~age_group, scales = 'free_y') +
  scale_y_continuous(labels = label_number()) + # This will ensure y-axis labels are full integers
  labs(title = "Sweden - 2010-2022 - Population Evolution by Marital Status and Age Group",
       x = "Year",
       y = "Population",
       color = "Marital Status") +
  theme_minimal() +
  scale_x_continuous(breaks = 2010:2022)

# Summarize population by age_group and year, regardless of marital_status_group
total_population_by_age_year <- stats %>%
  ungroup() %>% # Remove previous groupings
  group_by(age_group, year) %>%
  summarize(total_population = sum(population, na.rm = TRUE))
total_population_by_age_year <- total_population_by_age_year %>%
  filter(year >= 2010)
  
# Print the new dataframe
print(total_population_by_age_year)

# Write the output
write_csv(total_population_by_age_year, "women_population_by_age_groups_R.csv")
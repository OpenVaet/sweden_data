###############################################################
## Age-Standardized Fertility Rate (ASFR) - ESP 2013 weights
## Ages 15–48, Swedish women
## Linear trend fitted on 2012–2020, 95% PI projected to 2021–2025
###############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

OUT_DIR <- "visual"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =====================================================================
# 1. LOAD DATA
# =====================================================================
pop <- read_csv("2025_update/1860_to_2025_women_population.csv",
                show_col_types = FALSE) %>%
  mutate(Age = as.integer(Age), Year = as.integer(Year),
         Population = as.numeric(Population))

births <- read_csv("2025_update/1891_to_2025_births.csv",
                   show_col_types = FALSE) %>%
  mutate(MotherAge = as.integer(MotherAge), Year = as.integer(Year),
         Births = as.numeric(Births))

# =====================================================================
# 1b. LOAD INFANT MORTALITY
# =====================================================================
imr_raw <- read_delim(
  "2025_update/000000MM_20260309-204950.csv",
  delim = ";", skip = 2, col_names = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Keep only "Deaths in the first year" columns (first 165 year-columns after sex)
imr <- imr_raw %>%
  slice(1) %>%                              # "boys and girls" row
  select(-1) %>%                            # drop sex column
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "col_name", values_to = "Rate") %>%
  filter(str_detect(col_name, "first year")) %>%
  mutate(
    Year = as.integer(str_extract(col_name, "\\d{4}$")),
    IMR  = as.numeric(ifelse(Rate == "..", NA, Rate))
  ) %>%
  select(Year, IMR) %>%
  filter(!is.na(IMR))

cat("Infant mortality loaded:", nrow(imr), "years,",
    min(imr$Year), "-", max(imr$Year), "\n")

# =====================================================================
# 2. AGE-SPECIFIC RATES BY 5-YEAR GROUP
# =====================================================================
ages_keep <- 15:48

mk_group5 <- function(a) {
  gs <- (a %/% 5) * 5
  sprintf("%02d-%02d", gs, gs + 4)
}

births_grp <- births %>%
  filter(MotherAge %in% ages_keep) %>%
  mutate(AgeGroup5 = mk_group5(MotherAge)) %>%
  group_by(Year, AgeGroup5) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

pop_grp <- pop %>%
  filter(Age %in% ages_keep) %>%
  mutate(AgeGroup5 = mk_group5(Age)) %>%
  group_by(Year, AgeGroup5) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

rates <- births_grp %>%
  inner_join(pop_grp, by = c("Year", "AgeGroup5")) %>%
  mutate(ASFR = Births / Population)   # per-woman rate (not per 1000)

# =====================================================================
# 3. ESP 2013 WEIGHTS
# =====================================================================
# European Standard Population 2013, per 5-year group (per 100,000):
#   15-19: 5500   20-24: 5500   25-29: 5500   30-34: 5500
#   35-39: 5500   40-44: 5500   45-49: 5500
# All equal in the fertile range, so the standardized rate is
# simply the mean ASFR across groups × 5 (TFR-like scaling),
# but we keep the proper weighted formula for correctness.

esp2013 <- tibble(
  AgeGroup5 = c("15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49"),
  ESP_w     = c(5500, 5500, 5500, 5500, 5500, 5500, 5500)
)

# Normalize weights to sum to 1
esp2013 <- esp2013 %>% mutate(w = ESP_w / sum(ESP_w))

# Join weights and compute standardized rate per year
asfr_std <- rates %>%
  inner_join(esp2013, by = "AgeGroup5") %>%
  group_by(Year) %>%
  summarise(
    ASFR_std = sum(ASFR * w),          # weighted mean (per woman)
    ASFR_std_1000 = sum(ASFR * w) * 1000,  # per 1,000 women
    .groups = "drop"
  ) %>%
  arrange(Year)

cat("ASFR computed:", nrow(asfr_std), "years,",
    min(asfr_std$Year), "-", max(asfr_std$Year), "\n")

# =====================================================================
# 4. LINEAR TRENDS + 95% PI 2021-2025
# =====================================================================

# --- Trend 1: 1970–2020 ---
fit_data_70 <- asfr_std %>% filter(Year >= 1970, Year <= 2020)
lm_70 <- lm(ASFR_std_1000 ~ Year, data = fit_data_70)

cat("Linear trend 1970-2020:\n")
cat("  Slope:    ", coef(lm_70)[2], "per year\n")
cat("  R²:       ", summary(lm_70)$r.squared, "\n")

trend_70 <- tibble(Year = 1970:2020) %>%
  mutate(fit = predict(lm_70, newdata = .))

# --- Trend 2: 2012–2020 (existing) ---
fit_data <- asfr_std %>% filter(Year >= 2012, Year <= 2020)
lm_fit   <- lm(ASFR_std_1000 ~ Year, data = fit_data)

cat("Linear trend 2012-2020:\n")
cat("  Slope:    ", coef(lm_fit)[2], "per year\n")
cat("  R²:       ", summary(lm_fit)$r.squared, "\n")

# Trend line for 2012–2020
trend_line <- tibble(Year = 2012:2020) %>%
  mutate(fit = predict(lm_fit, newdata = .))

# 95% prediction interval for 2021–2025
pi_years <- tibble(Year = 2021:2025)
pi_pred  <- predict(lm_fit, newdata = pi_years, interval = "prediction", level = 0.95)
pi_df    <- bind_cols(pi_years, as_tibble(pi_pred)) %>%
  rename(fit = fit, lwr = lwr, upr = upr)

# =====================================================================
# 5. PLOT (single axis: ASFR + Infant Mortality both per 1,000)
# =====================================================================

# Filter IMR to ASFR year range
imr_plot <- imr %>%
  filter(Year >= min(asfr_std$Year), Year <= max(asfr_std$Year))

p <- ggplot() +
  # Infant mortality (shaded area, behind)
  geom_area(data = imr_plot, aes(x = Year, y = IMR),
            fill = "#d95f02", alpha = 0.12) +
  geom_line(data = imr_plot, aes(x = Year, y = IMR),
            linewidth = 0.6, color = "#d95f02", alpha = 0.7) +
  # Observed ASFR
  geom_line(data = asfr_std, aes(x = Year, y = ASFR_std_1000),
            linewidth = 0.9, color = "black") +
  geom_point(data = asfr_std, aes(x = Year, y = ASFR_std_1000),
             size = 0.8, color = "black") +
  # Linear trend 1970–2020
  geom_line(data = trend_70, aes(x = Year, y = fit),
            linewidth = 0.7, color = "#e7298a", linetype = "longdash") +
  # 95% prediction interval ribbon (2021–2025)
  geom_ribbon(data = pi_df, aes(x = Year, ymin = lwr, ymax = upr),
              fill = "#3182bd", alpha = 0.2) +
  # Linear trend 2012–2020 (solid)
  geom_line(data = trend_line, aes(x = Year, y = fit),
            linewidth = 0.8, color = "#3182bd", linetype = "solid") +
  # Trend extrapolation into 2021–2025 (dashed)
  geom_line(data = pi_df, aes(x = Year, y = fit),
            linewidth = 0.8, color = "#3182bd", linetype = "dashed") +
  # Connecting the two segments
  geom_segment(
    aes(x = 2020, xend = 2021,
        y = tail(trend_line$fit, 1), yend = head(pi_df$fit, 1)),
    linewidth = 0.8, color = "#3182bd", linetype = "dashed"
  ) +
  # Trend labels
  annotate("text", x = 1973, y = predict(lm_70, newdata = tibble(Year = 1973)) + 4,
           label = paste0("1970\u20132020: ", sprintf("%+.2f", coef(lm_70)[2]), "/yr"),
           color = "#e7298a", fontface = "bold", size = 3.5, hjust = 0) +
  annotate("text", x = 2012, y = predict(lm_fit, newdata = tibble(Year = 2012)) + 4,
           label = paste0("2012\u20132020: ", sprintf("%+.2f", coef(lm_fit)[2]), "/yr"),
           color = "#3182bd", fontface = "bold", size = 3.5, hjust = 0) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Age-Standardized Fertility Rate (ESP 2013) & Infant Mortality - Sweden",
    subtitle = paste0(
      "Black: ASFR 15-48. Orange: infant mortality. ",
      "Linear trends: 1970\u20132020 (pink), 2012\u20132020 (blue) + 95% PI."
    ),
    x = NULL, y = "Rate per 1,000"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p

ggsave(file.path(OUT_DIR, "ASFR_ESP2013_trend_PI_1891_2025.png"),
       p, width = 14, height = 7, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "ASFR_ESP2013_trend_PI_1891_2025.pdf"),
       p, width = 14, height = 7, bg = "white")

cat("ASFR plot saved.\n")
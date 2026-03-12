###############################################################
## Births per 1,000 women by 5-year age group (15–48)
## FOCUS: 2010–2025
##   - Linear trend fitted on 2010–2020
##   - 95% Prediction interval for 2021–2025
##   - % deviation from trend for 2021–2025
##
## Sources:
##   - Population : 2025_update/1860_to_2025_women_population.csv
##   - Births HFD : 2025_update/SWEbirthsRR.txt  (HFD, 1891-2024)
##   - Births SCB : 2025_update/BE0101E2_20260309-172943.csv (1968-2024)
##   - Births SCB : 2025_update/00000863_20260309-172822.csv (2025)
##
## Merge strategy: HFD for 1891-1967, SCB for 1968-2025
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
# 1. LOAD WOMEN POPULATION (already merged long-format)
# =====================================================================
pop <- read_csv("2025_update/1860_to_2025_women_population.csv",
                show_col_types = FALSE) %>%
  mutate(Age = as.integer(Age),
         Year = as.integer(Year),
         Population = as.numeric(Population))

cat("Population loaded:", nrow(pop), "rows\n")

# =====================================================================
# 2. LOAD & MERGE BIRTHS (HFD 1891-1967 + SCB 1968-2025)
# =====================================================================

# --- HFD: 1891-2024 (whitespace-delimited, 3 header lines) ---
hfd_raw <- read_table(
  "2025_update/SWEbirthsRR.txt",
  skip = 2, col_names = TRUE,
  show_col_types = FALSE
)

parse_hfd_age <- function(x) {
  x <- str_trim(x)
  ifelse(str_detect(x, "[\\-\\+]"), NA_integer_, as.integer(x))
}

births_hfd <- hfd_raw %>%
  mutate(MotherAge = parse_hfd_age(Age),
         Year      = as.integer(Year),
         Births    = as.numeric(Total)) %>%
  filter(!is.na(MotherAge)) %>%
  select(Year, MotherAge, Births)

cat("HFD births loaded:", nrow(births_hfd), "rows,",
    "Years:", min(births_hfd$Year), "-", max(births_hfd$Year), "\n")

# --- SCB File A: 1968-2024 ---
parse_mother_age <- function(x) {
  x <- str_trim(x)
  case_when(
    str_detect(x, "^\\d+ year")          ~ as.integer(str_extract(x, "^\\d+")),
    str_detect(x, "^-\\d+ year")         ~ NA_integer_,
    str_detect(x, "^49\\+|^total|data")  ~ NA_integer_,
    TRUE                                 ~ NA_integer_
  )
}

raw_births_a <- read_delim(
  "2025_update/BE0101E2_20260309-172943.csv",
  delim = ";", skip = 2, col_names = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

births_scb_a <- raw_births_a %>%
  rename(region = 1, age_raw = 2, sex = 3) %>%
  mutate(MotherAge = parse_mother_age(age_raw)) %>%
  filter(!is.na(MotherAge)) %>%
  select(-region, -age_raw, -sex) %>%
  pivot_longer(-MotherAge, names_to = "Year", values_to = "Births") %>%
  mutate(Year   = as.integer(Year),
         Births = as.numeric(Births)) %>%
  group_by(Year, MotherAge) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

# --- SCB File B: 2025 ---
raw_births_b <- read_delim(
  "2025_update/00000863_20260309-172822.csv",
  delim = ";", skip = 2, col_names = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

births_scb_b <- raw_births_b %>%
  rename(region = 1, age_raw = 2, sex = 3) %>%
  filter(!str_detect(str_trim(sex), "^total")) %>%
  mutate(MotherAge = parse_mother_age(age_raw)) %>%
  filter(!is.na(MotherAge)) %>%
  select(-region, -age_raw, -sex) %>%
  pivot_longer(-MotherAge, names_to = "Year", values_to = "Births") %>%
  mutate(Year   = as.integer(Year),
         Births = as.numeric(Births)) %>%
  group_by(Year, MotherAge) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

# --- Merge: SCB for 1968-2025 (preferred), HFD for 1891-1967 ---
births_scb <- bind_rows(births_scb_a, births_scb_b) %>%
  group_by(Year, MotherAge) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

births_hfd_only <- births_hfd %>%
  filter(Year < min(births_scb$Year))

births <- bind_rows(births_hfd_only, births_scb) %>%
  arrange(Year, MotherAge)

cat("Merged births:", nrow(births), "rows,",
    "Years:", min(births$Year), "-", max(births$Year),
    "(HFD 1891-1967 + SCB 1968-2025)\n")

# =====================================================================
# 3. COMPUTE BIRTHS / 1,000 WOMEN by 5-year age group
# =====================================================================

mk_group5 <- function(a) {
  gs <- (a %/% 5) * 5
  ge <- gs + 4
  sprintf("%02d-%02d", gs, ge)
}

ages_keep <- 15:44

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
  mutate(Rate = Births / Population * 1000) %>%
  mutate(AgeGroup5 = factor(AgeGroup5,
                            levels = sort(unique(mk_group5(ages_keep)))))

cat("Rate data:", nrow(rates), "rows,",
    "Years:", min(rates$Year), "-", max(rates$Year), "\n")

# =====================================================================
# 4. FOCUS 2010-2025: Linear trend, prediction intervals, deviations
# =====================================================================

# Restrict to 2010-2025
rates_focus <- rates %>% filter(Year >= 2010, Year <= 2025)

# Fit 2010-2020 linear model per age group, predict 2010-2025
age_groups <- levels(rates$AgeGroup5)

trend_list   <- list()
predict_list <- list()
dev_list     <- list()

for (ag in age_groups) {

  df_train <- rates_focus %>% filter(AgeGroup5 == ag, Year >= 2010, Year <= 2020)
  df_all   <- rates_focus %>% filter(AgeGroup5 == ag)

  if (nrow(df_train) < 3) next

  fit <- lm(Rate ~ Year, data = df_train)

  # Predict over full 2010-2025 range with 95% prediction interval
  newdata <- data.frame(Year = 2010:2025)
  pred <- predict(fit, newdata = newdata, interval = "prediction", level = 0.95)
  pred_df <- cbind(newdata, as.data.frame(pred)) %>%
    rename(Trend = fit, PI_lo = lwr, PI_hi = upr) %>%
    mutate(AgeGroup5 = ag)

  predict_list[[ag]] <- pred_df

  # Deviation for 2021-2025
  df_post <- df_all %>%
    filter(Year >= 2021) %>%
    left_join(pred_df %>% select(Year, Trend, PI_lo, PI_hi), by = "Year") %>%
    mutate(
      Deviation     = Rate - Trend,
      Deviation_pct = (Rate - Trend) / Trend * 100,
      Below_PI      = Rate < PI_lo,
      Above_PI      = Rate > PI_hi
    )

  dev_list[[ag]] <- df_post
}

predictions <- bind_rows(predict_list) %>%
  mutate(AgeGroup5 = factor(AgeGroup5, levels = age_groups))

deviations <- bind_rows(dev_list) %>%
  mutate(AgeGroup5 = factor(AgeGroup5, levels = age_groups))

# =====================================================================
# 5. PRINT DEVIATION TABLE
# =====================================================================

cat("\n====================================================================\n")
cat("DEVIATION FROM 2010-2020 LINEAR TREND (2021-2025)\n")
cat("====================================================================\n\n")

for (ag in age_groups) {
  df <- deviations %>% filter(AgeGroup5 == ag)
  if (nrow(df) == 0) next

  cat(sprintf("--- Age group: %s ---\n", ag))
  cat(sprintf("  %-6s  %8s  %8s  %8s  %8s  %8s  %s\n",
              "Year", "Observed", "Trend", "PI_lo", "PI_hi", "Dev(%)", "Outside PI?"))

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    flag <- ifelse(row$Below_PI, "BELOW",
                   ifelse(row$Above_PI, "ABOVE", ""))
    cat(sprintf("  %-6d  %8.2f  %8.2f  %8.2f  %8.2f  %+7.1f%%  %s\n",
                row$Year, row$Rate, row$Trend, row$PI_lo, row$PI_hi,
                row$Deviation_pct, flag))
  }
  cat("\n")
}

# =====================================================================
# 6. FACETED PLOT
# =====================================================================

n_groups <- length(age_groups)
pal <- setNames(
  c("#e41a1c", "#ff7f00", "#4daf4a", "#377eb8",
    "#984ea3", "#a65628", "#f781bf")[seq_len(n_groups)],
  age_groups
)

p <- ggplot() +

  # 95% prediction interval ribbon (2021-2025 only)
  geom_ribbon(
    data = predictions %>% filter(Year >= 2021),
    aes(x = Year, ymin = PI_lo, ymax = PI_hi),
    fill = "grey80", alpha = 0.5
  ) +

  # Trend line (full 2010-2025 extrapolation)
  geom_line(
    data = predictions,
    aes(x = Year, y = Trend),
    color = "grey40", linewidth = 0.7, linetype = "dashed"
  ) +

  # Observed rate
  geom_line(
    data = rates_focus,
    aes(x = Year, y = Rate, color = AgeGroup5),
    linewidth = 1.0
  ) +
  geom_point(
    data = rates_focus,
    aes(x = Year, y = Rate, color = AgeGroup5),
    size = 1.8
  ) +

  # Mark points outside prediction interval
  geom_point(
    data = deviations %>% filter(Below_PI | Above_PI),
    aes(x = Year, y = Rate),
    shape = 21, size = 3.5, fill = "red", color = "black", stroke = 0.6
  ) +

  # Deviation % labels for 2021-2025
  geom_text(
    data = deviations,
    aes(x = Year, y = Rate,
        label = sprintf("%+.1f%%", Deviation_pct)),
    vjust = -1.2, hjust = 0.5, size = 2.8, color = "grey20"
  ) +

  facet_wrap(~ AgeGroup5, scales = "free_y", ncol = 2) +

  scale_x_continuous(breaks = 2010:2025, minor_breaks = NULL) +
  scale_color_manual(values = pal, guide = "none") +

  labs(
    title    = "Age-Specific Fertility Rate - Swedish Women (15-48): 2010-2025",
    subtitle = paste0(
      "Dashed line = linear trend fitted on 2010-2020.  ",
      "Grey ribbon = 95% prediction interval (2021-2025).  ",
      "Red circles = observed values outside PI.  ",
      "Labels = % deviation from trend."
    ),
    x = NULL, y = "Births / 1,000 women",
    caption = "Sources: HFD (1891-1967) + SCB (1968-2025)."
  ) +

  theme_minimal(base_size = 14) +
  theme(
    strip.text       = element_text(face = "bold", size = 13),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 10, color = "grey30"),
    plot.caption     = element_text(size = 9, color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9)
  )

p

ggsave(file.path(OUT_DIR, "fertility_rate_deviation_2010_2025.png"),
       p, width = 16, height = 14, dpi = 300, bg = "white")

cat("Deviation plot saved to:", file.path(OUT_DIR, "fertility_rate_deviation_2010_2025.png"), "\n")

# =====================================================================
# 7. EXPORT DEVIATION DATA
# =====================================================================

export_dev <- deviations %>%
  select(AgeGroup5, Year, Rate, Trend, PI_lo, PI_hi,
         Deviation, Deviation_pct, Below_PI, Above_PI) %>%
  arrange(AgeGroup5, Year)

write_csv(export_dev, file.path(OUT_DIR, "deviation_from_trend_2021_2025.csv"))
cat("Deviation CSV saved to:", file.path(OUT_DIR, "deviation_from_trend_2021_2025.csv"), "\n")

cat("\nDone.\n")

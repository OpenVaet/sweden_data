###############################################################
## Marriage Rate (15-48) vs ASFR — Sweden
##
## Sources:
##   - SCB: Population by marital status, age, sex (1968-2024)
##   - ASFR merged dataset (1971-2024)
##
## Computes: % married among 15-48 year-olds (men, women, total)
## Plots dual-axis time series + scatter + change correlations
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
# 1. LOAD & PARSE MARRIAGE DATA
# =====================================================================

raw <- read_delim(
  "2025_update/BE0101N1_20260315-000755.csv",
  delim = ";", skip = 2, col_names = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Parse age
parse_age <- function(x) {
  x <- str_trim(x)
  as.integer(str_extract(x, "^\\d+"))
}

mar <- raw %>%
  rename(region = 1, status = 2, age_raw = 3, sex = 4) %>%
  mutate(
    status = str_trim(status),
    sex    = str_trim(sex),
    Age    = parse_age(age_raw)
  ) %>%
  filter(!is.na(Age), Age >= 15, Age <= 48) %>%
  select(-region, -age_raw) %>%
  pivot_longer(-c(status, sex, Age), names_to = "Year", values_to = "Pop") %>%
  mutate(Year = as.integer(Year),
         Pop  = as.numeric(Pop))

cat("Marriage data parsed:", nrow(mar), "rows\n")
cat("Statuses:", paste(unique(mar$status), collapse = ", "), "\n")
cat("Years:", min(mar$Year), "-", max(mar$Year), "\n")

# =====================================================================
# 2. COMPUTE MARRIAGE RATE (% married) AMONG 15-48
# =====================================================================

# Total population and married population by year x sex
mar_rate <- mar %>%
  group_by(Year, sex) %>%
  summarise(
    total   = sum(Pop, na.rm = TRUE),
    married = sum(Pop[status == "married"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_married = 100 * married / total)

# Also compute combined (both sexes)
mar_rate_both <- mar %>%
  group_by(Year) %>%
  summarise(
    total   = sum(Pop, na.rm = TRUE),
    married = sum(Pop[status == "married"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sex = "both", pct_married = 100 * married / total)

mar_rate_all <- bind_rows(mar_rate, mar_rate_both) %>%
  arrange(Year, sex)

cat("\nMarriage rate (% married, 15-48):\n")
mar_rate_all %>%
  filter(Year %in% c(1968, 1980, 1990, 2000, 2010, 2020, 2024)) %>%
  select(Year, sex, pct_married) %>%
  pivot_wider(names_from = sex, values_from = pct_married) %>%
  print()

# =====================================================================
# 3. LOAD ASFR & MERGE
# =====================================================================

asfr <- read_csv("2025_update/ASFR_vs_affordability_merged.csv",
                 show_col_types = FALSE) %>%
  select(year, ASFR_1000)

df <- mar_rate_all %>%
  filter(sex == "both") %>%
  select(Year, pct_married) %>%
  rename(year = Year) %>%
  inner_join(asfr, by = "year") %>%
  arrange(year)

cat("\nMerged data:", nrow(df), "years,", min(df$year), "-", max(df$year), "\n")

# =====================================================================
# 4. CORRELATION ANALYSIS
# =====================================================================

# --- Levels ---
cor_lev <- cor.test(df$pct_married, df$ASFR_1000)
cat("\n=== Correlation: % Married vs ASFR (levels) ===\n")
cat("  Pearson r: ", sprintf("%.3f", cor_lev$estimate), "\n")
cat("  p-value:   ", sprintf("%.2e", cor_lev$p.value), "\n")

# --- Year-over-year changes ---
df_chg <- df %>%
  mutate(
    d_married = pct_married - lag(pct_married),
    d_asfr    = ASFR_1000 - lag(ASFR_1000)
  ) %>%
  filter(!is.na(d_married))

cor_chg <- cor.test(df_chg$d_married, df_chg$d_asfr)
cat("\n=== Correlation: Δ % Married vs Δ ASFR (changes) ===\n")
cat("  Pearson r: ", sprintf("%.3f", cor_chg$estimate), "\n")
cat("  Spearman ρ:", sprintf("%.3f", cor(df_chg$d_married, df_chg$d_asfr, method = "spearman")), "\n")
cat("  p-value:   ", sprintf("%.2e", cor_chg$p.value), "\n")

# --- Lagged changes (Δ married at t vs Δ ASFR at t+k) ---
cat("\n=== Lagged correlations on CHANGES (Δ married leads Δ ASFR) ===\n")
lag_chg <- tibble(lag = 0:5, pearson = NA_real_, p_value = NA_real_)
for (k in 0:5) {
  tmp <- df %>%
    mutate(
      d_married = pct_married - lag(pct_married),
      d_asfr    = ASFR_1000 - lag(ASFR_1000),
      d_mar_lag = lag(d_married, k)
    ) %>%
    filter(!is.na(d_mar_lag), !is.na(d_asfr))
  ct <- cor.test(tmp$d_mar_lag, tmp$d_asfr)
  lag_chg$pearson[k + 1] <- ct$estimate
  lag_chg$p_value[k + 1] <- ct$p.value
}
print(lag_chg)

# =====================================================================
# 5. PLOT A — Marriage rate by sex over time
# =====================================================================

p_mar <- ggplot(mar_rate_all, aes(x = Year, y = pct_married, color = sex)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    name = NULL,
    values = c("men" = "#377eb8", "women" = "#e41a1c", "both" = "black"),
    labels = c("men" = "Men", "women" = "Women", "both" = "Both sexes")
  ) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  labs(
    title    = "Proportion Married Among 15-48 Year-Olds - Sweden",
    subtitle = "Source: SCB population by marital status, 1968-2024.",
    x = NULL, y = "% Married"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 20),
    plot.subtitle    = element_text(size = 12, color = "grey30"),
    legend.position  = c(0.85, 0.85),
    legend.text      = element_text(size = 12),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    axis.text        = element_text(size = 12),
    axis.title.y     = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "marriage_rate_15_48_by_sex.png"),
       p_mar, width = 14, height = 8, dpi = 300, bg = "white")

# =====================================================================
# 6. PLOT B — ASFR vs Marriage rate (dual axis)
# =====================================================================

asfr_r <- range(df$ASFR_1000)
mar_r  <- range(df$pct_married)
a_sc   <- diff(asfr_r) / diff(mar_r)
b_sc   <- asfr_r[1] - a_sc * mar_r[1]

p_dual <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = ASFR_1000), color = "black", linewidth = 1.2) +
  geom_line(aes(y = a_sc * pct_married + b_sc),
            color = "#e41a1c", linewidth = 1.1) +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(
    name = "ASFR (per 1,000 women)",
    sec.axis = sec_axis(
      ~ (. - b_sc) / a_sc,
      name = "% Married among 15-48"
    )
  ) +
  labs(
    title = "ASFR vs Marriage Rate - Sweden, 1971-2024",
    subtitle = paste0(
      "Black: ASFR (ESP 2013, ages 15-48). ",
      "Red: % married among 15-48 (both sexes). ",
      "Levels r = ", sprintf("%.2f", cor_lev$estimate),
      " (p = ", sprintf("%.2e", cor_lev$p.value), ")."
    ),
    x = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 20),
    plot.subtitle      = element_text(size = 11, color = "grey30"),
    axis.title.y.left  = element_text(color = "black", face = "bold", size = 13),
    axis.title.y.right = element_text(color = "#e41a1c", face = "bold", size = 13),
    axis.text.y.right  = element_text(color = "#e41a1c"),
    axis.text          = element_text(size = 12),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

p_dual

ggsave(file.path(OUT_DIR, "ASFR_vs_marriage_rate_timeseries.png"),
       p_dual, width = 15, height = 8, dpi = 300, bg = "white")

# =====================================================================
# 7. PLOT C — Scatter: Δ marriage vs Δ ASFR
# =====================================================================

p_scat <- ggplot(df_chg, aes(x = d_married, y = d_asfr)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#e41a1c",
              fill = "#e41a1c", alpha = 0.12, linewidth = 0.8) +
  geom_point(aes(color = year), size = 2.5) +
  scale_color_viridis_c(name = "Year", option = "C") +
  labs(
    title = "ASFR vs Marriage Rate - Year-over-Year Changes",
    subtitle = paste0(
      "Each point = one year's change. ",
      "Pearson r = ", sprintf("%.2f", cor_chg$estimate),
      ", p = ", sprintf("%.2e", cor_chg$p.value), "."
    ),
    x = "% Married among 15-48 (year-over-year)",
    y = "ASFR (per 1,000 women, year-over-year)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 18),
    plot.subtitle    = element_text(size = 11.5, color = "grey30"),
    axis.title       = element_text(size = 13, face = "bold"),
    axis.text        = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_scat

ggsave(file.path(OUT_DIR, "ASFR_vs_marriage_rate_changes.png"),
       p_scat, width = 12, height = 9, dpi = 300, bg = "white")

# =====================================================================
# 8. EXPORT
# =====================================================================

write_csv(mar_rate_all, "2025_update/marriage_rate_15_48_sweden.csv")
write_csv(df, "2025_update/ASFR_vs_marriage_rate_merged.csv")

cat("\nAll plots and data saved.\n")

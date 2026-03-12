###############################################################
## ASFR vs Housing Affordability Pressure — Sweden, 1971-2024
##
## Merges:
##   - ASFR (ESP 2013, ages 15-48) from births + population data
##   - Housing affordability ratio from BIS + OECD via FRED
##
## Measures correlation between affordability pressure and
## fertility decline.
##
## Additional Sources (FRED):
##   - NAEXKP01SEA657S: National Accounts: GDP by Expenditure: Constant Prices: Gross Domestic Product: Total for Sweden
##   https://fred.stlouisfed.org/series/NAEXKP01SEA657S
##   - LRHUTTTTSEM156S: Infra-Annual Labor Statistics: Monthly Unemployment Rate Total: 15 Years or over for Sweden
##   https://fred.stlouisfed.org/series/LRHUTTTTSEM156S
###############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(tibble)
})

OUT_DIR <- "visual"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =====================================================================
# 1. COMPUTE ASFR (same method as ASFR_and_markers.R)
# =====================================================================
pop <- read_csv("2025_update/1860_to_2025_women_population.csv",
                show_col_types = FALSE) %>%
  mutate(Age = as.integer(Age), Year = as.integer(Year),
         Population = as.numeric(Population))

births <- read_csv("2025_update/1891_to_2025_births.csv",
                   show_col_types = FALSE) %>%
  mutate(MotherAge = as.integer(MotherAge), Year = as.integer(Year),
         Births = as.numeric(Births))

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

esp2013 <- tibble(
  AgeGroup5 = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"),
  ESP_w = rep(5500, 7)
) %>% mutate(w = ESP_w / sum(ESP_w))

asfr_std <- births_grp %>%
  inner_join(pop_grp, by = c("Year", "AgeGroup5")) %>%
  mutate(ASFR = Births / Population) %>%
  inner_join(esp2013, by = "AgeGroup5") %>%
  group_by(Year) %>%
  summarise(ASFR_1000 = sum(ASFR * w) * 1000, .groups = "drop") %>%
  arrange(Year)

# =====================================================================
# 2. LOAD HOUSING AFFORDABILITY
# =====================================================================
afford <- read_csv("2025_update/housing_affordability_sweden_1971_present.csv",
                   show_col_types = FALSE) %>%
  select(year, affordability_index)

# =====================================================================
# 3. MERGE (1971 to latest common year)
# =====================================================================
df <- asfr_std %>%
  rename(year = Year) %>%
  inner_join(afford, by = "year") %>%
  arrange(year)

cat("Merged data:", nrow(df), "years,", min(df$year), "-", max(df$year), "\n")

# =====================================================================
# 4. CORRELATION ANALYSIS
# =====================================================================

# --- 4a. Levels: Pearson & Spearman ---
cor_pearson  <- cor(df$affordability_index, df$ASFR_1000, method = "pearson")
cor_spearman <- cor(df$affordability_index, df$ASFR_1000, method = "spearman")
cor_test     <- cor.test(df$affordability_index, df$ASFR_1000)

cat("\n=== Correlation: Affordability Index vs ASFR (levels) ===\n")
cat("  Pearson r:  ", sprintf("%.3f", cor_pearson), "\n")
cat("  Spearman ρ: ", sprintf("%.3f", cor_spearman), "\n")
cat("  p-value:    ", sprintf("%.2e", cor_test$p.value), "\n")

# --- 4b. Year-over-year changes (contemporaneous) ---
df_chg <- df %>%
  mutate(
    d_afford = affordability_index - lag(affordability_index),
    d_asfr   = ASFR_1000 - lag(ASFR_1000)
  ) %>%
  filter(!is.na(d_afford))

cor_chg_pearson  <- cor(df_chg$d_afford, df_chg$d_asfr, method = "pearson")
cor_chg_spearman <- cor(df_chg$d_afford, df_chg$d_asfr, method = "spearman")
cor_chg_test     <- cor.test(df_chg$d_afford, df_chg$d_asfr)

cat("\n=== Correlation: Year-over-year CHANGES (contemporaneous) ===\n")
cat("  Pearson r:  ", sprintf("%.3f", cor_chg_pearson), "\n")
cat("  Spearman ρ: ", sprintf("%.3f", cor_chg_spearman), "\n")
cat("  p-value:    ", sprintf("%.2e", cor_chg_test$p.value), "\n")

# --- 4b2. Year-over-year changes with 1-year lag ---
# Δ affordability at year N vs Δ ASFR at year N+1
df_chg_lag1 <- df %>%
  mutate(
    d_afford = affordability_index - lag(affordability_index),
    d_asfr   = ASFR_1000 - lag(ASFR_1000),
    d_afford_lag1 = lag(d_afford, 1)  # previous year's affordability change
  ) %>%
  filter(!is.na(d_afford_lag1), !is.na(d_asfr))

cor_lag1_pearson  <- cor(df_chg_lag1$d_afford_lag1, df_chg_lag1$d_asfr, method = "pearson")
cor_lag1_spearman <- cor(df_chg_lag1$d_afford_lag1, df_chg_lag1$d_asfr, method = "spearman")
cor_lag1_test     <- cor.test(df_chg_lag1$d_afford_lag1, df_chg_lag1$d_asfr)

cat("\n=== Correlation: Year-over-year CHANGES (1-year lag: Δ afford → Δ ASFR next year) ===\n")
cat("  Pearson r:  ", sprintf("%.3f", cor_lag1_pearson), "\n")
cat("  Spearman ρ: ", sprintf("%.3f", cor_lag1_spearman), "\n")
cat("  p-value:    ", sprintf("%.2e", cor_lag1_test$p.value), "\n")

# --- 4c. Lagged correlations (affordability leads fertility by 0-5 years) ---
cat("\n=== Lagged correlations (affordability leads ASFR) ===\n")
lag_results <- tibble(lag = 0:5, pearson = NA_real_, spearman = NA_real_,
                      p_value = NA_real_)
for (k in 0:5) {
  tmp <- df %>%
    mutate(afford_lag = lag(affordability_index, k)) %>%
    filter(!is.na(afford_lag))
  ct <- cor.test(tmp$afford_lag, tmp$ASFR_1000)
  lag_results$pearson[k + 1]  <- ct$estimate
  lag_results$spearman[k + 1] <- cor(tmp$afford_lag, tmp$ASFR_1000,
                                      method = "spearman")
  lag_results$p_value[k + 1]  <- ct$p.value
}
print(lag_results)

# --- 4d. Lagged correlations on CHANGES (Δ afford at t vs Δ ASFR at t+k) ---
cat("\n=== Lagged correlations on CHANGES (Δ afford leads Δ ASFR) ===\n")
lag_chg_results <- tibble(lag = 0:5, pearson = NA_real_, spearman = NA_real_,
                          p_value = NA_real_)
for (k in 0:5) {
  tmp <- df %>%
    mutate(
      d_afford = affordability_index - lag(affordability_index),
      d_asfr   = ASFR_1000 - lag(ASFR_1000),
      d_afford_lagk = lag(d_afford, k)
    ) %>%
    filter(!is.na(d_afford_lagk), !is.na(d_asfr))
  ct <- cor.test(tmp$d_afford_lagk, tmp$d_asfr)
  lag_chg_results$pearson[k + 1]  <- ct$estimate
  lag_chg_results$spearman[k + 1] <- cor(tmp$d_afford_lagk, tmp$d_asfr,
                                          method = "spearman")
  lag_chg_results$p_value[k + 1]  <- ct$p.value
}
print(lag_chg_results)

# =====================================================================
# 5. PLOT A — Dual-axis time series
# =====================================================================

# Scaling for dual axis: map affordability onto ASFR axis
asfr_range   <- range(df$ASFR_1000)
afford_range <- range(df$affordability_index)
# Linear transform: afford_scaled = a * afford + b  maps afford_range → asfr_range
a_scale <- diff(asfr_range) / diff(afford_range)
b_scale <- asfr_range[1] - a_scale * afford_range[1]

p1 <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = ASFR_1000), color = "black", linewidth = 1.2) +
  geom_line(aes(y = a_scale * affordability_index + b_scale),
            color = "#d95f02", linewidth = 1.1, linetype = "solid") +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(
    name = "ASFR (per 1,000 women)",
    sec.axis = sec_axis(
      ~ (. - b_scale) / a_scale,
      name = "Affordability index (1971 = 100)"
    )
  ) +
  labs(
    title = "ASFR vs Housing Affordability - Sweden, 1971\u20132024",
    subtitle = paste0(
      "Black: ASFR (ESP 2013, ages 15\u201348). ",
      "Orange: real house prices / real labour costs (1971=100). ",
      "Pearson r = ", sprintf("%.2f", cor_pearson), "."
    ),
    x = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 20),
    plot.subtitle      = element_text(size = 11.5, color = "grey30"),
    axis.title.y.left  = element_text(color = "black", face = "bold", size = 13),
    axis.title.y.right = element_text(color = "#d95f02", face = "bold", size = 13),
    axis.text.y.right  = element_text(color = "#d95f02"),
    axis.text          = element_text(size = 12),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_timeseries.png"),
       p1, width = 15, height = 8, dpi = 300, bg = "white")

# =====================================================================
# 5b. PLOT A2 — Dual-axis time series, affordability shifted +1 year
# =====================================================================
# Affordability at year N plotted at year N+1 to test visual alignment
# with the hypothesis that affordability pressure leads fertility change

df_lag1_ts <- df %>%
  mutate(afford_shifted = lag(affordability_index, 1)) %>%
  filter(!is.na(afford_shifted))

# Recompute scaling for the shifted series
asfr_range2   <- range(df_lag1_ts$ASFR_1000)
afford_range2 <- range(df_lag1_ts$afford_shifted)
a_scale2 <- diff(asfr_range2) / diff(afford_range2)
b_scale2 <- asfr_range2[1] - a_scale2 * afford_range2[1]

# Lagged level correlation
cor_lag1_levels <- cor.test(df_lag1_ts$afford_shifted, df_lag1_ts$ASFR_1000)

p1b <- ggplot(df_lag1_ts, aes(x = year)) +
  geom_line(aes(y = ASFR_1000), color = "black", linewidth = 1.2) +
  geom_line(aes(y = a_scale2 * afford_shifted + b_scale2),
            color = "#d95f02", linewidth = 1.1, linetype = "solid") +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(
    name = "ASFR (per 1,000 women)",
    sec.axis = sec_axis(
      ~ (. - b_scale2) / a_scale2,
      name = "Affordability index (1971 = 100), shifted +1 year"
    )
  ) +
  labs(
    title = "ASFR vs Housing Affordability (1-year lag) - Sweden",
    subtitle = paste0(
      "Black: ASFR. Orange: affordability index shifted forward 1 year ",
      "(value at year N plotted at N+1). ",
      "Pearson r = ", sprintf("%.2f", cor_lag1_levels$estimate), "."
    ),
    x = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 20),
    plot.subtitle      = element_text(size = 11.5, color = "grey30"),
    axis.title.y.left  = element_text(color = "black", face = "bold", size = 13),
    axis.title.y.right = element_text(color = "#d95f02", face = "bold", size = 13),
    axis.text.y.right  = element_text(color = "#d95f02"),
    axis.text          = element_text(size = 12),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_timeseries_lag1.png"),
       p1b, width = 15, height = 8, dpi = 300, bg = "white")

# =====================================================================
# 6. PLOT B — Scatter (levels)
# =====================================================================

lm_scatter <- lm(ASFR_1000 ~ affordability_index, data = df)

p2 <- ggplot(df, aes(x = affordability_index, y = ASFR_1000)) +
  geom_smooth(method = "lm", se = TRUE, color = "#3182bd",
              fill = "#3182bd", alpha = 0.15, linewidth = 0.8) +
  geom_path(color = "grey60", linewidth = 0.4) +
  geom_point(aes(color = year), size = 2.5) +
  geom_text(
    data = df %>% filter(year %in% c(1971, 1980, 1990, 2000,
                                      2010, 2020, 2024)),
    aes(label = year), size = 3.5, fontface = "bold",
    nudge_y = 1.5, nudge_x = 1
  ) +
  scale_color_viridis_c(name = "Year", option = "C") +
  labs(
    title = "ASFR vs Housing Affordability - Scatter",
    subtitle = paste0(
      "Each point = one year (1971\u20132024). ",
      "Pearson r = ", sprintf("%.2f", cor_pearson),
      ", Spearman \u03c1 = ", sprintf("%.2f", cor_spearman),
      ". Path shows temporal sequence."
    ),
    x = "Affordability index (real house prices / real labour costs, 1971=100)",
    y = "ASFR (per 1,000 women)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 20),
    plot.subtitle    = element_text(size = 11.5, color = "grey30"),
    axis.title       = element_text(size = 13, face = "bold"),
    axis.text        = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_scatter.png"),
       p2, width = 12, height = 9, dpi = 300, bg = "white")

# =====================================================================
# 7. PLOT C — Year-over-year changes scatter
# =====================================================================

p3 <- ggplot(df_chg, aes(x = d_afford, y = d_asfr)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#e7298a",
              fill = "#e7298a", alpha = 0.12, linewidth = 0.8) +
  geom_point(aes(color = year), size = 2.5) +
  scale_color_viridis_c(name = "Year", option = "C") +
  labs(
    title = "ASFR vs Affordability - Year-over-Year Changes",
    subtitle = paste0(
      "Each point = one year\u2019s change. ",
      "Pearson r = ", sprintf("%.2f", cor_chg_pearson),
      ", p = ", sprintf("%.2e", cor_chg_test$p.value), "."
    ),
    x = "Affordability index (year-over-year)",
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

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_changes.png"),
       p3, width = 12, height = 9, dpi = 300, bg = "white")

# =====================================================================
# 8. PLOT D — Year-over-year changes scatter, 1-year lag
# =====================================================================

p4 <- ggplot(df_chg_lag1, aes(x = d_afford_lag1, y = d_asfr)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#7570b3",
              fill = "#7570b3", alpha = 0.12, linewidth = 0.8) +
  geom_point(aes(color = year), size = 2.5) +
  scale_color_viridis_c(name = "Year", option = "C") +
  labs(
    title = "ASFR vs Affordability - 1-Year Lag",
    subtitle = paste0(
      "X = affordability change at year N, Y = ASFR change at year N+1. ",
      "Pearson r = ", sprintf("%.2f", cor_lag1_pearson),
      ", p = ", sprintf("%.2e", cor_lag1_test$p.value), "."
    ),
    x = "Affordability index (year N)",
    y = "ASFR (per 1,000 women, year N+1)"
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

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_changes_lag1.png"),
       p4, width = 12, height = 9, dpi = 300, bg = "white")

# =====================================================================
# 9. PARTIAL CORRELATION — controlling for GDP growth & unemployment
# =====================================================================

gdp <- read_csv("2025_update/NAEXKP01SEA657S.csv", show_col_types = FALSE) %>%
  mutate(year = as.integer(substr(observation_date, 1, 4)),
         gdp_growth = NAEXKP01SEA657S) %>%
  select(year, gdp_growth)

unemp <- read_csv("2025_update/LRHUTTTTSEM156S.csv", show_col_types = FALSE) %>%
  mutate(year = as.integer(substr(observation_date, 1, 4)),
         unemp_rate = LRHUTTTTSEM156S) %>%
  group_by(year) %>%
  summarise(unemp_rate = mean(unemp_rate, na.rm = TRUE), .groups = "drop")

# --- 9a. Partial on GDP (full overlap: 1971–2024) ---
df_gdp <- df %>%
  inner_join(gdp, by = "year") %>%
  mutate(
    d_afford = affordability_index - lag(affordability_index),
    d_asfr   = ASFR_1000 - lag(ASFR_1000)
  ) %>%
  filter(!is.na(d_afford))

cat("\n=== PARTIAL CORRELATION: controlling for GDP growth ===\n")
cat("Overlap:", nrow(df_gdp), "years\n")

raw_r_gdp  <- cor(df_gdp$d_afford, df_gdp$d_asfr)
raw_ct_gdp <- cor.test(df_gdp$d_afford, df_gdp$d_asfr)

resid_a_gdp <- residuals(lm(d_afford ~ gdp_growth, data = df_gdp))
resid_f_gdp <- residuals(lm(d_asfr ~ gdp_growth, data = df_gdp))
partial_r_gdp  <- cor(resid_a_gdp, resid_f_gdp)
partial_ct_gdp <- cor.test(resid_a_gdp, resid_f_gdp)

cat("  Raw r:     ", sprintf("%.3f", raw_r_gdp),
    " (p = ", sprintf("%.3e", raw_ct_gdp$p.value), ")\n")
cat("  Partial r: ", sprintf("%.3f", partial_r_gdp),
    " (p = ", sprintf("%.3e", partial_ct_gdp$p.value), ")\n")
cat("  GDP → Δ afford: r =", sprintf("%.3f", cor(df_gdp$gdp_growth, df_gdp$d_afford)), "\n")
cat("  GDP → Δ ASFR:   r =", sprintf("%.3f", cor(df_gdp$gdp_growth, df_gdp$d_asfr)), "\n")

# --- 9b. Partial on unemployment (overlap: 1991–2024) ---
df_ue <- df %>%
  inner_join(unemp, by = "year") %>%
  mutate(
    d_afford = affordability_index - lag(affordability_index),
    d_asfr   = ASFR_1000 - lag(ASFR_1000),
    d_unemp  = unemp_rate - lag(unemp_rate)
  ) %>%
  filter(!is.na(d_afford), !is.na(d_unemp))

cat("\n=== PARTIAL CORRELATION: controlling for unemployment ===\n")
cat("Overlap:", nrow(df_ue), "years (", min(df_ue$year), "-", max(df_ue$year), ")\n")

raw_r_ue  <- cor(df_ue$d_afford, df_ue$d_asfr)
raw_ct_ue <- cor.test(df_ue$d_afford, df_ue$d_asfr)

# Control for unemployment LEVEL (not change) — unemployment level affects fertility decisions
resid_a_ue <- residuals(lm(d_afford ~ unemp_rate, data = df_ue))
resid_f_ue <- residuals(lm(d_asfr ~ unemp_rate, data = df_ue))
partial_r_ue  <- cor(resid_a_ue, resid_f_ue)
partial_ct_ue <- cor.test(resid_a_ue, resid_f_ue)

cat("  Raw r (this period):  ", sprintf("%.3f", raw_r_ue),
    " (p = ", sprintf("%.3e", raw_ct_ue$p.value), ")\n")
cat("  Partial r (ctrl UE):  ", sprintf("%.3f", partial_r_ue),
    " (p = ", sprintf("%.3e", partial_ct_ue$p.value), ")\n")
cat("  UE → Δ afford: r =", sprintf("%.3f", cor(df_ue$unemp_rate, df_ue$d_afford)), "\n")
cat("  UE → Δ ASFR:   r =", sprintf("%.3f", cor(df_ue$unemp_rate, df_ue$d_asfr)), "\n")

# Also control for Δ unemployment (change, not level)
resid_a_due <- residuals(lm(d_afford ~ d_unemp, data = df_ue))
resid_f_due <- residuals(lm(d_asfr ~ d_unemp, data = df_ue))
partial_r_due  <- cor(resid_a_due, resid_f_due)
partial_ct_due <- cor.test(resid_a_due, resid_f_due)

cat("\n  Partial r (ctrl Δ UE):", sprintf("%.3f", partial_r_due),
    " (p = ", sprintf("%.3e", partial_ct_due$p.value), ")\n")
cat("  Δ UE → Δ afford: r =", sprintf("%.3f", cor(df_ue$d_unemp, df_ue$d_afford)), "\n")
cat("  Δ UE → Δ ASFR:   r =", sprintf("%.3f", cor(df_ue$d_unemp, df_ue$d_asfr)), "\n")

# --- 9c. Full model: GDP + unemployment ---
df_full <- df %>%
  inner_join(gdp, by = "year") %>%
  inner_join(unemp, by = "year") %>%
  mutate(
    d_afford = affordability_index - lag(affordability_index),
    d_asfr   = ASFR_1000 - lag(ASFR_1000)
  ) %>%
  filter(!is.na(d_afford))

resid_a_full <- residuals(lm(d_afford ~ gdp_growth + unemp_rate, data = df_full))
resid_f_full <- residuals(lm(d_asfr ~ gdp_growth + unemp_rate, data = df_full))
partial_r_full  <- cor(resid_a_full, resid_f_full)
partial_ct_full <- cor.test(resid_a_full, resid_f_full)

cat("\n=== PARTIAL CORRELATION: controlling for GDP + unemployment ===\n")
cat("  Partial r:", sprintf("%.3f", partial_r_full),
    " (p = ", sprintf("%.3e", partial_ct_full$p.value), ")\n")

# Summary table
cat("\n=== SUMMARY TABLE ===\n")
cat(sprintf("%-45s  %7s  %10s\n", "Model", "r", "p-value"))
cat(sprintf("%-45s  %7s  %10s\n", "-----", "---", "-------"))
cat(sprintf("%-45s  %7.3f  %10.3e\n", "Raw (contemporaneous Δ, 1972-2024)", raw_r_gdp, raw_ct_gdp$p.value))
cat(sprintf("%-45s  %7.3f  %10.3e\n", "Partial (ctrl GDP, 1972-2024)", partial_r_gdp, partial_ct_gdp$p.value))
cat(sprintf("%-45s  %7.3f  %10.3e\n", paste0("Partial (ctrl UE level, ", min(df_ue$year), "-", max(df_ue$year), ")"), partial_r_ue, partial_ct_ue$p.value))
cat(sprintf("%-45s  %7.3f  %10.3e\n", paste0("Partial (ctrl Δ UE, ", min(df_ue$year), "-", max(df_ue$year), ")"), partial_r_due, partial_ct_due$p.value))
cat(sprintf("%-45s  %7.3f  %10.3e\n", paste0("Partial (ctrl GDP + UE, ", min(df_full$year), "-", max(df_full$year), ")"), partial_r_full, partial_ct_full$p.value))

# =====================================================================
# 10. PLOT — Partial correlation scatter (GDP control)
# =====================================================================

df_resid <- tibble(
  year = df_gdp$year,
  resid_afford = resid_a_gdp,
  resid_asfr   = resid_f_gdp
)

p5 <- ggplot(df_resid, aes(x = resid_afford, y = resid_asfr)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#2ca02c",
              fill = "#2ca02c", alpha = 0.12, linewidth = 0.8) +
  geom_point(aes(color = year), size = 2.5) +
  scale_color_viridis_c(name = "Year", option = "C") +
  labs(
    title = "Partial Correlation: ASFR vs Affordability, controlling for GDP",
    subtitle = paste0(
      "Residuals after regressing each on GDP growth. ",
      "Partial r = ", sprintf("%.2f", partial_r_gdp),
      " (raw r = ", sprintf("%.2f", raw_r_gdp),
      "). p = ", sprintf("%.2e", partial_ct_gdp$p.value), "."
    ),
    x = "Residual Affordability (GDP-adjusted)",
    y = "Residual ASFR (GDP-adjusted)"
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

ggsave(file.path(OUT_DIR, "ASFR_vs_affordability_partial_GDP.png"),
       p5, width = 12, height = 9, dpi = 300, bg = "white")

# =====================================================================
# 11. VISUAL OVERVIEW — All factors alongside ASFR
# =====================================================================

# Merge all series for plotting
df_all <- df %>%
  left_join(gdp, by = "year") %>%
  left_join(unemp, by = "year")

# --- 11a. ASFR vs Unemployment (dual axis) ---
# Scale unemployment onto ASFR axis (inverted — high UE at bottom)
ue_sub  <- df_all %>% filter(!is.na(unemp_rate))
asfr_r  <- range(ue_sub$ASFR_1000)
ue_r    <- range(ue_sub$unemp_rate)
a_ue    <- diff(asfr_r) / diff(ue_r)
b_ue    <- asfr_r[2] - a_ue * ue_r[1]  # high ASFR = low UE (inverted)

# Correlation for subtitle
cor_ue_asfr <- cor.test(ue_sub$unemp_rate, ue_sub$ASFR_1000)

p_ue <- ggplot(ue_sub, aes(x = year)) +
  geom_line(aes(y = ASFR_1000), color = "black", linewidth = 1.2) +
  geom_line(aes(y = -a_ue * unemp_rate + b_ue),
            color = "#e41a1c", linewidth = 1.1) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  scale_y_continuous(
    name = "ASFR (per 1,000 women)",
    sec.axis = sec_axis(
      ~ -(. - b_ue) / a_ue,
      name = "Unemployment rate (%, inverted)"
    )
  ) +
  labs(
    title = "ASFR vs Unemployment Rate - Sweden",
    subtitle = paste0(
      "Black: ASFR. Red: unemployment (inverted axis - up = lower unemployment). ",
      "r = ", sprintf("%.2f", cor_ue_asfr$estimate),
      " (p = ", sprintf("%.2e", cor_ue_asfr$p.value), ")."
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

ggsave(file.path(OUT_DIR, "ASFR_vs_unemployment_timeseries.png"),
       p_ue, width = 15, height = 8, dpi = 300, bg = "white")

# --- 11b. ASFR vs GDP growth (dual axis) ---
gdp_sub <- df_all %>% filter(!is.na(gdp_growth))
asfr_r2 <- range(gdp_sub$ASFR_1000)
gdp_r   <- range(gdp_sub$gdp_growth)
a_gdp   <- diff(asfr_r2) / diff(gdp_r)
b_gdp   <- asfr_r2[1] - a_gdp * gdp_r[1]

cor_gdp_asfr <- cor.test(gdp_sub$gdp_growth, gdp_sub$ASFR_1000)

p_gdp <- ggplot(gdp_sub, aes(x = year)) +
  geom_hline(yintercept = a_gdp * 0 + b_gdp, linetype = "dotted",
             color = "grey60", linewidth = 0.4) +
  geom_line(aes(y = ASFR_1000), color = "black", linewidth = 1.2) +
  geom_line(aes(y = a_gdp * gdp_growth + b_gdp),
            color = "#377eb8", linewidth = 0.9, alpha = 0.8) +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(
    name = "ASFR (per 1,000 women)",
    sec.axis = sec_axis(
      ~ (. - b_gdp) / a_gdp,
      name = "Real GDP growth (%)"
    )
  ) +
  labs(
    title = "ASFR vs GDP Growth - Sweden",
    subtitle = paste0(
      "Black: ASFR. Blue: real GDP growth (%). ",
      "Levels r = ", sprintf("%.2f", cor_gdp_asfr$estimate),
      " (p = ", sprintf("%.2e", cor_gdp_asfr$p.value), ")."
    ),
    x = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 20),
    plot.subtitle      = element_text(size = 11, color = "grey30"),
    axis.title.y.left  = element_text(color = "black", face = "bold", size = 13),
    axis.title.y.right = element_text(color = "#377eb8", face = "bold", size = 13),
    axis.text.y.right  = element_text(color = "#377eb8"),
    axis.text          = element_text(size = 12),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "ASFR_vs_GDP_timeseries.png"),
       p_gdp, width = 15, height = 8, dpi = 300, bg = "white")

# --- 11c. Combined 4-panel summary ---
# All three factors + ASFR on one tall figure
df_long_factors <- df_all %>%
  filter(!is.na(unemp_rate), !is.na(gdp_growth)) %>%
  select(year, ASFR_1000, affordability_index, unemp_rate, gdp_growth) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series,
    levels = c("ASFR_1000", "unemp_rate", "affordability_index", "gdp_growth"),
    labels = c("ASFR (per 1,000 women)",
               "Unemployment rate (%)",
               "Housing affordability (1971=100)",
               "Real GDP growth (%)")
  ))

p_panel <- ggplot(df_long_factors, aes(x = year, y = value)) +
  geom_line(aes(color = series), linewidth = 1.1, show.legend = FALSE) +
  geom_hline(
    data = data.frame(
      series = factor(
        c("Real GDP growth (%)"),
        levels = levels(df_long_factors$series)
      ),
      yint = 0
    ),
    aes(yintercept = yint), linetype = "dotted", color = "grey50"
  ) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(
    "ASFR (per 1,000 women)" = "black",
    "Unemployment rate (%)" = "#e41a1c",
    "Housing affordability (1971=100)" = "#d95f02",
    "Real GDP growth (%)" = "#377eb8"
  )) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  labs(
    title = "ASFR and Economic Factors - Sweden",
    subtitle = "Each panel shares the same x-axis. Compare turning points across series visually.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 20),
    plot.subtitle    = element_text(size = 12, color = "grey30"),
    strip.text       = element_text(face = "bold", size = 13),
    axis.text        = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.8, "lines"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(OUT_DIR, "ASFR_and_economic_factors_panel.png"),
       p_panel, width = 15, height = 14, dpi = 300, bg = "white")

p_panel

# =====================================================================
# 11. EXPORT
# =====================================================================

write_csv(df, "2025_update/ASFR_vs_affordability_merged.csv")

cat("\nAll plots and data saved.\n")
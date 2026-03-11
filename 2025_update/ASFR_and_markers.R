###############################################################
## Age-Standardized Fertility Rate (ASFR) - ESP 2013 weights
## Ages 15–48, Swedish women
## Linear trend fitted on 2010–2020, 95% PI projected to 2021–2025
## + historical markers / eras
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
  mutate(ASFR = Births / Population)

# =====================================================================
# 3. ESP 2013 WEIGHTS
# =====================================================================
esp2013 <- tibble(
  AgeGroup5 = c("15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49"),
  ESP_w     = c(5500, 5500, 5500, 5500, 5500, 5500, 5500)
)
esp2013 <- esp2013 %>% mutate(w = ESP_w / sum(ESP_w))

asfr_std <- rates %>%
  inner_join(esp2013, by = "AgeGroup5") %>%
  group_by(Year) %>%
  summarise(
    ASFR_std = sum(ASFR * w),
    ASFR_std_1000 = sum(ASFR * w) * 1000,
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
trend_70 <- tibble(Year = 1970:2020) %>%
  mutate(fit = predict(lm_70, newdata = .))

# --- Trend 2: 2010–2020 ---
fit_data <- asfr_std %>% filter(Year >= 2010, Year <= 2020)
lm_fit   <- lm(ASFR_std_1000 ~ Year, data = fit_data)
trend_line <- tibble(Year = 2010:2020) %>%
  mutate(fit = predict(lm_fit, newdata = .))

pi_years <- tibble(Year = 2021:2025)
pi_pred  <- predict(lm_fit, newdata = pi_years, interval = "prediction", level = 0.95)
pi_df    <- bind_cols(pi_years, as_tibble(pi_pred)) %>%
  rename(fit = fit, lwr = lwr, upr = upr)

# =====================================================================
# 5. HISTORICAL ANNOTATIONS
# =====================================================================
# Strategy: use two annotation tracks below the chart (negative y space)
# to avoid cluttering the data area. Point events get colored tick marks
# on the x-axis with a compact legend table. Era bands are shaded
# directly on the plot with minimal labels.

# --- Eras (shaded vertical bands on the chart) ---
eras <- tribble(
  ~start, ~end,  ~label,                    ~color,     ~label_y,
  1914,   1918,  "WW1",                     "#bdbdbd",  130,
  1929,   1933,  "Depression",              "#fee0d2",  130,
  1939,   1945,  "WW2",                     "#c6dbef",  130,
  1964,   1968,  "Baby boom\npeak",         "#deebf7",  130,
  1973,   1975,  "Oil crisis\ncollapse",    "#fee0d2",  122,
  1985,   1991,  "\"Speed\npremium\"",      "#e5f5e0",  130,
  1991,   1993,  "Banking\ncrisis",         "#fee0d2",  122,
  2008,   2009,  "Financial\ncrisis",       "#fee0d2",  130,
  2022,   2025,  "Post-2021\ncollapse",     "#fdd0a2",  130
)

# --- Point events (direct labels on the dotted lines) ---
# Positioning strategy:
#   - Labels go above or below the curve depending on available space
#   - Nearby events are staggered vertically to avoid overlap
#   - vjust=0 → label above y_pos, vjust=1 → label below y_pos
events <- tribble(
  ~year, ~label,                                          ~y_pos, ~vjust,
  1910,  "Preventive\nInformation Act",                    120,    0,
  1933,  "First birth\ncontrol clinic",                     18,    1,
  1934,  "Myrdal: Crisis in\nthe Population Question",      6,     1,
  1938,  "Contraception ban\nrepealed + family benefits",   30,    1,
  1948,  "Universal child\nallowance (barnbidrag)",         42,    1,
  1970,  "End of infant mortality\nas major driver",        18,    1,
  1974,  "Gender-neutral\nparental leave",                   6,    1,
  1980,  "Expanded\nparental leave",                        30,    1,
)

# =====================================================================
# 6. PLOT
# =====================================================================

p <- ggplot() +
  # Era shaded bands
  geom_rect(
    data = eras,
    aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = label),
    alpha = 0.35, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = setNames(eras$color, eras$label)
  ) +
  # Era labels
  geom_text(
    data = eras,
    aes(x = (start + end) / 2, y = label_y, label = label),
    size = 3.8, color = "grey30", fontface = "italic",
    lineheight = 0.85, vjust = 0
  ) +
  # Decade vertical lines
  geom_vline(
    xintercept = seq(1900, 2020, by = 10),
    linewidth = 0.3, color = "grey70", linetype = "dashed"
  ) +
  # Observed ASFR
  geom_line(data = asfr_std, aes(x = Year, y = ASFR_std_1000),
            linewidth = 1.3, color = "black") +
  geom_point(data = asfr_std, aes(x = Year, y = ASFR_std_1000),
             size = 1.0, color = "black") +
  # Linear trend 1970–2020
  geom_line(data = trend_70, aes(x = Year, y = fit),
            linewidth = 1.0, color = "#e7298a", linetype = "longdash") +
  # 95% PI ribbon
  geom_ribbon(data = pi_df, aes(x = Year, ymin = lwr, ymax = upr),
              fill = "#3182bd", alpha = 0.2) +
  # Linear trend 2010–2020
  geom_line(data = trend_line, aes(x = Year, y = fit),
            linewidth = 1.1, color = "#3182bd") +
  # Extrapolation
  geom_line(data = pi_df, aes(x = Year, y = fit),
            linewidth = 1.1, color = "#3182bd", linetype = "dashed") +
  geom_segment(
    aes(x = 2020, xend = 2021,
        y = tail(trend_line$fit, 1), yend = head(pi_df$fit, 1)),
    linewidth = 1.1, color = "#3182bd", linetype = "dashed"
  ) +
  # Point-event vertical markers
  geom_vline(
    data = events,
    aes(xintercept = year),
    linewidth = 0.45, color = "#d62728", linetype = "dotted"
  ) +
  # Point-event inline labels
  geom_label(
    data = events,
    aes(x = year, y = y_pos, label = label, vjust = vjust),
    size = 3.8, lineheight = 0.85,
    color = "#8b0000", fill = alpha("white", 0.88),
    label.size = 0.3, label.padding = unit(0.25, "lines"),
    fontface = "plain"
  ) +
  # Connector segments from label to curve
  geom_segment(
    data = events %>% 
      left_join(asfr_std %>% select(Year, ASFR_std_1000), by = c("year" = "Year")),
    aes(x = year, xend = year,
        y = ifelse(vjust == 0, y_pos - 1, y_pos + 1),
        yend = ASFR_std_1000),
    linewidth = 0.35, color = "#d62728", linetype = "dotted"
  ) +
  # Trend slope annotations
  annotate("text", x = 1975,
           y = predict(lm_70, newdata = tibble(Year = 1975)) - 7,
           label = paste0("1970-2020: ", sprintf("%+.2f", coef(lm_70)[2]), "/yr"),
           color = "#e7298a", fontface = "bold", size = 4.8, hjust = 0) +
  annotate("text", x = 2001,
           y = predict(lm_fit, newdata = tibble(Year = 2010)) + 6,
           label = paste0("2010-2020: ", sprintf("%+.2f", coef(lm_fit)[2]), "/yr"),
           color = "#3182bd", fontface = "bold", size = 4.8, hjust = 0) +
  scale_x_continuous(
    breaks = seq(1900, 2025, by = 10),
    limits = c(NA, 2028)
  ) +
  scale_y_continuous(
    limits = c(-10, 140),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Age-Standardized Fertility Rate (ESP 2013) - Sweden, 1891-2025",
    subtitle = "ASFR across 5-year groups, ages 15-48. Shaded bands: demographic eras. Red dotted: policy milestones.",
    x = NULL, y = "ASFR (per 1,000 women)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(face = "bold", size = 22),
    plot.subtitle      = element_text(size = 14, color = "grey30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 14),
    axis.text.y        = element_text(size = 14),
    axis.title.y       = element_text(size = 15, face = "bold"),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(t = 14, r = 14, b = 14, l = 14)
  )

p

ggsave(file.path(OUT_DIR, "ASFR_ESP2013_trend_PI_1891_2025_marked.png"),
       p, width = 18, height = 9, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "ASFR_ESP2013_trend_PI_1891_2025_marked.pdf"),
       p, width = 18, height = 9, bg = "white")

cat("ASFR plot with historical markers saved.\n")
###############################################################
## Housing Affordability Pressure - Sweden
##
## Indicator: Real House Price Index / Real Labour Cost Index
##
## QSER628BIS is already CPI-deflated by BIS (2010=100, real).
## LCEAMN01SEA661N is nominal (2015=100).
## SWECPIALLAINMEI is CPI (2015=100).
##
## We deflate wages by CPI to get real wages, then compute
## the ratio of real house prices to real wages.
## Both are rebased to 1971=100 (first overlapping year).
##
## Result: > 100 = real house prices outpaced real wages since 1971
##
## Note: LCEAMN01SEA661N is total labour compensation per hour
## (incl. employer contributions), not take-home wages.
##
## Sources (FRED):
##   - QSER628BIS       : BIS Real Residential Property Prices (2010=100)
##   https://fred.stlouisfed.org/series/QSER628BIS
##   - LCEAMN01SEA661N  : OECD Labour Cost Index, nominal (2015=100)
##   https://fred.stlouisfed.org/series/LCEAMN01SEA661N
##   - SWECPIALLAINMEI  : OECD CPI (2015=100)
##   https://fred.stlouisfed.org/series/SWECPIALLAINMEI
###############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(scales)
})

OUT_DIR <- "visual"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =====================================================================
# 1. LOAD DATA
# =====================================================================

house <- read_csv("2025_update/QSER628BIS.csv", show_col_types = FALSE)
wage  <- read_csv("2025_update/LCEAMN01SEA661N.csv", show_col_types = FALSE)
cpi   <- read_csv("2025_update/SWECPIALLAINMEI.csv", show_col_types = FALSE)

# =====================================================================
# 2. CLEAN & ANNUALISE
# =====================================================================

# House prices (real, quarterly → annual average)
house <- house %>%
  mutate(year = year(as.Date(observation_date))) %>%
  group_by(year) %>%
  summarise(real_price = mean(QSER628BIS, na.rm = TRUE), .groups = "drop")

# Wages (nominal, annual)
wage <- wage %>%
  mutate(year = year(as.Date(observation_date)),
         nom_wages = LCEAMN01SEA661N) %>%
  select(year, nom_wages)

# CPI (annual)
cpi <- cpi %>%
  mutate(year = year(as.Date(observation_date)),
         cpi = SWECPIALLAINMEI) %>%
  select(year, cpi)

# =====================================================================
# 3. MERGE, DEFLATE WAGES, COMPUTE RATIO
# =====================================================================

df <- house %>%
  inner_join(wage, by = "year") %>%
  inner_join(cpi, by = "year") %>%
  arrange(year)

# Deflate nominal wages by CPI to get real wages
# Both CPI and wages are 2015=100, so real_wages is also based at 2015
df <- df %>%
  mutate(real_wages = nom_wages / cpi * 100)  # *100 to keep index scale

BASE_YEAR <- min(df$year)
cat("Base year:", BASE_YEAR, "\n")

# Rebase both real series to BASE_YEAR = 100
base_price <- df %>% filter(year == BASE_YEAR) %>% pull(real_price)
base_rwage <- df %>% filter(year == BASE_YEAR) %>% pull(real_wages)

df <- df %>%
  mutate(
    price_rebased = 100 * real_price / base_price,
    rwages_rebased = 100 * real_wages / base_rwage,
    affordability_index = price_rebased / rwages_rebased * 100
  )

cat("Year range:", min(df$year), "-", max(df$year), "\n")
cat("Affordability at base:", 
    df %>% filter(year == BASE_YEAR) %>% pull(affordability_index), "\n")
cat("Affordability latest:", 
    df %>% slice_tail(n = 1) %>% pull(affordability_index), "\n")

# =====================================================================
# 4. PLOT
# =====================================================================

p <- ggplot(df, aes(x = year)) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50",
             linewidth = 0.5) +
  # Rebased real indices
  geom_line(aes(y = price_rebased, color = "Real house prices (BIS, CPI-deflated)"),
            linewidth = 1.0) +
  geom_line(aes(y = rwages_rebased, color = "Real labour costs (OECD, CPI-deflated)"),
            linewidth = 1.0) +
  # Affordability ratio
  geom_line(aes(y = affordability_index, color = "Affordability ratio"),
            linewidth = 1.3) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Real house prices (BIS, CPI-deflated)"    = "#d95f02",
      "Real labour costs (OECD, CPI-deflated)"   = "#1b9e77",
      "Affordability ratio"                       = "black"
    )
  ) +
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  annotate("text", x = 2005, y = 90,
           label = paste0("100 = parity at ", BASE_YEAR, " level"),
           color = "grey40", size = 4, fontface = "italic") +
  labs(
    title    = "Housing Affordability Pressure - Sweden",
    subtitle = paste0(
      "Real House Prices / Real Labour Costs (both CPI-deflated), rebased to ",
      BASE_YEAR, " = 100. Black > 100 \u2192 houses outpaced wages. Sources: BIS + OECD via FRED."
    ),
    x = NULL,
    y = paste0("Index (", BASE_YEAR, " = 100)")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 20),
    plot.subtitle    = element_text(size = 12, color = "grey30"),
    legend.position  = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text      = element_text(size = 12),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    axis.text        = element_text(size = 12),
    axis.title.y     = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p

ggsave(file.path(OUT_DIR, "housing_affordability_sweden.png"),
       p, width = 14, height = 8, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "housing_affordability_sweden.pdf"),
       p, width = 14, height = 8, bg = "white")

# =====================================================================
# 5. EXPORT
# =====================================================================

write_csv(
  df %>% select(year, real_price, nom_wages, cpi, real_wages,
                price_rebased, rwages_rebased, affordability_index),
  "2025_update/housing_affordability_sweden_1971_present.csv"
)

cat("All outputs saved.\n")

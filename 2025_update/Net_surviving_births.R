###############################################################
## Net Surviving Births - Sweden
##
## "New live births actually feeding the population"
## = Births × (1 - IMR/1000)
##
## This gives the number of infants surviving their first year,
## i.e. the effective annual addition to the population from births.
##
## Using the same-year IMR is the standard demographic approach:
## it avoids the timing mismatch of subtracting year N+1 deaths
## from year N births (since cohort exposure straddles calendar years).
##
## Sources:
##   - Total births : 2025_update/1891_to_2025_births.csv
##   - IMR          : 2025_update/000000MM_20260309-204950.csv
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
# 1. LOAD TOTAL BIRTHS PER YEAR (all ages summed)
# =====================================================================
births_raw <- read_csv("2025_update/1891_to_2025_births.csv",
                       show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year), Births = as.numeric(Births))

total_births <- births_raw %>%
  group_by(Year) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year)

cat("Total births:", nrow(total_births), "years,",
    min(total_births$Year), "-", max(total_births$Year), "\n")

# =====================================================================
# 2. LOAD INFANT MORTALITY RATE (per 1,000 live births)
# =====================================================================
imr_raw <- read_delim(
  "2025_update/000000MM_20260309-204950.csv",
  delim = ";", skip = 2, col_names = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

imr <- imr_raw %>%
  slice(1) %>%
  select(-1) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "col_name", values_to = "Rate") %>%
  filter(str_detect(col_name, "first year")) %>%
  mutate(
    Year = as.integer(str_extract(col_name, "\\d{4}$")),
    IMR  = as.numeric(ifelse(Rate == "..", NA, Rate))
  ) %>%
  select(Year, IMR) %>%
  filter(!is.na(IMR))

cat("IMR loaded:", nrow(imr), "years,",
    min(imr$Year), "-", max(imr$Year), "\n")

# =====================================================================
# 3. COMPUTE NET SURVIVING BIRTHS
# =====================================================================
df <- total_births %>%
  inner_join(imr, by = "Year") %>%
  mutate(
    InfantDeaths    = Births * IMR / 1000,
    SurvivingBirths = Births - InfantDeaths,
    SurvivalRate    = 1 - IMR / 1000
  )

cat("Net surviving births computed:", nrow(df), "years,",
    min(df$Year), "-", max(df$Year), "\n")

# =====================================================================
# 4. PLOT
# =====================================================================

# Long format for stacked/overlaid plotting
df_long <- df %>%
  select(Year, Births, SurvivingBirths, InfantDeaths) %>%
  pivot_longer(-Year, names_to = "Metric", values_to = "Count")

# --- Main plot: total births vs surviving births, with infant deaths shaded ---
p <- ggplot(df) +
  # Total births (upper boundary)
  geom_area(aes(x = Year, y = Births),
            fill = "#d95f02", alpha = 0.25) +
  # Surviving births (lower solid area)
  geom_area(aes(x = Year, y = SurvivingBirths),
            fill = "#1b9e77", alpha = 0.4) +
  # Lines on top for clarity
  geom_line(aes(x = Year, y = Births),
            linewidth = 0.7, color = "#d95f02") +
  geom_line(aes(x = Year, y = SurvivingBirths),
            linewidth = 0.9, color = "#1b9e77") +
  # Annotations
  annotate("text", x = 1935, y = max(df$Births) * 0.92,
           label = "Total live births", color = "#d95f02",
           fontface = "bold", size = 4.5, hjust = 0) +
  annotate("text", x = 1935, y = max(df$Births) * 0.82,
           label = "Surviving births (past 1st year)", color = "#1b9e77",
           fontface = "bold", size = 4.5, hjust = 0) +
  annotate("text", x = 1910, y = max(df$Births) * 0.55,
           label = "Infant deaths\n(gap between curves)", color = "#d95f02",
           fontface = "italic", size = 3.8, hjust = 0.5, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_number(scale_cut = cut_si(""))
  ) +
  labs(
    title    = "Net Surviving Births - Sweden (1891-2024)",
    subtitle = "Surviving births = total live births \u00d7 (1 \u2212 IMR/1,000). Orange area: infant deaths. Sources: HFD + SCB.",
    x = NULL, y = "Number of births per year"
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

ggsave(file.path(OUT_DIR, "net_surviving_births_1891_2024.png"),
       p, width = 14, height = 7, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "net_surviving_births_1891_2024.pdf"),
       p, width = 14, height = 7, bg = "white")

# --- Supplementary: survival rate over time ---
p2 <- ggplot(df, aes(x = Year, y = SurvivalRate * 100)) +
  geom_line(linewidth = 0.9, color = "#1b9e77") +
  geom_point(size = 0.6, color = "#1b9e77") +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, by = 2)) +
  labs(
    title    = "First-Year Survival Rate - Sweden (1891-2024)",
    subtitle = "% of live births surviving past age 1. Sources: SCB.",
    x = NULL, y = "Survival rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p2

ggsave(file.path(OUT_DIR, "first_year_survival_rate_1891_2024.png"),
       p2, width = 14, height = 7, dpi = 300, bg = "white")

cat("All plots saved.\n")
###############################################################
## Swedish Women Population Analysis
## 1. Merge two SCB CSV files into one dataframe
## 2. Plot evolution of 5-year age groups (15-48) from 1860-2025
## 3. Cohort-aging heatmap (all ages, 2000-2025)
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
# 1. LOAD & MERGE
# =====================================================================

# --- File A: 1860-2024 (wide format, semicolon-separated) ---
raw_a <- read_delim(
  "2025_update/0000053A_20260309-173435.csv",
  delim       = ";",
  skip        = 2,
  col_names   = TRUE,
  show_col_types = FALSE,
  locale      = locale(encoding = "UTF-8")
)

# Parse age from "0 years", "1 year", "100+ years" etc.
parse_age <- function(x) {
  x <- str_trim(x)
  ifelse(str_detect(x, "\\+"), NA_integer_,
         as.integer(str_extract(x, "^\\d+")))
}

# Pivot to long
pop_a <- raw_a %>%
  rename(age_raw = 1, sex = 2) %>%
  mutate(Age = parse_age(age_raw)) %>%
  filter(!is.na(Age)) %>%
  select(-age_raw, -sex) %>%
  pivot_longer(-Age, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year),
         Population = as.numeric(Population))

# --- File B: 2025 (single-year, semicolon-separated) ---
raw_b <- read_delim(
  "2025_update/0000088D_20260309-173522.csv",
  delim       = ";",
  skip        = 2,
  col_names   = TRUE,
  show_col_types = FALSE,
  locale      = locale(encoding = "UTF-8")
)

pop_b <- raw_b %>%
  rename(age_raw = 1, sex = 2) %>%
  mutate(Age = parse_age(age_raw)) %>%
  filter(!is.na(Age)) %>%
  select(-age_raw, -sex) %>%
  pivot_longer(-Age, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year),
         Population = as.numeric(Population))

# --- Merge (bind rows, remove duplicates keeping latest file) ---
pop <- bind_rows(pop_a, pop_b) %>%
  distinct(Year, Age, .keep_all = TRUE) %>%
  arrange(Year, Age) %>%
  filter(!is.na(Population))

cat("Merged data:", nrow(pop), "rows,",
    "Years:", min(pop$Year), "-", max(pop$Year),
    ", Ages: 0 -", max(pop$Age), "\n")

# --- Export merged long-format data ---
write_csv(pop, "2025_update/1860_to_2025_women_population.csv")
cat("Merged CSV written to 2025_update/1860_to_2025_women_population.csv\n")

# =====================================================================
# HELPER: 5-year age group labels
# =====================================================================
max_age <- max(pop$Age, na.rm = TRUE)

mk_group5 <- function(a) {
  gs <- (a %/% 5) * 5
  ge <- pmin(gs + 4, max_age)
  sprintf("%02d-%02d", gs, ge)
}

# =====================================================================
# 2. LINE PLOTS - Women 15-48, 5-year groups, 1860-2025
# =====================================================================

pop_fertile <- pop %>%
  filter(Age >= 15, Age <= 48) %>%
  mutate(AgeGroup5 = mk_group5(Age),
         grp_start = (Age %/% 5) * 5) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  mutate(AgeGroup5 = factor(AgeGroup5,
                            levels = sort(unique(mk_group5(seq(15, 48))))))

# -- Faceted line plot --
p_lines <- ggplot(pop_fertile, aes(x = Year, y = Population)) +
  geom_line(aes(color = AgeGroup5), linewidth = 0.9) +
  facet_wrap(~ AgeGroup5, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1860, 2030, by = 20)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "Swedish Women - Population by 5-Year Age Group (15-48)",
    subtitle = "Source: SCB, 1860-2025",
    x = NULL, y = "Population"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text       = element_text(face = "bold", size = 13),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_lines

ggsave(file.path(OUT_DIR, "women_15_48_faceted_lines_1860_2025.png"),
       p_lines, width = 14, height = 12, dpi = 300, bg = "white")

# -- Combined (all groups on one panel) --
p_combined <- ggplot(pop_fertile, aes(x = Year, y = Population,
                                       color = AgeGroup5)) +
  geom_line(linewidth = 0.85) +
  scale_x_continuous(breaks = seq(1860, 2030, by = 20)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
  scale_color_brewer(palette = "Set2", name = "Age Group") +
  labs(
    title    = "Swedish Women - Population by 5-Year Age Group (15-48)",
    subtitle = "Source: SCB, 1860-2025",
    x = NULL, y = "Population"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 12),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_combined

ggsave(file.path(OUT_DIR, "women_15_48_combined_lines_1860_2025.png"),
       p_combined, width = 16, height = 8, dpi = 300, bg = "white")

cat("Line plots saved.\n")

# =====================================================================
# 3. COHORT-AGING HEATMAP - All ages, 2000-2025
#    Δ(N,A) = Pop(N,A) - Pop(N-1,A-1)  (cohort aging delta)
# =====================================================================

HIGHLIGHT_THRESHOLD <- 10000L   # outline cells when |Δ| > this

# --- Single-age cohort deltas ---
delta_long <- pop %>%
  mutate(Year_prev = Year - 1L, Age_prev = Age - 1L) %>%
  left_join(
    pop %>% select(Year, Age, Population) %>%
      rename(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
    by = c("Year_prev", "Age_prev")
  ) %>%
  transmute(Year, Age, delta = Population - Pop_prev)

# --- 5-year grouped population & delta ---
pop_group_year <- pop %>%
  mutate(AgeGroup5 = mk_group5(Age),
         grp_start = (Age %/% 5) * 5) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

delta_group_year <- delta_long %>%
  mutate(AgeGroup5 = mk_group5(Age),
         grp_start  = (Age %/% 5) * 5) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Delta = sum(delta, na.rm = TRUE), .groups = "drop")

# --- Merge & restrict to 2000-2025 ---
YEAR_MIN <- 2000L
YEAR_MAX <- max(pop$Year)

df5 <- pop_group_year %>%
  left_join(delta_group_year, by = c("AgeGroup5", "grp_start", "Year")) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

group_levels <- df5 %>% distinct(AgeGroup5, grp_start) %>%
  arrange(grp_start) %>% pull(AgeGroup5)
df5 <- df5 %>% mutate(AgeGroup5 = factor(AgeGroup5, levels = group_levels))

# --- Labels: Population + Δ ---
fmt_si     <- label_number(accuracy = 0.1, scale_cut = cut_si(""))
fmt_si_int <- label_number(accuracy = 1,   scale_cut = cut_si(""))

df5 <- df5 %>%
  mutate(
    pop_lbl   = fmt_si(Population),
    delta_lbl = ifelse(is.na(Delta), "",
                       paste0(ifelse(Delta > 0, "+", ""), fmt_si_int(Delta))),
    label     = ifelse(delta_lbl == "", pop_lbl,
                       paste0(pop_lbl, "\n\u0394 ", delta_lbl))
  )

# --- Color clamping ---
q98   <- quantile(abs(df5$Delta), 0.98, na.rm = TRUE)
limit <- max(HIGHLIGHT_THRESHOLD, q98, na.rm = TRUE)
df5 <- df5 %>%
  mutate(
    delta_clamped = pmax(pmin(Delta, limit), -limit),
    extreme       = !is.na(Delta) & abs(Delta) > HIGHLIGHT_THRESHOLD
  )

lim <- max(abs(df5$delta_clamped), na.rm = TRUE)
df5 <- df5 %>%
  mutate(text_col = case_when(
    is.na(delta_clamped) ~ "black",
    abs(delta_clamped) > 0.55 * lim ~ "white",
    TRUE ~ "black"
  ))

# --- Heatmap ---
p_heat <- ggplot(df5, aes(x = Year, y = AgeGroup5, fill = delta_clamped)) +
  geom_tile(color = "grey80", linewidth = 0.25) +
  geom_tile(
    data = subset(df5, extreme),
    aes(x = Year, y = AgeGroup5),
    fill = NA, color = "black", linewidth = 0.5
  ) +
  geom_text(
    aes(label = label, color = text_col),
    size = 2.4, lineheight = 0.88
  ) +
  scale_color_identity() +
  scale_x_continuous(breaks = seq(YEAR_MIN, YEAR_MAX, by = 1)) +
  scale_fill_gradient2(
    name     = "\u0394 (sum over ages in group of Pop[N,A] \u2013 Pop[N\u22121,A\u22121])",
    low      = "#b30000", mid = "#f2f2f2", high = "#007a1f",
    midpoint = 0, limits = c(-lim, lim),
    labels   = label_number(scale_cut = cut_si("")),
    na.value = "grey95"
  ) +
  labs(
    title    = "Cohort Aging \u2014 5-Year Age Groups, Swedish Women (Population & \u0394)",
    subtitle = paste0(
      "Years ", YEAR_MIN, "\u2013", YEAR_MAX,
      ". Cells outlined when |\u0394| > ", comma(HIGHLIGHT_THRESHOLD),
      ". Red = negative \u0394 (loss), Green = positive \u0394 (gain)."
    ),
    x = NULL, y = "Age Group (5y)"
  ) +
  guides(
    fill = guide_colorbar(
      direction      = "horizontal",
      title.position = "top",
      barwidth       = unit(5, "in"),
      barheight      = unit(0.25, "in")
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid         = element_blank(),
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(size = 11),
    axis.text.x        = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 10),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text        = element_text(size = 10),
    legend.background  = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(10, 14, 10, 14)
  )

p_heat

ggsave(file.path(OUT_DIR, "cohort_heatmap_5y_women_2000_2025.png"),
       p_heat, width = 22, height = 14, dpi = 300, bg = "white")
ggsave(file.path(OUT_DIR, "cohort_heatmap_5y_women_2000_2025.pdf"),
       p_heat, width = 22, height = 14, bg = "white")

cat("Heatmap saved.\n")
cat("Done - all outputs in:", OUT_DIR, "\n")
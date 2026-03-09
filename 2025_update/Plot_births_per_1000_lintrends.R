###############################################################
## Births per 1,000 women by 5-year age group (15–48)
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

# Parse HFD age: numeric ages + "12-" and "55+" → NA
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
    str_detect(x, "^-\\d+ year")         ~ NA_integer_,  # under-14
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
  # Sum men + women babies per mother age & year
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
  filter(!str_detect(str_trim(sex), "^total")) %>%   # drop "total" rows
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

# --- Export merged births ---
write_csv(births, "2025_update/1891_to_2025_births.csv")
cat("Merged births CSV written to 2025_update/1891_to_2025_births.csv\n")

# =====================================================================
# 3. COMPUTE BIRTHS / 1,000 WOMEN by 5-year age group
# =====================================================================

mk_group5 <- function(a) {
  gs <- (a %/% 5) * 5
  ge <- gs + 4
  sprintf("%02d-%02d", gs, ge)
}

# Restrict to ages 15–48
ages_keep <- 15:48

# Births grouped
births_grp <- births %>%
  filter(MotherAge %in% ages_keep) %>%
  mutate(AgeGroup5 = mk_group5(MotherAge)) %>%
  group_by(Year, AgeGroup5) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

# Population grouped
pop_grp <- pop %>%
  filter(Age %in% ages_keep) %>%
  mutate(AgeGroup5 = mk_group5(Age)) %>%
  group_by(Year, AgeGroup5) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# Join & compute rate
rates <- births_grp %>%
  inner_join(pop_grp, by = c("Year", "AgeGroup5")) %>%
  mutate(Rate = Births / Population * 1000) %>%
  mutate(AgeGroup5 = factor(AgeGroup5,
                            levels = sort(unique(mk_group5(ages_keep)))))

cat("Rate data:", nrow(rates), "rows,",
    "Years:", min(rates$Year), "-", max(rates$Year), "\n")

# =====================================================================
# 4. PLOTS
# =====================================================================

# Color palette
age_groups <- levels(rates$AgeGroup5)
n_groups   <- length(age_groups)
pal        <- setNames(
  c("#e41a1c", "#ff7f00", "#4daf4a", "#377eb8",
    "#984ea3", "#a65628", "#f781bf")[seq_len(n_groups)],
  age_groups
)

# -- A) Faceted line plot --
p_facet <- ggplot(rates, aes(x = Year, y = Rate)) +
  geom_line(aes(color = AgeGroup5), linewidth = 0.9) +
  facet_wrap(~ AgeGroup5, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_color_manual(values = pal, guide = "none") +
  labs(
    title    = "Age-Specific Fertility Rate - Swedish Women (15-48)",
    subtitle = "Births per 1,000 women in each 5-year age group. Sources: HFD (1891-1967) + SCB (1968-2025).",
    x = NULL, y = "Births / 1,000 women"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text       = element_text(face = "bold", size = 13),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_facet

ggsave(file.path(OUT_DIR, "fertility_rate_faceted_1891_2025.png"),
       p_facet, width = 14, height = 12, dpi = 300, bg = "white")

# -- B) Combined overlay --
p_combined <- ggplot(rates, aes(x = Year, y = Rate, color = AgeGroup5)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_color_manual(values = pal, name = "Age Group") +
  labs(
    title    = "Age-Specific Fertility Rate - Swedish Women (15-48)",
    subtitle = "Births per 1,000 women in each 5-year age group. Sources: HFD (1891-1967) + SCB (1968-2025).",
    x = NULL, y = "Births / 1,000 women"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_combined

ggsave(file.path(OUT_DIR, "fertility_rate_combined_1891_2025.png"),
       p_combined, width = 16, height = 8, dpi = 300, bg = "white")

cat("All plots saved in:", OUT_DIR, "\n")
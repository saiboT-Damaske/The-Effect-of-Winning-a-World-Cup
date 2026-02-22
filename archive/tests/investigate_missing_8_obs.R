############################################################
# Investigate 8 missing observations vs. paper Table 1
#
# Paper:  1295 winners,  7342 non-winners  (total 8637)
# Ours:   1291 winners,  7338 non-winners  (total 8629)
# Diff:     +4 winners,    +4 non-winners  (total    8)
#
# Hypothesis: the 8 missing obs come from late-start countries
# (e.g., Brazil, Argentina) where our pipeline recomputes YoY
# inside the trimmed window — killing the first 4 quarters
# whose 4-quarter lag falls before the country's start date.
# The paper may keep those if the raw OECD data goes back
# further than the stated "GDP start".
############################################################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# ═══════════════════════════════════════════════════════════
# 0) Shared definitions
# ═══════════════════════════════════════════════════════════

safe_log <- function(x) {
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & x > 0)
  out[idx] <- log(x[idx])
  out
}

controls <- c(
  "ARG","AUS","AUT","BEL","BGR","CAN","CHL","COL","CRI","HRV","CZE","DNK","EST","FIN",
  "GRC","HUN","IND","IDN","ISL","IRL","ISR","LVA","LTU","LUX","NLD","NZL","NOR","POL",
  "PRT","ROU","SAU","SVK","SVN","SWE","CHE","TUR"
)
hosts <- c(
  "BRA","GBR","FRA","DEU","ITA","JPN","MEX","ZAF","KOR","ESP","RUS","USA"
)
paper_countries <- sort(unique(c(controls, hosts)))

# Winner countries (ever won in-sample: 1966–2018)
winner_countries <- c("BRA","DEU","ESP","FRA","GBR","ITA")

# Paper targets
paper_winner_obs    <- 1295
paper_nonwinner_obs <- 7342


# ═══════════════════════════════════════════════════════════
# 1) Load the FULL enriched panel (YoY computed on all OECD data, pre-trim)
# ═══════════════════════════════════════════════════════════

master <- read_csv(
  "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
  show_col_types = FALSE
)

master <- master %>%
  mutate(
    year    = as.integer(str_extract(quarter, "^\\d{4}")),
    qtr     = as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    q_index = year * 4L + qtr
  )

# ═══════════════════════════════════════════════════════════
# 2) Check actual OECD GDP coverage per country
#    (i.e., when does the RAW data start, before any trim?)
# ═══════════════════════════════════════════════════════════

oecd_coverage <- master %>%
  filter(country %in% paper_countries, !is.na(gdp)) %>%
  group_by(country) %>%
  summarise(
    oecd_first_q = min(quarter),
    oecd_last_q  = max(quarter),
    n_gdp_quarters = n(),
    .groups = "drop"
  ) %>%
  arrange(country)

cat("══════════════════════════════════════════════════════\n")
cat("  OECD GDP coverage for paper countries\n")
cat("══════════════════════════════════════════════════════\n\n")

# Focus on late-start countries (start after 1960)
late_start <- oecd_coverage %>% filter(oecd_first_q > "1960-Q1")
cat("Countries with GDP data starting AFTER 1960-Q1:\n")
print(as.data.frame(late_start), row.names = FALSE)

cat("\nAll countries starting at 1960-Q1:\n")
early <- oecd_coverage %>% filter(oecd_first_q == "1960-Q1")
cat("  ", paste(early$country, collapse = ", "), "\n")
cat("  Count:", nrow(early), "\n\n")


# ═══════════════════════════════════════════════════════════
# 3) Also check YoY coverage from the MASTER panel
#    (YoY computed on full OECD data, pre-trim)
# ═══════════════════════════════════════════════════════════

yoy_coverage_master <- master %>%
  filter(country %in% paper_countries, !is.na(gdp_yoy_log_4q)) %>%
  group_by(country) %>%
  summarise(
    first_yoy_q = min(quarter),
    last_yoy_q  = max(quarter),
    n_yoy       = n(),
    .groups = "drop"
  )

cat("YoY coverage from MASTER panel (computed on all OECD data):\n")
late_yoy <- yoy_coverage_master %>% filter(first_yoy_q > "1961-Q1")
cat("Countries with first valid YoY AFTER 1961-Q1:\n")
print(as.data.frame(late_yoy), row.names = FALSE)
cat("\n")


# ═══════════════════════════════════════════════════════════
# 4) Helper: classify winners and count obs for any panel
# ═══════════════════════════════════════════════════════════

count_obs <- function(df, label) {
  # Winner = country ever won WC in our dataset (rank1 == 1 in any row)
  winner_by_country <- df %>%
    group_by(country) %>%
    summarise(is_winner = any(rank1 == 1, na.rm = TRUE), .groups = "drop")
  
  df2 <- df %>%
    left_join(winner_by_country, by = "country") %>%
    mutate(group = if_else(is_winner, "Winner", "Non-winner"))
  
  obs <- df2 %>%
    group_by(group) %>%
    summarise(n = n(), n_cty = n_distinct(country), .groups = "drop")
  
  w  <- obs %>% filter(group == "Winner")    %>% pull(n)  %>% {if(length(.) == 0) 0L else .}
  nw <- obs %>% filter(group == "Non-winner") %>% pull(n) %>% {if(length(.) == 0) 0L else .}
  
  tibble(
    variant         = label,
    winner_obs      = w,
    nonwinner_obs   = nw,
    total_obs       = w + nw,
    diff_winner     = w - paper_winner_obs,
    diff_nonwinner  = nw - paper_nonwinner_obs,
    diff_total      = (w + nw) - (paper_winner_obs + paper_nonwinner_obs),
    n_countries     = n_distinct(df2$country)
  )
}


# ═══════════════════════════════════════════════════════════
# 5) Build multiple sample variants
# ═══════════════════════════════════════════════════════════

results <- list()

# ----------------------------------------------------------
# VARIANT A: CURRENT PIPELINE (trim first → recompute YoY)
# This is what create_paper_replication_es_csv.R now does:
# 1. Filter to paper countries + 1961-Q1..2021-Q4
# 2. Filter Brazil ≥ 1998-Q2
# 3. Recompute YoY within the trimmed window
# 4. Drop NA YoY
# ----------------------------------------------------------

df_a <- master %>%
  filter(country %in% paper_countries,
         quarter >= "1961-Q1", quarter <= "2021-Q4") %>%
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  arrange(country, q_index) %>%
  # Recompute YoY within trimmed window
  group_by(country) %>%
  arrange(q_index, .by_group = TRUE) %>%
  mutate(
    gdp_yoy_log_4q = (safe_log(gdp) - lag(safe_log(gdp), 4)) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(gdp_yoy_log_4q))

results[[1]] <- count_obs(df_a, "A: trim→recompute YoY (current)")


# ----------------------------------------------------------
# VARIANT B: MASTER YOY (YoY from full OECD, then trim)
# Use pre-computed YoY from master panel (uses all available
# OECD data for lags).  Then apply paper filters.
# This gives late-start countries extra obs if OECD data
# exists before their "GDP start" date.
# BUT gives too many obs for 1961-Q1 countries.
# ----------------------------------------------------------

df_b <- master %>%
  filter(country %in% paper_countries,
         quarter >= "1961-Q1", quarter <= "2021-Q4") %>%
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  filter(!is.na(gdp_yoy_log_4q))

results[[2]] <- count_obs(df_b, "B: master YoY→trim (too many for 1961)")


# ----------------------------------------------------------
# VARIANT C: HYBRID — recompute for 1961-start, master for late-start
# For countries whose OECD data starts at 1960-Q1: recompute
# YoY within 1961+ window (first valid = 1962-Q1).
# For late-start countries: use master YoY (preserves their
# first 4 quarters of YoY).
# ----------------------------------------------------------

# Identify late-start countries (OECD GDP starts after 1960-Q1)
late_countries <- oecd_coverage %>%
  filter(oecd_first_q > "1960-Q1") %>%
  pull(country)

cat("Late-start countries (OECD data after 1960-Q1):\n  ",
    paste(late_countries, collapse = ", "), "\n\n")

# 1961-start countries: trim and recompute
df_c_early <- master %>%
  filter(country %in% paper_countries,
         !(country %in% late_countries),
         quarter >= "1961-Q1", quarter <= "2021-Q4") %>%
  arrange(country, q_index) %>%
  group_by(country) %>%
  mutate(gdp_yoy_log_4q = (safe_log(gdp) - lag(safe_log(gdp), 4)) * 100) %>%
  ungroup()

# Late-start countries: use master YoY, trim to paper window
df_c_late <- master %>%
  filter(country %in% late_countries,
         country %in% paper_countries,
         quarter >= "1961-Q1", quarter <= "2021-Q4") %>%
  filter(!(country == "BRA" & quarter < "1998-Q2"))

df_c <- bind_rows(df_c_early, df_c_late) %>%
  filter(!is.na(gdp_yoy_log_4q))

results[[3]] <- count_obs(df_c, "C: hybrid (recompute 1961, master for late)")


# ----------------------------------------------------------
# VARIANT D: Like A, but DON'T filter Brazil to 1998-Q2
#            (let Brazil keep all available data)
# ----------------------------------------------------------

df_d <- master %>%
  filter(country %in% paper_countries,
         quarter >= "1961-Q1", quarter <= "2021-Q4") %>%
  arrange(country, q_index) %>%
  group_by(country) %>%
  mutate(gdp_yoy_log_4q = (safe_log(gdp) - lag(safe_log(gdp), 4)) * 100) %>%
  ungroup() %>%
  filter(!is.na(gdp_yoy_log_4q))

results[[4]] <- count_obs(df_d, "D: like A but no Brazil 1998-Q2 filter")


# ----------------------------------------------------------
# VARIANT E: Compute YoY on ALL available data per country,
#            then trim: countries starting 1960 → keep from 1962-Q1,
#            Brazil → keep from 1998-Q2,
#            other late-start → keep from their first valid YoY quarter.
# This mimics: "work with whatever OECD gives you, compute YoY,
# then apply paper's date trims."
# ----------------------------------------------------------

# Paper Table A1 stated GDP start dates for late-start countries:
paper_starts <- tribble(
  ~country, ~paper_start,
  "BRA",    "1998-Q2",
  "ARG",    "1993-Q1",
  "RUS",    "1995-Q1",
  "CHL",    "1996-Q1"
  # All other paper countries: 1961-Q1
)

df_e <- master %>%
  filter(country %in% paper_countries, quarter <= "2021-Q4") %>%
  # Use master YoY (computed on full OECD data)
  filter(!is.na(gdp_yoy_log_4q)) %>%
  # Apply per-country start filters
  left_join(paper_starts, by = "country") %>%
  mutate(
    effective_start = coalesce(paper_start, "1962-Q1")  # 1962-Q1 for most (1961 + 4q lag)
  ) %>%
  filter(quarter >= effective_start) %>%
  select(-paper_start, -effective_start)

results[[5]] <- count_obs(df_e, "E: master YoY + paper Table A1 start dates")


# ----------------------------------------------------------
# VARIANT F: Like E, but 1961-start countries begin at 1961-Q1
#            (allows 1961 YoY using 1960 lag — probably too many)
# ----------------------------------------------------------

df_f <- master %>%
  filter(country %in% paper_countries, quarter <= "2021-Q4") %>%
  filter(!is.na(gdp_yoy_log_4q)) %>%
  left_join(paper_starts, by = "country") %>%
  mutate(
    effective_start = coalesce(paper_start, "1961-Q1")
  ) %>%
  filter(quarter >= effective_start) %>%
  select(-paper_start, -effective_start)

results[[6]] <- count_obs(df_f, "F: like E but 1961 countries start at 1961-Q1")


# ═══════════════════════════════════════════════════════════
# 6) Comparison table
# ═══════════════════════════════════════════════════════════

comparison <- bind_rows(results)

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  OBSERVATION COUNT COMPARISON (Paper target: W=1295, NW=7342)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

print(as.data.frame(comparison), row.names = FALSE)


# ═══════════════════════════════════════════════════════════
# 7) Drill down: which quarters differ between variants?
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  DRILL DOWN: extra obs in best-matching variant vs. current (A)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Find best matching variant
best_idx <- which.min(abs(comparison$diff_total))
best_label <- comparison$variant[best_idx]
cat("Best matching variant:", best_label, "\n\n")

# Build a list of all variant dataframes for drill-down
variant_dfs <- list(A = df_a, B = df_b, C = df_c, D = df_d, E = df_e, F = df_f)
best_key <- LETTERS[best_idx]
df_best <- variant_dfs[[best_key]]

# Find rows in best that are NOT in A
key_a    <- df_a %>% transmute(key = paste(country, quarter))
key_best <- df_best %>% transmute(key = paste(country, quarter))

extra_in_best <- df_best %>%
  mutate(key = paste(country, quarter)) %>%
  filter(!(key %in% key_a$key)) %>%
  select(country, quarter, gdp, gdp_yoy_log_4q) %>%
  arrange(country, quarter)

missing_in_best <- df_a %>%
  mutate(key = paste(country, quarter)) %>%
  filter(!(key %in% key_best$key)) %>%
  select(country, quarter, gdp, gdp_yoy_log_4q) %>%
  arrange(country, quarter)

if (nrow(extra_in_best) > 0) {
  cat("Quarters in", best_label, "but NOT in variant A:\n")
  print(as.data.frame(extra_in_best), row.names = FALSE)
} else {
  cat("No extra quarters in", best_label, "vs A.\n")
}

if (nrow(missing_in_best) > 0) {
  cat("\nQuarters in A but NOT in", best_label, ":\n")
  print(as.data.frame(missing_in_best), row.names = FALSE)
}


# ═══════════════════════════════════════════════════════════
# 8) Per-country obs breakdown for each variant
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  PER-COUNTRY OBS BY VARIANT (only countries that differ)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

country_counts <- function(df, label) {
  df %>%
    group_by(country) %>%
    summarise(n = n(), .groups = "drop") %>%
    rename(!!label := n)
}

per_country <- country_counts(df_a, "A_current") %>%
  full_join(country_counts(df_b, "B_masterYoY"), by = "country") %>%
  full_join(country_counts(df_c, "C_hybrid"), by = "country") %>%
  full_join(country_counts(df_d, "D_noBRAfilter"), by = "country") %>%
  full_join(country_counts(df_e, "E_tableA1"), by = "country") %>%
  full_join(country_counts(df_f, "F_1961start"), by = "country") %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) %>%
  # Show only countries where variants differ
  filter(A_current != B_masterYoY | A_current != C_hybrid |
         A_current != E_tableA1 | A_current != D_noBRAfilter) %>%
  arrange(country)

print(as.data.frame(per_country), row.names = FALSE)


# ═══════════════════════════════════════════════════════════
# 9) Coverage of late-start countries in OECD raw data
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  OECD raw GDP availability for late-start countries\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

for (ctry in c("BRA", "ARG", "RUS", "CHL")) {
  ctry_data <- master %>%
    filter(country == ctry, !is.na(gdp)) %>%
    arrange(quarter)
  
  ctry_yoy <- master %>%
    filter(country == ctry, !is.na(gdp_yoy_log_4q)) %>%
    arrange(quarter)
  
  cat(ctry, ":\n")
  cat("  GDP level: ", min(ctry_data$quarter), "to", max(ctry_data$quarter),
      " (", nrow(ctry_data), "quarters)\n")
  cat("  Master YoY:", min(ctry_yoy$quarter), "to", max(ctry_yoy$quarter),
      " (", nrow(ctry_yoy), "quarters)\n")
  
  # Show the first few quarters
  cat("  First 8 GDP quarters:\n")
  print(head(ctry_data %>% select(country, quarter, gdp), 8))
  cat("\n")
}


# ═══════════════════════════════════════════════════════════
# 10) Summary statistics for the best-matching variant
#     (Table 1 comparison)
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  TABLE 1 SUMMARY STATISTICS — ALL VARIANTS\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

make_summary <- function(df, label) {
  winner_by_country <- df %>%
    group_by(country) %>%
    summarise(is_winner = any(rank1 == 1, na.rm = TRUE), .groups = "drop")
  
  df2 <- df %>%
    left_join(winner_by_country, by = "country") %>%
    mutate(
      group = if_else(is_winner, "Winner", "Non-winner"),
      year  = as.integer(str_extract(quarter, "\\d{4}")),
      period = case_when(
        year >= 1960 & year <= 1980 ~ "1960-80",
        year > 1980  & year <= 2000 ~ "1980-2000",
        year > 2000  & year <= 2020 ~ "2000-20",
        TRUE ~ NA_character_
      )
    )
  
  period_levels <- c("1960-80", "1980-2000", "2000-20", "Full sample")
  
  make_block <- function(d, period_name) {
    w <- d %>% filter(group == "Winner")
    n <- d %>% filter(group == "Non-winner")
    
    tibble(
      Period     = period_name,
      W_mean_yoy = sprintf("%.2f", mean(w$gdp_yoy_log_4q, na.rm = TRUE)),
      W_sd_yoy   = sprintf("%.2f", sd(w$gdp_yoy_log_4q, na.rm = TRUE)),
      W_obs      = nrow(w),
      W_cty      = n_distinct(w$country),
      NW_mean_yoy = sprintf("%.2f", mean(n$gdp_yoy_log_4q, na.rm = TRUE)),
      NW_sd_yoy   = sprintf("%.2f", sd(n$gdp_yoy_log_4q, na.rm = TRUE)),
      NW_obs      = nrow(n),
      NW_cty      = n_distinct(n$country)
    )
  }
  
  summary_df <- bind_rows(
    make_block(df2 %>% filter(period == "1960-80"), "1960-80"),
    make_block(df2 %>% filter(period == "1980-2000"), "1980-2000"),
    make_block(df2 %>% filter(period == "2000-20"), "2000-20"),
    make_block(df2, "Full sample")
  ) %>%
    mutate(Variant = label, .before = Period)
  
  summary_df
}

all_summaries <- bind_rows(
  make_summary(df_a, "A_current"),
  make_summary(df_b, "B_masterYoY"),
  make_summary(df_c, "C_hybrid"),
  make_summary(df_d, "D_noBRAfilter"),
  make_summary(df_e, "E_tableA1"),
  make_summary(df_f, "F_1961start")
)

# Print per period for easy comparison
for (p in c("1960-80", "1980-2000", "2000-20", "Full sample")) {
  cat("--- Period:", p, "---\n")
  block <- all_summaries %>% filter(Period == p)
  print(as.data.frame(block), row.names = FALSE)
  cat("\n")
}


# ═══════════════════════════════════════════════════════════
# 11) Paper Table 1 reference values (Mello 2024, OBES, p.8)
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  PAPER TABLE 1 REFERENCE (Mello 2024 OBES, published version)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
cat("                              Winner                      Non-winner\n")
cat("                          Mean      SD               Mean        SD       t-test\n")
cat("1960-80\n")
cat("  GDP (thousands)       1,098.60  (417.39)          498.43  (1,163.13)   9.91***\n")
cat("  Population (m)           54.43   (13.91)           26.15     (46.02)  11.85***\n")
cat("  GDP per capita       19,697.70 (4,049.61)      19,708.23  (9,015.79)  -0.02\n")
cat("  YoY GDP growth          3.96     (2.83)            4.54      (3.42)   -3.08***\n")
cat("1980-2000\n")
cat("  GDP (thousands)       1,802.83  (592.64)          852.00  (1,958.94)   9.99***\n")
cat("  Population (m)           60.93   (22.04)           40.59     (99.01)   4.24***\n")
cat("  GDP per capita       29,873.18 (5,652.38)      27,323.93 (14,022.82)   3.72***\n")
cat("  YoY GDP growth          2.25     (1.82)            3.14      (3.54)   -5.11***\n")
cat("2000-20\n")
cat("  GDP (thousands)       2,563.41  (659.91)        1,211.01  (2,750.20)  10.99***\n")
cat("  Population (m)           84.61   (50.41)           65.80    (194.02)   2.17**\n")
cat("  GDP per capita       35,574.93 (10,442.53)     33,633.54 (19,068.19)   2.24**\n")
cat("  YoY GDP growth          1.05     (3.48)            2.55      (3.91)   -8.17***\n")
cat("Full sample\n")
cat("  GDP (thousands)       1,908.88  (843.57)          958.65  (2,302.79)  14.68***\n")
cat("  Population (m)           68.51   (37.53)           49.65    (148.58)   4.54***\n")
cat("  GDP per capita       29,258.50 (10,061.40)     28,887.99 (16,921.76)   0.76\n")
cat("  YoY GDP growth          2.33     (3.24)            3.22      (3.87)   -7.81***\n")
cat("  # countries                6                        42\n")
cat("  # observations          1,295                     7,342\n")
cat("  Total:                 8,637\n")
cat("\n  (GDP in thousands of 2015 US dollar millions;\n")
cat("   YoY GDP growth = Δ4 log × 100)\n")


cat("\n\nDONE.\n")

library(readr)
library(dplyr)
library(stringr)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# --- load (6-feature panel: gdp, private_consumption, government_consumption,
#          capital_formation, exports, imports)
gdp <- read_csv("Data/oecd_processed/oecd_usd_ppp_real_paper_features_wide.csv", show_col_types = FALSE)
pop <- read_csv("Data/oecd_source/oecd_population_annual_1960_2024.csv", show_col_types = FALSE)

# --- detect GDP time column (usually "time" or "time_period")
gdp_time_col <- intersect(names(gdp), c("quarter", "time"))[1]
stopifnot(!is.na(gdp_time_col))

# --- identify GDP value column (first non-id column)
gdp_value_col <- setdiff(names(gdp), c("country", gdp_time_col))[1]
stopifnot(!is.na(gdp_value_col))

# --- GDP coverage by country (quarterly)
gdp_cov <- gdp %>%
  filter(!is.na(.data[[gdp_value_col]])) %>%
  mutate(year = as.integer(str_sub(.data[[gdp_time_col]], 1, 4))) %>%
  group_by(country) %>%
  summarise(
    gdp_start_q = min(.data[[gdp_time_col]]),
    gdp_end_q   = max(.data[[gdp_time_col]]),
    gdp_start_y = min(year, na.rm = TRUE),
    gdp_end_y   = max(year, na.rm = TRUE),
    gdp_n_q     = n(),
    .groups = "drop"
  ) %>%
  rename(country_code = country)

# --- POP coverage by country (annual)
pop_cov <- pop %>%
  filter(!is.na(population_million)) %>%
  group_by(country_code) %>%
  summarise(
    pop_start_y = min(year, na.rm = TRUE),
    pop_end_y   = max(year, na.rm = TRUE),
    pop_n_y     = n(),
    .groups = "drop"
  )

# --- (1) country set comparison
only_in_gdp <- anti_join(gdp_cov, pop_cov, by = "country_code") %>%
  arrange(country_code)

only_in_pop <- anti_join(pop_cov, gdp_cov, by = "country_code") %>%
  arrange(country_code)

cat("Countries in GDP but not in POP:", nrow(only_in_gdp), "\n")
cat("Countries in POP but not in GDP:", nrow(only_in_pop), "\n\n")

if (nrow(only_in_gdp) > 0) print(only_in_gdp %>% select(country_code), n = Inf)
if (nrow(only_in_pop) > 0) print(only_in_pop %>% select(country_code), n = Inf)

# --- (2) coverage overlap check for matched countries
cov_compare <- inner_join(gdp_cov, pop_cov, by = "country_code") %>%
  mutate(
    overlap_start_y = pmax(gdp_start_y, pop_start_y),
    overlap_end_y   = pmin(gdp_end_y, pop_end_y),
    overlap_years   = pmax(0L, overlap_end_y - overlap_start_y + 1L),
    pop_covers_gdp  = pop_start_y <= gdp_start_y & pop_end_y >= gdp_end_y,
    gdp_covers_pop  = gdp_start_y <= pop_start_y & gdp_end_y >= pop_end_y
  ) %>%
  arrange(country_code)

pop_gap_vs_gdp <- cov_compare %>%
  filter(!pop_covers_gdp) %>%
  select(country_code, gdp_start_y, gdp_end_y, pop_start_y, pop_end_y, overlap_years) %>%
  arrange(country_code)

cat("\nMatched countries:", nrow(cov_compare), "\n")
cat("Countries where POP does not fully cover GDP (year-wise):", nrow(pop_gap_vs_gdp), "\n\n")

if (nrow(pop_gap_vs_gdp) > 0) print(pop_gap_vs_gdp, n = Inf)



########### JOIN the data #################

gdp <- read_csv("Data/oecd_processed/oecd_usd_ppp_real_paper_features_wide.csv", show_col_types = FALSE)
pop <- read_csv("Data/oecd_source/oecd_population_annual_1960_2024.csv", show_col_types = FALSE)

# keep only real countries (drop group codes; keep exactly 3-letter ISO3)
gdp2 <- gdp %>% filter(str_detect(country, "^[A-Z]{3}$"))
pop2 <- pop %>% filter(str_detect(country_code, "^[A-Z]{3}$"))

# extract year from quarter (e.g. "2006-Q1" -> 2006)
gdp2 <- gdp2 %>%
  mutate(year = as.integer(str_extract(quarter, "\\d{4}")))

# join and duplicate pop across all quarters of the year
gdp_pop <- gdp2 %>%
  left_join(pop2 %>% select(country_code, year, population_million),
            by = c("country" = "country_code", "year" = "year")) %>%
  rename(population = population_million) %>%
  select(-year)

# quick check: how many GDP rows still missing population?
cat("GDP rows with missing population:", sum(is.na(gdp_pop$population)), "\n")

write_csv(gdp_pop, "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop.csv")



#################
# Join WC to gdp_pop
################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# -------------------------------------------------
# 1) Load datasets
# -------------------------------------------------
gdp_pop <- read_csv(
  "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop.csv",
  show_col_types = FALSE
)

wc <- read_csv(
  "Data/world_cup/world_cup_results_wide_entities.csv",
  show_col_types = FALSE
)

# -------------------------------------------------
# 2) WC: keep only countries, reshape to (country, year) indicators
# -------------------------------------------------
wc_yearly <- wc %>%
  filter(entity_type == "country") %>%
  select(entity_code, starts_with("WC_")) %>%
  pivot_longer(
    cols = starts_with("WC_"),
    names_to = "wc_col",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(wc_col, "\\d{4}")),
    var  = str_remove(wc_col, "^WC_\\d{4}_")
  ) %>%
  select(entity_code, year, var, value) %>%
  pivot_wider(
    names_from = var,
    values_from = value,
    values_fill = 0
  )

# -------------------------------------------------
# 3) GDP: extract year and quarter number
# -------------------------------------------------
gdp_pop2 <- gdp_pop %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    qtr  = as.integer(str_extract(quarter, "(?<=-Q)\\d"))
  )

# -------------------------------------------------
# 4) Define event quarter: Q2 for all years except 2022 -> Q4
# -------------------------------------------------
gdp_pop2 <- gdp_pop2 %>%
  mutate(event_qtr = if_else(year == 2022L, 4L, 2L))

# -------------------------------------------------
# 5) Join WC by (country, year), then keep indicators only in event quarter
# -------------------------------------------------
gdp_pop_wc <- gdp_pop2 %>%
  left_join(wc_yearly, by = c("country" = "entity_code", "year" = "year")) %>%
  mutate(across(
    c(host, rank1, rank2, rank3, rank4, host_won, host_top4),
    ~ replace_na(.x, 0L)
  )) %>%
  mutate(across(
    c(host, rank1, rank2, rank3, rank4, host_won, host_top4),
    ~ if_else(qtr == event_qtr, .x, 0L)
  )) %>%
  select(-year, -qtr, -event_qtr)

# -------------------------------------------------
# 6) save raw
# -------------------------------------------------
write_csv(gdp_pop_wc, "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq.csv")



# -------------------------------------------------
# 7) ADD CALCULATION METRICS (YoY logs + YoY percent)
#     - No filtering for country or timeframe
#     - Compute YoY for all numeric non-binary indicators (same logic as your 2nd script)
# -------------------------------------------------

safe_log <- function(x) {
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & x > 0)
  out[idx] <- log(x[idx])
  out
}

is_binary01 <- function(v) {
  u <- unique(v[!is.na(v)])
  length(u) > 0 && all(u %in% c(0, 1))
}

# add year/qtr/q_index once (useful later; not a filter)
gdp_pop_wc2 <- gdp_pop_wc %>%
  mutate(
    year = as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    q_index = year * 4L + qtr
  ) %>%
  arrange(country, q_index)

# choose numeric columns that are not IDs and not binary dummies (0/1)
id_cols <- c("country", "quarter", "year", "qtr", "q_index")
num_cols <- names(gdp_pop_wc2)[sapply(gdp_pop_wc2, is.numeric)]
num_cols <- setdiff(num_cols, id_cols)

econ_cols <- num_cols[!sapply(gdp_pop_wc2[num_cols], is_binary01)]

cat("Will compute YoY for", length(econ_cols), "numeric (non-binary) columns.\n")

gdp_pop_wc2 <- gdp_pop_wc2 %>%
  group_by(country) %>%
  arrange(q_index, .by_group = TRUE) %>%
  mutate(
    across(
      all_of(econ_cols),
      ~ (safe_log(.) - lag(safe_log(.), 4)) * 100,
      .names = "{.col}_yoy_log_4q"
    ),
    across(
      all_of(econ_cols),
      ~ if_else(!is.na(lag(., 4)) & lag(., 4) != 0,
                (. / lag(., 4) - 1) * 100,
                NA_real_),
      .names = "{.col}_yoy_pct"
    )
  ) %>%
  ungroup()

# Save enriched master panel (NO trimming yet)
write_csv(
  gdp_pop_wc2,
  "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv"
)

cat("Saved enriched panel with YoY columns.\n")



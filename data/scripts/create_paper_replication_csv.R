############################################################
# Paper replication dataset (Table 1 sample)
# - Keep only countries listed in Table A2
# - Trim to 1961-Q1 ... 2021-Q4
# - Apply paper special case: Brazil starts at 1998-Q2
# - Use MASTER-panel YoY (computed on all available OECD data)
#   so that late-start countries (BRA, ARG, RUS, CHL) keep
#   their first 4 quarters of valid YoY growth.
# - For countries whose OECD data starts in 1960, the paper's
#   effective first YoY quarter is 1962-Q1 (lag needs 1961-Q1,
#   the earliest in-window quarter).
# - See investigate_missing_8_obs.R (archived) for background.
############################################################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# -------------------------
# 1) Load enriched GDP+POP+WC+YoY (NO pruning upstream)
# -------------------------
gdp_pop <- read_csv(
  "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
  show_col_types = FALSE
)

stopifnot("country" %in% names(gdp_pop), "quarter" %in% names(gdp_pop))

# -------------------------
# 2) Paper country set (Table A2)
# -------------------------
controls <- c(
  "ARG","AUS","AUT","BEL","BGR","CAN","CHL","COL","CRI","HRV","CZE","DNK","EST","FIN",
  "GRC","HUN","IND","IDN","ISL","IRL","ISR","LVA","LTU","LUX","NLD","NZL","NOR","POL",
  "PRT","ROU","SAU","SVK","SVN","SWE","CHE","TUR"
)

hosts <- c(
  "BRA","GBR","FRA","DEU","ITA","JPN","MEX","ZAF","KOR","ESP","RUS","USA"
)

paper_countries <- sort(unique(c(controls, hosts)))

# -------------------------
# 3) Trim to paper window
#    Keep all data from 1961-Q1, but do NOT recompute YoY.
#    The master panel's YoY (computed on all available OECD data)
#    is needed so late-start countries get valid YoY in their
#    first 4 quarters (their OECD data predates the paper window).
# -------------------------
paper_replication_df <- gdp_pop %>%
  filter(
    country %in% paper_countries,
    quarter >= "1961-Q1",
    quarter <= "2021-Q4"
  ) %>%
  # Paper special case: Brazil starts at 1998-Q2
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  mutate(
    year = if ("year" %in% names(.)) year else as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = if ("qtr" %in% names(.)) qtr  else as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    q_index = if ("q_index" %in% names(.)) q_index else year * 4L + qtr
  ) %>%
  arrange(country, q_index)

# Quick checks
cat("Countries in replication df:", n_distinct(paper_replication_df$country), "\n")
cat("Quarter range:", min(paper_replication_df$quarter), "to", max(paper_replication_df$quarter), "\n")

missing_countries <- setdiff(paper_countries, unique(paper_replication_df$country))
if (length(missing_countries) > 0) {
  cat("Countries expected but not present in your GDP panel:\n")
  print(missing_countries)
}

# -------------------------
# 4) Save the trimmed replication panel (already contains YoY cols)
# -------------------------
write_csv(paper_replication_df, "Data/mello_paper_replication/paper_replication_dataset_q_1961_2021.csv")

# ============================================================
# 5) PAPER REPLICATION SAMPLE (Table 1, OBES 2024)
#    Apply per-country effective start dates from Table A1:
#    - Most countries (OECD data from 1960): effective YoY start
#      is 1962-Q1 (4-quarter lag back to 1961-Q1, the first
#      in-window quarter).
#    - Late-start countries: use Table A1 start dates.
#      Their master YoY is valid because OECD data predates
#      their stated start (e.g. BRA has GDP from 1996-Q1,
#      so YoY at 1998-Q2 uses the 1997-Q2 lag).
#    - Then filter for non-NA YoY.
# ============================================================

df <- paper_replication_df

# Pre-compute 4th lags of log(feature_level) from the FULL 1960+ data.
# This ensures 1962-Q1 has a valid 4th lag (= log value at 1961-Q1),
# which would otherwise be NA if computed only within the 1962+ sample.
# Mello's Eq. (1) includes ln(y_{c,t-4}) as a convergence control.
feature_levels <- c("gdp", "private_consumption", "government_consumption",
                    "capital_formation", "exports", "imports")

ln_lag_all <- gdp_pop %>%
  filter(country %in% paper_countries) %>%
  mutate(
    year = if ("year" %in% names(.)) year else as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = if ("qtr" %in% names(.)) qtr  else as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    tq   = year * 4L + qtr
  ) %>%
  group_by(country) %>%
  arrange(tq, .by_group = TRUE) %>%
  mutate(across(all_of(feature_levels),
                ~ dplyr::lag(log(.x), 4),
                .names = "ln_{.col}_l4")) %>%
  ungroup() %>%
  select(country, tq, starts_with("ln_") & ends_with("_l4"))

# Table A1 start dates for late-start countries
paper_starts <- tibble::tribble(
  ~country, ~paper_start,
  "BRA",    "1998-Q2",
  "ARG",    "1993-Q1",
  "RUS",    "1995-Q1",
  "CHL",    "1996-Q1"
)

df_es <- df %>%
  left_join(paper_starts, by = "country") %>%
  mutate(
    effective_start = coalesce(paper_start, "1962-Q1")
  ) %>%
  filter(quarter >= effective_start) %>%
  select(-paper_start, -effective_start) %>%
  mutate(
    tq = year * 4L + qtr,
    winner = as.integer(rank1 == 1)
  ) %>%
  # Join pre-computed lags for all 6 features (from 1960+ data)
  left_join(ln_lag_all, by = c("country", "tq")) %>%
  arrange(country, tq) %>%
  filter(!is.na(gdp_yoy_log_4q))

write.csv(df_es, "Data/mello_paper_replication/paper_replication_sample.csv", row.names = FALSE)

# quick check
df_es %>%
  summarise(
    mean_gdp = mean(gdp, na.rm = TRUE),
    mean_yoy = mean(gdp_yoy_log_4q, na.rm = TRUE)
  )

# ============================================================
# 6) STACKED EVENT-STUDY DATASET (L=16)
#    Stack [-16,+16] around each win event (rank1==1 in event quarter)
#    Keep ALL countries in those calendar quarters.
# ============================================================

# identify win events (these should be in the event quarter because your WC join zeroes others)
events <- df %>%
  filter(rank1 == 1) %>%
  distinct(country, quarter, year, qtr, q_index) %>%
  arrange(country, q_index) %>%
  mutate(event_id = row_number()) %>%
  select(event_id, event_country = country, event_quarter = quarter, event_q_index = q_index)

L <- 16L

event_calendar <- events %>%
  tidyr::expand_grid(l = -L:L) %>%
  mutate(q_index = event_q_index + l) %>%
  select(event_id, event_country, event_quarter, event_q_index, l, q_index)

event_study_df <- event_calendar %>%
  left_join(
    df,
    by = "q_index"
  ) %>%
  filter(!is.na(country)) %>%
  mutate(
    treated = as.integer(country == event_country),
    win_l   = as.integer(treated == 1)
  )

# optional: wide dummies WIN_m16 ... WIN_p16
event_study_df <- event_study_df %>%
  mutate(l_label = if_else(l < 0, paste0("m", abs(l)), paste0("p", l))) %>%
  pivot_wider(
    id_cols = c(event_id, event_country, event_quarter, event_q_index, q_index, country, quarter),
    names_from = l_label,
    values_from = win_l,
    values_fill = 0,
    names_prefix = "WIN_"
  )

cat("Events (wins):", nrow(events), "\n")
cat("Stacked rows:", nrow(event_study_df), "\n")

write_csv(event_study_df, "Data/mello_paper_replication/event_study_dataset_stacked_L16.csv")


################################################################################
# check coverage
################################################################################

library(dplyr)
library(stringr)
library(readr)

df <- read_csv("Data/mello_paper_replication/paper_replication_sample.csv", show_col_types = FALSE)

coverage <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = as.integer(str_extract(quarter, "(?<=-Q)\\d"))
  ) %>%
  group_by(country) %>%
  summarise(
    start_quarter = min(quarter),
    end_quarter   = max(quarter),
    start_year    = min(year),
    end_year      = max(year),
    n_quarters    = n(),
    n_years       = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(country)

print(coverage, n = Inf)

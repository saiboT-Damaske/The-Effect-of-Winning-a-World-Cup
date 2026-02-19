############################################################
# Paper replication dataset (Table 1 sample)
# - Keep only countries listed in Table A2
# - Trim to 1961-Q1 ... 2021-Q4
# - Apply paper special case: Brazil starts at 1998-Q2
# - DO NOT recompute YoY (already in enriched master panel)
############################################################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Set working directory to data folder (parent of scripts)
setwd("..")

# -------------------------
# 1) Load enriched GDP+POP+WC+YoY (NO pruning upstream)
# -------------------------
gdp_pop <- read_csv(
  "oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
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
# 3) Trim to paper window and apply special cases
#     (No YoY recomputation here)
# -------------------------
paper_replication_df <- gdp_pop %>%
  filter(
    country %in% paper_countries,
    quarter >= "1961-Q1",
    quarter <= "2021-Q4"
  ) %>%
  # Paper special case: Brazil starts at 1998-Q2
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  # ensure we have year/qtr/q_index (they should already exist; recompute only if missing)
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
write_csv(paper_replication_df, "mello_paper_replication/paper_replication_dataset_q_1961_2021.csv")

# ============================================================
# 5) EVENT-STUDY SAMPLE FILTER FOR TABLE 1 (OBES 2024):
#    keep only quarters where YoY GDP growth is defined (needs t-4)
#    (YoY already exists; just filter)
# ============================================================

df <- paper_replication_df

# define tq like your earlier code (optional, but convenient)
df_es <- df %>%
  mutate(
    tq = year * 4L + qtr,
    winner = as.integer(rank1 == 1)  # winner indicator for Table 1 summaries
  ) %>%
  arrange(country, tq) %>%
  filter(!is.na(gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_log_4q))

write.csv(df_es, "mello_paper_replication/paper_replication_event_study_sample.csv", row.names = FALSE)

# quick check
df_es %>%
  summarise(
    mean_gdp = mean(gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted, na.rm = TRUE),
    mean_yoy = mean(gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_log_4q, na.rm = TRUE)
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

write_csv(event_study_df, "mello_paper_replication/event_study_dataset_stacked_L16.csv")


################################################################################
# check coverage
################################################################################

library(dplyr)
library(stringr)
library(readr)

df <- read_csv("mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)

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

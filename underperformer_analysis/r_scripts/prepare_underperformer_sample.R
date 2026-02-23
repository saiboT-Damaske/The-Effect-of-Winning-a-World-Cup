# ============================================================
# Prepare Underperformer Analysis Sample
#
# Definition: A country that was ranked in the top 10 of the
#   pre-tournament ELO rating but was eliminated in the group stage.
#   Source: Data/world_cup/wc_underperformance_elo.csv
#
# This script:
#   1) Loads the ELO underperformance data
#   2) Loads the OECD panel (paper_replication_sample.csv)
#   3) Identifies which underperformer events have panel data
#   4) Adds an `underperformer` column to the panel (1 at Q2 of WC year)
#   5) Saves the augmented panel + a diagnostic summary
#
# Input:  Data/world_cup/wc_underperformance_elo.csv
#         Data/mello_paper_replication/paper_replication_sample.csv
# Output: underperformer_analysis/results/underperformer_sample.csv
#         underperformer_analysis/results/underperformer_events_summary.txt
# ============================================================

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ---------- 1) Load ELO underperformance data ----------
elo <- read_csv("Data/world_cup/wc_underperformance_elo.csv", show_col_types = FALSE)

cat("ELO data loaded:", nrow(elo), "rows\n")
cat("Years covered:", paste(sort(unique(elo$wc_year)), collapse = ", "), "\n")

# Filter to underperformers only
underperf_all <- elo %>%
  filter(underperformed == 1) %>%
  select(wc_year, elo_rank, iso3, team_name, elo_rating, group_stage_exit) %>%
  arrange(wc_year, elo_rank)

cat("\nAll underperformer events (top-10 ELO, group stage exit):\n")
print(as.data.frame(underperf_all))
cat("\nTotal underperformer events:", nrow(underperf_all), "\n")

# ---------- 2) Load OECD panel ----------
panel <- read_csv(
  "Data/mello_paper_replication/paper_replication_sample.csv",
  show_col_types = FALSE
)

panel_countries <- sort(unique(panel$country))
cat("\nOECD panel:", nrow(panel), "rows,", length(panel_countries), "countries\n")

# ---------- 3) Match underperformer events to OECD panel ----------
# Check which iso3 codes from the ELO data exist in the panel
underperf_matched <- underperf_all %>%
  mutate(in_panel = iso3 %in% panel_countries) %>%
  arrange(wc_year, elo_rank)

cat("\n--- Matching underperformer events to OECD panel ---\n")
cat("Events in panel:\n")
print(as.data.frame(underperf_matched %>% filter(in_panel)))
cat("\nEvents NOT in panel:\n")
print(as.data.frame(underperf_matched %>% filter(!in_panel)))

# Keep only events where the country is in our panel
underperf_events <- underperf_matched %>%
  filter(in_panel) %>%
  select(wc_year, iso3, team_name, elo_rank, elo_rating)

cat("\n--- Final underperformer events for analysis ---\n")
cat("Events:", nrow(underperf_events), "\n")
cat("Unique countries:", n_distinct(underperf_events$iso3), "\n")
cat("Countries:", paste(sort(unique(underperf_events$iso3)), collapse = ", "), "\n")
cat("WC years:", paste(sort(unique(underperf_events$wc_year)), collapse = ", "), "\n")

# ---------- 4) Add underperformer column to panel ----------
# The treatment fires at Q2 of the WC year (same convention as rank1/rank2)
# underperformer = 1 at the event quarter, 0 otherwise

# Create lookup: for each (iso3, wc_year) pair
up_lookup <- underperf_events %>%
  select(iso3, wc_year) %>%
  mutate(is_underperf_event = 1L)

# Mark panel rows: underperformer = 1 if country underperformed at that WC year's Q2
panel_aug <- panel %>%
  left_join(
    up_lookup,
    by = c("country" = "iso3", "year" = "wc_year")
  ) %>%
  mutate(
    # Only mark at Q2 (the event quarter convention)
    underperformer = if_else(
      !is.na(is_underperf_event) & qtr == 2L,
      1L, 0L
    )
  ) %>%
  select(-is_underperf_event)

# Also add a time-invariant flag: ever_underperformer
ever_up_countries <- unique(underperf_events$iso3)
panel_aug <- panel_aug %>%
  mutate(
    ever_underperformer = as.integer(country %in% ever_up_countries)
  )

cat("\n--- Panel augmented ---\n")
cat("Rows:", nrow(panel_aug), "\n")
cat("underperformer == 1 rows:", sum(panel_aug$underperformer == 1), "\n")
cat("ever_underperformer countries:", sum(unique(panel_aug$country) %in% ever_up_countries), "\n")

# Verify: list the underperformer == 1 rows
cat("\nVerification — rows with underperformer = 1:\n")
print(
  as.data.frame(
    panel_aug %>%
      filter(underperformer == 1) %>%
      select(country, year, qtr, quarter, underperformer, gdp_yoy_log_4q) %>%
      arrange(country, year)
  )
)

# ---------- 5) Check data availability per event ----------
# For SDiD we need gdp_yoy_log_4q (and component features) in the window [-7, +2]
# around Q2 of WC year.  Check which events have complete GDP data.

feature_cols <- c(
  "gdp_yoy_log_4q",
  "private_consumption_yoy_log_4q",
  "government_consumption_yoy_log_4q",
  "capital_formation_yoy_log_4q",
  "exports_yoy_log_4q",
  "imports_yoy_log_4q"
)

availability <- underperf_events %>%
  rowwise() %>%
  mutate(
    event_tq = 4L * wc_year + (2L - 1L),
    tq_min   = event_tq - 7L,
    tq_max   = event_tq + 2L,
    country_rows = {
      sub <- panel_aug %>%
        filter(country == iso3, tq >= tq_min, tq <= tq_max)
      nrow(sub)
    },
    gdp_available = {
      sub <- panel_aug %>%
        filter(country == iso3, tq >= tq_min, tq <= tq_max)
      sum(!is.na(sub$gdp_yoy_log_4q))
    }
  ) %>%
  ungroup() %>%
  mutate(
    complete_window = (gdp_available == 10L)
  )

cat("\n--- Data availability per underperformer event ---\n")
print(as.data.frame(availability %>% select(wc_year, iso3, team_name, elo_rank,
                                             country_rows, gdp_available, complete_window)))

cat("\nEvents with complete GDP window (10 quarters):",
    sum(availability$complete_window), "of", nrow(availability), "\n")

# Check per-feature availability for all events
cat("\n--- Feature availability across all matched events ---\n")
for (fc in feature_cols) {
  n_complete <- underperf_events %>%
    rowwise() %>%
    mutate(
      event_tq = 4L * wc_year + (2L - 1L),
      n_ok = {
        sub <- panel_aug %>%
          filter(country == iso3,
                 tq >= (event_tq - 7L),
                 tq <= (event_tq + 2L))
        sum(!is.na(sub[[fc]]))
      },
      complete = (n_ok == 10L)
    ) %>%
    ungroup() %>%
    summarise(n_complete = sum(complete)) %>%
    pull(n_complete)

  cat(sprintf("  %-45s: %d / %d events with complete window\n",
              fc, n_complete, nrow(underperf_events)))
}

# ---------- 6) Save outputs ----------

# Save augmented panel
write_csv(panel_aug, "underperformer_analysis/results/underperformer_sample.csv")
cat("\nSaved: underperformer_analysis/results/underperformer_sample.csv\n")

# Save diagnostic summary
sink("underperformer_analysis/results/underperformer_events_summary.txt")

cat("========================================\n")
cat("  UNDERPERFORMER ANALYSIS — DATA SUMMARY\n")
cat("========================================\n\n")

cat("Definition: Top-10 pre-tournament ELO rating, eliminated in group stage.\n")
cat("Source: Data/world_cup/wc_underperformance_elo.csv\n\n")

cat("--- All underperformer events (regardless of OECD panel membership) ---\n")
print(as.data.frame(underperf_all))

cat("\n--- Events matched to OECD panel ---\n")
cat("Total matched events:", nrow(underperf_events), "\n")
cat("Unique countries:", n_distinct(underperf_events$iso3), "\n")
cat("Countries:", paste(sort(unique(underperf_events$iso3)), collapse = ", "), "\n")
cat("WC years with events:", paste(sort(unique(underperf_events$wc_year)), collapse = ", "), "\n\n")

cat("--- Events by country ---\n")
by_country <- underperf_events %>%
  group_by(iso3) %>%
  summarise(n_events = n(), wc_years = paste(wc_year, collapse = ", "), .groups = "drop") %>%
  arrange(desc(n_events), iso3)
print(as.data.frame(by_country))

cat("\n--- Events by WC year ---\n")
by_year <- underperf_events %>%
  group_by(wc_year) %>%
  summarise(n_events = n(), countries = paste(iso3, collapse = ", "), .groups = "drop") %>%
  arrange(wc_year)
print(as.data.frame(by_year))

cat("\n--- Events NOT in OECD panel ---\n")
not_in <- underperf_matched %>% filter(!in_panel) %>% select(wc_year, iso3, team_name, elo_rank)
if (nrow(not_in) > 0) {
  print(as.data.frame(not_in))
} else {
  cat("None — all events matched.\n")
}

cat("\n--- Data availability (10-quarter window for SDiD) ---\n")
print(as.data.frame(availability %>% select(wc_year, iso3, team_name, elo_rank,
                                             gdp_available, complete_window)))

cat("\n--- Feature-level availability ---\n")
for (fc in feature_cols) {
  n_complete <- underperf_events %>%
    rowwise() %>%
    mutate(
      event_tq = 4L * wc_year + (2L - 1L),
      n_ok = {
        sub <- panel_aug %>%
          filter(country == iso3,
                 tq >= (event_tq - 7L),
                 tq <= (event_tq + 2L))
        sum(!is.na(sub[[fc]]))
      },
      complete = (n_ok == 10L)
    ) %>%
    ungroup() %>%
    summarise(n_complete = sum(complete)) %>%
    pull(n_complete)

  cat(sprintf("  %-45s: %d / %d events with complete window\n",
              fc, n_complete, nrow(underperf_events)))
}

cat("\n--- Panel summary ---\n")
cat("Total panel rows:", nrow(panel_aug), "\n")
cat("underperformer == 1 rows:", sum(panel_aug$underperformer == 1), "\n")
cat("ever_underperformer == 1 countries:", length(ever_up_countries), "\n")
cat("Ever-underperformer country list:", paste(sort(ever_up_countries), collapse = ", "), "\n")

cat("\n========================================\n")
cat("  END OF SUMMARY\n")
cat("========================================\n")

sink()
cat("Saved: underperformer_analysis/results/underperformer_events_summary.txt\n")

cat("\nDone.\n")

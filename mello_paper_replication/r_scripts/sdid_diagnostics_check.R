# SDiD Diagnostics Check — Compare sample sizes and event years to Mello (OBES)
# This script checks the number of treated/control units and event years for each feature
# It does NOT run SDiD estimation, just diagnostics

library(readr)
library(dplyr)
library(tidyr)

# Load data
file_path <- "Data/mello_paper_replication/paper_replication_sample.csv"
df <- read_csv(file_path, show_col_types = FALSE) %>%
  mutate(
    country = as.character(country),
    quarter = as.character(quarter),
    year    = as.integer(year),
    qtr     = as.integer(qtr),
    host    = as.integer(host)
  ) %>%
  arrange(country, year, qtr) %>%
  mutate(
    tq_num = 4L * year + (qtr - 1L)
  )

# World Cup years (as in Mello)
wc_years <- c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L)
events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)
)

is_treated_subseries <- function(country, wc_year) {
  (country == "FRA" && wc_year %in% c(1998L, 2018L)) ||
    (country == "BRA" && wc_year == 2002L) ||
    (country == "ITA" && wc_year == 2006L) ||
    (country == "ESP" && wc_year == 2010L) ||
    (country == "DEU" && wc_year == 2014L)
}

features <- c(
  "gdp_yoy_pct", "private_consumption_yoy_pct", "government_consumption_yoy_pct",
  "capital_formation_yoy_pct", "exports_yoy_pct", "imports_yoy_pct",
  "gdp_yoy_log_4q", "private_consumption_yoy_log_4q", "government_consumption_yoy_log_4q",
  "capital_formation_yoy_log_4q", "exports_yoy_log_4q", "imports_yoy_log_4q"
)

for (feature in features) {
  cat("\n==============================\n")
  cat("Feature:", feature, "\n")
  if (!(feature %in% names(df))) {
    cat("Column not found in data.\n")
    next
  }
  d <- df %>%
    select(country, host, tq_num, !!feature) %>%
    rename(y = !!feature) %>%
    tidyr::crossing(events) %>%
    mutate(
      rel_time = as.integer(tq_num - event_tq),
      unit_id  = paste(country, wc_year, sep = "_"),
      treated  = as.integer(mapply(is_treated_subseries, country, wc_year)),
      post     = as.integer(rel_time >= 1L),
      D        = as.integer(treated * post)
    ) %>%
    filter(rel_time >= -7L, rel_time <= 2L)
  # Drop host-only controls
  d <- d %>%
    group_by(unit_id) %>%
    mutate(host_subseries = max(host, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!(host_subseries == 1L & treated == 0L)) %>%
    select(-host_subseries)
  # Balanced panel and no missing outcomes
  d <- d %>%
    group_by(unit_id) %>%
    filter(
      n_distinct(rel_time) == 10L,
      all(!is.na(y))
    ) %>%
    ungroup()
  cat("Units:", n_distinct(d$unit_id), "\n")
  cat("Treated units:", d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow(), "\n")
  cat("Control units:", d %>% distinct(unit_id, treated) %>% filter(treated == 0L) %>% nrow(), "\n")
  cat("rel_time range:", paste(range(d$rel_time), collapse = " to "), "\n")
  cat("Countries in treated units:", paste(unique(d %>% filter(treated == 1L) %>% pull(country)), collapse = ", "), "\n")
  cat("Countries in control units:", paste(unique(d %>% filter(treated == 0L) %>% pull(country)), collapse = ", "), "\n")
}
cat("\nDiagnostics check complete.\n")

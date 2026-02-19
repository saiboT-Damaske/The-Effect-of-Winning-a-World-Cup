############################################################
# OECD QNA (Expenditure approach, national currency)
# Clean pipeline to:
#  (1) Download + clean
#  (2) Filter quarterly SA(Y)
#  (3) Build LONG base dataset + YoY variants
#  (4) Build feature dictionary (codes -> readable names)
#  (5) Build WIDE base panel (macro slice) + readable columns
#  (6) Export: base datasets, metadata, and per-country wide CSVs
#
# NOTE: Paper-specific dataset creation intentionally omitted here.
############################################################

# ----------------------------
# 0) Packages
# ----------------------------
# install.packages(c("readr","dplyr","tidyr","janitor","stringr"))
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

# ----------------------------
# 1) Download OECD SDMX data
# ----------------------------
oecd_download_qna_expenditure <- function(
    start_period = "1960-Q1",
    end_period   = "2024-Q4"
) {
  base <- "https://sdmx.oecd.org/public/rest/data"
  flow <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_NATIO_CURR,1.1"
  
  url <- paste0(
    base, "/", flow, "/all",
    "?startPeriod=", start_period,
    "&endPeriod=", end_period,
    "&dimensionAtObservation=AllDimensions",
    "&format=csvfilewithlabels"
  )
  
  message("Downloading: ", url)
  df_raw <- read_csv(url, show_col_types = FALSE)
  df_raw %>% clean_names()
}

df <- oecd_download_qna_expenditure("1960-Q1", "2024-Q4")

# ----------------------------
# 2) Filter quarterly + calendar&seasonally adjusted
# ----------------------------
## ---- Replace SA with NSA to match paper-style YoY ----
df_q_nsa <- df %>% 
  filter(freq == "Q", adjustment == "N", transformation == "N")

cat("Quarterly NSA rows:", nrow(df_q_nsa), "\n")
cat("Countries:", n_distinct(df_q_nsa$ref_area), "\n")
cat("Time range:", min(df_q_nsa$time_period), "to", max(df_q_nsa$time_period), "\n")

# ----------------------------
# 3) Build base dataset (LONG) + YoY variants
# ----------------------------
key_cols <- c(
  "ref_area","reference_area",
  "transaction","transaction_2",
  "sector","institutional_sector",
  "counterpart_sector","counterpart_institutional_sector",
  "expenditure","expenditure_2",
  "activity","economic_activity",
  "instr_asset","financial_instruments_and_non_financial_assets",
  "unit_measure","unit_of_measure",
  "price_base","price_base_2",
  "table_identifier","table_identifier_2",
  "currency","currency_2"
)

safe_log <- function(x) {
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & x > 0)
  out[idx] <- log(x[idx])
  out
}

build_base_long <- function(df_q_nsa, key_cols) {
  df_q_nsa %>%
    select(all_of(key_cols), time_period, obs_value) %>%
    # de-duplicate identical keys deterministically
    group_by(across(all_of(key_cols)), time_period) %>%
    summarise(
      obs_value = if (all(is.na(obs_value))) NA_real_ else mean(obs_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(time_period) %>%
    group_by(across(all_of(key_cols))) %>%
    mutate(
      log_value  = safe_log(obs_value),
      yoy_log_4q = (log_value - lag(log_value, 4)) * 100,              # paper-style (where defined)
      yoy_diff   = obs_value - lag(obs_value, 4),                      # always defined
      yoy_pct    = if_else(!is.na(lag(obs_value, 4)) & lag(obs_value, 4) != 0,
                           (obs_value / lag(obs_value, 4) - 1) * 100,
                           NA_real_)
    ) %>%
    select(-log_value) %>%
    ungroup()
}

df_base_long <- build_base_long(df_q_nsa, key_cols)
message("Base LONG rows: ", nrow(df_base_long))

# Optional write
write_csv(df_base_long, "oecd_source/oecd_qna_base_long_q_nsa_1960_2024.csv")

# ----------------------------
# 4) Feature dictionary (codes -> readable column names)
# ----------------------------
build_feature_dictionary <- function(df_base_long) {
  df_base_long %>%
    distinct(transaction, transaction_2, price_base, price_base_2, unit_measure, unit_of_measure) %>%
    mutate(
      feature = paste(transaction, price_base, unit_measure, sep = "__"),
      feature_label_raw = paste(transaction_2, price_base_2, unit_of_measure, sep = " | "),
      feature_label = janitor::make_clean_names(feature_label_raw)
    ) %>%
    select(feature, feature_label, feature_label_raw) %>%
    arrange(feature)
}

feature_dict <- build_feature_dictionary(df_base_long)
write_csv(feature_dict, "oecd_metadata/oecd_feature_dictionary.csv")

rename_map <- setNames(feature_dict$feature_label, feature_dict$feature)

# ----------------------------
# 5) Build WIDE base panel (macro slice) + readable feature columns
# ----------------------------
# Macro slice (coherent definition) to avoid duplicate series exploding columns.
macro_filter <- function(df) {
  df %>%
    filter(
      sector == "S1",
      counterpart_sector == "S1",
      expenditure == "_Z",
      activity == "_Z",
      instr_asset == "_Z"
    )
}

build_base_wide <- function(df_base_long) {
  df_feat <- df_base_long %>%
    macro_filter() %>%
    mutate(feature = paste(transaction, price_base, unit_measure, sep = "__")) %>%
    select(country = ref_area, quarter = time_period, feature, obs_value) %>%
    group_by(country, quarter, feature) %>%
    summarise(
      value = if (all(is.na(obs_value))) NA_real_ else mean(obs_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_feat %>%
    pivot_wider(names_from = feature, values_from = value) %>%
    arrange(country, quarter)
}

df_base_wide <- build_base_wide(df_base_long)
message("Base WIDE rows: ", nrow(df_base_wide), " | cols: ", ncol(df_base_wide))
write_csv(df_base_wide, "oecd_processed/oecd_base_panel_wide.csv")

# Rename feature columns to readable names (optional but recommended)
df_base_wide_named <- df_base_wide %>%
  rename_with(~ rename_map[.x], .cols = intersect(names(df_base_wide), names(rename_map)))

message("Base WIDE (named) rows: ", nrow(df_base_wide_named), " | cols: ", ncol(df_base_wide_named))
write_csv(df_base_wide_named, "oecd_processed/oecd_base_panel_wide_named.csv")

# ----------------------------
# 6) Export per-country WIDE CSVs
# ----------------------------
export_country_wide_csvs <- function(df_base_wide_named, out_dir = "oecd_by_country_wide") {
  dir.create(out_dir, showWarnings = FALSE)
  countries <- sort(unique(df_base_wide_named$country))
  
  for (cty in countries) {
    df_cty <- df_base_wide_named %>%
      filter(country == cty) %>%
      arrange(quarter)
    
    write_csv(df_cty, file.path(out_dir, paste0(cty, ".csv")))
  }
  
  message("Wrote ", length(countries), " files to: ", normalizePath(out_dir))
}

export_country_wide_csvs(df_base_wide_named, out_dir = "oecd_processed/oecd_by_country_wide")

# ----------------------------
# 7) Metadata: coverage timespans per country-feature (WIDE)
# ----------------------------
build_coverage_timespan_wide <- function(df_base_wide) {
  value_cols <- setdiff(names(df_base_wide), c("country", "quarter"))
  
  coverage_long <- df_base_wide %>%
    pivot_longer(cols = all_of(value_cols), names_to = "feature", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(country, feature) %>%
    summarise(
      start_q = min(quarter),
      end_q   = max(quarter),
      .groups = "drop"
    )
  
  coverage_long %>%
    mutate(span = paste0(start_q, "-", end_q)) %>%
    select(country, feature, span) %>%
    pivot_wider(names_from = feature, values_from = span) %>%
    arrange(country)
}

coverage_span_wide <- build_coverage_timespan_wide(df_base_wide)
write_csv(coverage_span_wide, "oecd_metadata/oecd_country_feature_timespans_wide.csv")

# Rename coverage columns to readable names as well (optional)
coverage_span_wide_named <- coverage_span_wide %>%
  rename_with(~ rename_map[.x], .cols = intersect(names(coverage_span_wide), names(rename_map)))

write_csv(coverage_span_wide_named, "oecd_metadata/oecd_country_feature_timespans_wide_named.csv")

# ----------------------------
# 8) Quick inspection helpers (optional)
# ----------------------------
# Country list
df_base_long %>%
  distinct(ref_area, reference_area) %>%
  arrange(ref_area) %>%
  print(n = Inf)

# Example: load one per-country wide file
DEU <- read_csv("oecd_processed/oecd_by_country_wide/DEU.csv", show_col_types = FALSE)
head(DEU)

############### paper derivation ####################

paper_iso <- c("CHL","MEX","ARG","ESP","ITA","USA","FRA","JPN","KOR","DEU","ZAF","BRA","RUS","GBR")

gdp_starts <- df_base_long %>%
  filter(transaction == "B1GQ") %>%
  group_by(ref_area) %>%
  summarise(gdp_start = min(time_period[!is.na(obs_value)]),
            gdp_end   = max(time_period[!is.na(obs_value)]),
            .groups="drop") %>%
  filter(ref_area %in% paper_iso) %>%
  arrange(ref_area)

print(gdp_starts, n = Inf)

########################################################

## West Germany (DEW) pull for the same flow
url_dew <- paste0(
  base, "/", flow, "/all",
  "?startPeriod=1960-Q1&endPeriod=2024-Q4",
  "&dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

df_dew_raw <- readr::read_csv(url_dew, show_col_types = FALSE) %>% janitor::clean_names()

df_dew_q <- df_dew_raw %>%
  filter(ref_area == "DEW", freq == "Q", adjustment == "N", transformation == "N")


#########################
paper_starts <- tibble::tibble(
  ref_area = c("ARG","BRA","CHL","DEU","ESP","FRA","GBR","ITA",
               "JPN","KOR","MEX","RUS","USA","ZAF"),
  paper_start = c("1993-Q1","1998-Q2","1996-Q1","1961-Q1","1970-Q1",
                  "1960-Q1","1960-Q1","1960-Q1","1960-Q1","1960-Q1",
                  "1993-Q1","1995-Q1","1960-Q1","1993-Q1")
)

comparison <- gdp_starts %>%
  left_join(paper_starts, by = "ref_area") %>%
  mutate(
    mismatch = gdp_start > paper_start
  )

print(comparison, n = Inf)

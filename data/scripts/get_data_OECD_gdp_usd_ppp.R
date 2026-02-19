############################################################
# OECD QNA Expenditure (USD, PPP-converted, SA, Real Volume)
# Clean reproducible pipeline (1960-Q1 to 2024-Q4)
#
# Output files:
#  1) oecd_usd_ppp_real_base_long_q_sa_1960_2024.csv
#  2) oecd_usd_ppp_real_base_panel_wide_named.csv
#  3) oecd_usd_ppp_real_feature_dictionary.csv
#  4) oecd_usd_ppp_real_country_feature_timespans_wide_named.csv
############################################################

setwd("..")

# =========================
# 0) Packages
# =========================
# install.packages(c("readr","dplyr","tidyr","janitor"), quiet = TRUE)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)

# =========================
# 1) Download (OECD SDMX)
# =========================
base <- "https://sdmx.oecd.org/public/rest/data"
flow <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_USD,1.1"

url_usd <- paste0(
  base, "/", flow, "/all",
  "?startPeriod=1960-Q1&endPeriod=2024-Q4",
  "&dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

df_raw <- read_csv(url_usd, show_col_types = FALSE)
df <- df_raw %>% clean_names()

# =========================
# 2) Restrict to "paper-style" slice
#    Quarterly + SA + Real volume (LR) + USD_PPP
# =========================
df_q <- df %>%
  filter(freq == "Q", adjustment == "Y")

df_q_real <- df_q %>%
  filter(price_base == "LR", unit_measure == "USD_PPP")

cat("Quarterly SA rows:", nrow(df_q), "\n")
cat("Quarterly SA real PPP-USD rows:", nrow(df_q_real), "\n")
cat("Time range:", min(df_q_real$time_period), "to", max(df_q_real$time_period), "\n\n")

# =========================
# 3) Build BASE LONG dataset (de-dup + YoY variants)
# =========================
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

df_base_long_real <- df_q_real %>%
  select(all_of(key_cols), time_period, obs_value) %>%
  group_by(across(all_of(key_cols)), time_period) %>%
  summarise(
    obs_value = if (all(is.na(obs_value))) NA_real_ else mean(obs_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(time_period) %>%
  group_by(across(all_of(key_cols))) %>%
  mutate(
    log_value  = safe_log(obs_value),
    yoy_log_4q = (log_value - lag(log_value, 4)) * 100,      # approx YoY % (log diff)
    yoy_diff   = obs_value - lag(obs_value, 4),              # YoY absolute change
    yoy_pct    = if_else(!is.na(lag(obs_value, 4)) & lag(obs_value, 4) != 0,
                         (obs_value / lag(obs_value, 4) - 1) * 100,
                         NA_real_)
  ) %>%
  select(-log_value) %>%
  ungroup()

cat("BASE LONG rows:", nrow(df_base_long_real), "\n")
cat("Countries:", n_distinct(df_base_long_real$ref_area), "\n\n")

write_csv(df_base_long_real, "oecd_usd_ppp_real_base_long_q_sa_1960_2024.csv")

# =========================
# 4) Fix macro aggregation (P3 is split by sector in this flow)
#    Sum the two P3 sector series to get total P3
# =========================

df_macro_fixed <- df_base_long_real %>%
  group_by(
    ref_area, reference_area,
    transaction, transaction_2,
    time_period,
    price_base, unit_measure
  ) %>%
  summarise(
    obs_value = if (dplyr::first(transaction) == "P3") {
      if (all(is.na(obs_value))) NA_real_ else sum(obs_value, na.rm = TRUE)
    } else {
      if (all(is.na(obs_value))) NA_real_ else mean(obs_value, na.rm = TRUE)
    },
    .groups = "drop"
  )

df_macro_fixed %>%
  count(transaction, sort = TRUE) %>%
  print(n = 20)


# =========================
# 5) Build WIDE base panel (country-quarter) with feature codes
# =========================
df_macro_feat <- df_macro_fixed %>%
  mutate(feature = paste(transaction, price_base, unit_measure, sep = "__")) %>%
  select(country = ref_area, quarter = time_period, feature, obs_value) %>%
  group_by(country, quarter, feature) %>%
  summarise(
    value = if (all(is.na(obs_value))) NA_real_ else mean(obs_value, na.rm = TRUE),
    .groups = "drop"
  )

df_base_wide_real <- df_macro_feat %>%
  pivot_wider(names_from = feature, values_from = value) %>%
  arrange(country, quarter)

cat("BASE WIDE rows:", nrow(df_base_wide_real), "| cols:", ncol(df_base_wide_real), "\n\n")

# =========================
# 6) Feature dictionary + readable WIDE panel
# =========================
feature_dict <- df_base_long_real %>%
  distinct(transaction, transaction_2, price_base, price_base_2, unit_measure, unit_of_measure) %>%
  mutate(
    feature = paste(transaction, price_base, unit_measure, sep = "__"),
    label_raw = paste(transaction_2, price_base_2, unit_of_measure, sep = " | "),
    label = janitor::make_clean_names(label_raw)
  ) %>%
  select(feature, label, label_raw) %>%
  arrange(feature)

rename_map <- setNames(feature_dict$label, feature_dict$feature)

df_base_wide_real_named <- df_base_wide_real %>%
  rename_with(~ rename_map[.x], .cols = intersect(names(df_base_wide_real), names(rename_map)))

write_csv(df_base_wide_real_named, "oecd_usd_ppp_real_base_panel_wide_named.csv")
write_csv(feature_dict, "oecd_usd_ppp_real_feature_dictionary.csv")

# =========================
# 7) Coverage metadata (country × feature) as time spans "start-end"
# =========================
value_cols <- setdiff(names(df_base_wide_real), c("country", "quarter"))

coverage_span_wide <- df_base_wide_real %>%
  pivot_longer(cols = all_of(value_cols), names_to = "feature", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(country, feature) %>%
  summarise(
    start_q = min(quarter),
    end_q   = max(quarter),
    .groups = "drop"
  ) %>%
  mutate(span = paste0(start_q, "-", end_q)) %>%
  select(country, feature, span) %>%
  pivot_wider(names_from = feature, values_from = span) %>%
  arrange(country)

coverage_span_wide_named <- coverage_span_wide %>%
  rename_with(~ rename_map[.x], .cols = intersect(names(coverage_span_wide), names(rename_map)))

write_csv(coverage_span_wide_named, "oecd_usd_ppp_real_country_feature_timespans_wide_named.csv")

cat("DONE: wrote 4 output CSV files.\n")

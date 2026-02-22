############################################################
# OECD QNA Expenditure (USD_PPP, SA, Real Volume)
# Build *paper-style* macro features:
#   - private consumption (households)
#   - government consumption (general government)
#   - capital formation (GFCF)
#   - exports, imports, GDP
############################################################

setwd("..")

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

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

df_raw <- read_csv(url_usd, show_col_types = FALSE) %>% clean_names()

# =========================
# 2) Paper-style slice
#    Quarterly + SA + Real volume (LR) + USD_PPP
# =========================
df_q_real <- df_raw %>%
  filter(freq == "Q", adjustment == "Y", price_base == "LR", unit_measure == "USD_PPP")

# =========================
# 3) Identify sector codes for P3 (consumption)
#    We want: households (private) + general government
# =========================
p3_sectors <- df_q_real %>%
  filter(transaction == "P3") %>%
  distinct(institutional_sector, institutional_sector_name) %>%
  arrange(institutional_sector)

print(p3_sectors, n = Inf)

# Robust picking:
# - household/private: code contains S14 or name contains "household"
# - government: code contains S13 or name contains "government"
hh_codes <- p3_sectors %>%
  filter(
    str_detect(toupper(institutional_sector), "S14") |
      str_detect(tolower(institutional_sector_name), "house")
  ) %>%
  pull(institutional_sector) %>%
  unique()

gov_codes <- p3_sectors %>%
  filter(
    str_detect(toupper(institutional_sector), "S13") |
      str_detect(tolower(institutional_sector_name), "government")
  ) %>%
  pull(institutional_sector) %>%
  unique()

cat("\nHousehold (private) sector codes used:\n"); print(hh_codes)
cat("\nGovernment sector codes used:\n"); print(gov_codes)

# =========================
# 4) Build the 6 required series at (country, quarter)
#    - GDP: B1GQ
#    - Exports: P6
#    - Imports: P7
#    - Capital formation: P51G (GFCF)
#    - Private consumption: P3 + household sector
#    - Government consumption: P3 + government sector
#
# IMPORTANT: We aggregate (sum) over any remaining dims like expenditure/activity/etc.
# =========================
agg_country_quarter <- function(d) {
  d %>%
    group_by(ref_area, reference_area, time_period) %>%
    summarise(value = if (all(is.na(obs_value))) NA_real_ else sum(obs_value, na.rm = TRUE),
              .groups = "drop")
}

gdp <- df_q_real %>%
  filter(transaction == "B1GQ") %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, gdp = value)

exports <- df_q_real %>%
  filter(transaction == "P6") %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, exports = value)

imports <- df_q_real %>%
  filter(transaction == "P7") %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, imports = value)

gfcf <- df_q_real %>%
  filter(transaction == "P51G") %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, capital_formation = value)

c_priv <- df_q_real %>%
  filter(transaction == "P3", institutional_sector %in% hh_codes) %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, private_consumption = value)

c_gov <- df_q_real %>%
  filter(transaction == "P3", institutional_sector %in% gov_codes) %>%
  agg_country_quarter() %>%
  transmute(country = ref_area, quarter = time_period, government_consumption = value)

# =========================
# 5) Merge into final wide panel
# =========================
panel <- list(gdp, c_priv, c_gov, gfcf, exports, imports) %>%
  reduce(full_join, by = c("country", "quarter")) %>%
  arrange(country, quarter)

# Quick check: how many non-missing observations per column?
na_check <- panel %>%
  summarise(across(-c(country, quarter), ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "series", values_to = "non_missing_obs")

print(na_check)

# =========================
# 6) Save
# =========================
write_csv(panel, "oecd_processed/oecd_usd_ppp_real_paper_features_wide.csv")

cat("\nDONE: wrote oecd_processed/oecd_usd_ppp_real_paper_features_wide.csv\n")
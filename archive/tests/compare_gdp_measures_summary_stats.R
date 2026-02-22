############################################################
# Compare ALL available OECD GDP measures against paper Table 1
#
# Goal: Download GDP (B1GQ) at every (price_base × unit_measure)
#       combo from both USD and national-currency QNA flows,
#       apply the same sample filters as Mello (2024), compute
#       summary stats (GDP level, YoY growth) for Winner vs
#       Non-winner, and compare to find the best match.
#
# Output: Console tables + CSV with all GDP measure comparisons
############################################################

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

# Set working directory to Data folder (parent of scripts)
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), ".."))
# Alternative: setwd("..") if running from scripts/

cat("Working directory:", getwd(), "\n\n")

# ═══════════════════════════════════════════════════════════
# 1) Paper sample definition
# ═══════════════════════════════════════════════════════════

controls <- c(
  "ARG","AUS","AUT","BEL","BGR","CAN","CHL","COL","CRI","HRV","CZE","DNK","EST","FIN",
  "GRC","HUN","IND","IDN","ISL","IRL","ISR","LVA","LTU","LUX","NLD","NZL","NOR","POL",
  "PRT","ROU","SAU","SVK","SVN","SWE","CHE","TUR"
)

hosts <- c(
  "BRA","GBR","FRA","DEU","ITA","JPN","MEX","ZAF","KOR","ESP","RUS","USA"
)

paper_countries <- sort(unique(c(controls, hosts)))

# Winner countries (ever won a WC in the sample: 1966-2018)
winner_countries <- c("BRA","DEU","ESP","FRA","GBR","ITA")

# ═══════════════════════════════════════════════════════════
# 2) Download OECD data: two flows
# ═══════════════════════════════════════════════════════════

base_url <- "https://sdmx.oecd.org/public/rest/data"

# --- Flow 1: USD-denominated ---
flow_usd <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_USD,1.1"
url_usd <- paste0(
  base_url, "/", flow_usd, "/all",
  "?startPeriod=1960-Q1&endPeriod=2024-Q4",
  "&dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

cat("Downloading USD flow...\n")
df_usd_raw <- read_csv(url_usd, show_col_types = FALSE) %>% clean_names()
cat("  Rows:", nrow(df_usd_raw), "\n")

# --- Flow 2: National currency ---
flow_nc <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_NATIO_CURR,1.1"
url_nc <- paste0(
  base_url, "/", flow_nc, "/all",
  "?startPeriod=1960-Q1&endPeriod=2024-Q4",
  "&dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

cat("Downloading national currency flow...\n")
df_nc_raw <- read_csv(url_nc, show_col_types = FALSE) %>% clean_names()
cat("  Rows:", nrow(df_nc_raw), "\n\n")

# ═══════════════════════════════════════════════════════════
# 3) Filter: GDP only (B1GQ), Quarterly, Seasonally adjusted
# ═══════════════════════════════════════════════════════════

filter_gdp_q_sa <- function(df, flow_label) {
  d <- df %>%
    filter(
      transaction == "B1GQ",
      freq == "Q",
      adjustment == "Y"
    ) %>%
    mutate(flow = flow_label)
  
  cat(flow_label, "- GDP Q SA rows:", nrow(d), "\n")
  d
}

gdp_usd <- filter_gdp_q_sa(df_usd_raw, "USD_flow")
gdp_nc  <- filter_gdp_q_sa(df_nc_raw,  "NatCurr_flow")

# Harmonise column types before binding (ref_year_price can be dbl vs chr)
common_cols <- intersect(names(gdp_usd), names(gdp_nc))
for (col in common_cols) {
  if (class(gdp_usd[[col]])[1] != class(gdp_nc[[col]])[1]) {
    gdp_usd[[col]] <- as.character(gdp_usd[[col]])
    gdp_nc[[col]]  <- as.character(gdp_nc[[col]])
  }
}

# Combine
gdp_all <- bind_rows(gdp_usd, gdp_nc)

# ═══════════════════════════════════════════════════════════
# 4) Enumerate all (flow × price_base × unit_measure) combos
# ═══════════════════════════════════════════════════════════

combos <- gdp_all %>%
  distinct(flow, price_base, price_base_2, unit_measure, unit_of_measure) %>%
  arrange(flow, price_base, unit_measure) %>%
  mutate(measure_id = paste(flow, price_base, unit_measure, sep = " | "))

cat("\n========================================\n")
cat("Available GDP measure combinations:\n")
cat("========================================\n")
print(as.data.frame(combos), row.names = FALSE)
cat("\nTotal combinations:", nrow(combos), "\n\n")

# ═══════════════════════════════════════════════════════════
# 5) For each combo: extract country-quarter GDP panel
# ═══════════════════════════════════════════════════════════

safe_log <- function(x) {
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & x > 0)
  out[idx] <- log(x[idx])
  out
}

build_gdp_panel <- function(df_combo) {
  # Aggregate to (country, quarter) in case of duplicate dimensions
  panel <- df_combo %>%
    group_by(ref_area, time_period) %>%
    summarise(gdp = if (all(is.na(obs_value))) NA_real_ else sum(obs_value, na.rm = TRUE),
              .groups = "drop") %>%
    rename(country = ref_area, quarter = time_period) %>%
    # Paper filters
    filter(
      country %in% paper_countries,
      quarter >= "1961-Q1",
      quarter <= "2021-Q4"
    ) %>%
    # Brazil special case
    filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
    arrange(country, quarter) %>%
    # Compute YoY log growth
    group_by(country) %>%
    mutate(
      yoy_log_4q = (safe_log(gdp) - lag(safe_log(gdp), 4)) * 100,
      yoy_pct    = if_else(!is.na(lag(gdp, 4)) & lag(gdp, 4) != 0,
                           (gdp / lag(gdp, 4) - 1) * 100, NA_real_)
    ) %>%
    ungroup() %>%
    # Winner group
    mutate(
      winner_group = if_else(country %in% winner_countries, "Winner", "Non-winner"),
      year = as.integer(str_extract(quarter, "\\d{4}"))
    ) %>%
    # Event study sample: require YoY to be defined
    filter(!is.na(yoy_log_4q))
  
  panel
}

# ═══════════════════════════════════════════════════════════
# 6) Compute summary stats for each combo
# ═══════════════════════════════════════════════════════════

period_label <- function(year) {
  case_when(
    year >= 1960 & year <= 1980 ~ "1960-80",
    year > 1980  & year <= 2000 ~ "1980-2000",
    year > 2000  & year <= 2020 ~ "2000-20",
    TRUE ~ NA_character_
  )
}

compute_summary <- function(panel, measure_id) {
  panel <- panel %>% mutate(period = period_label(year))
  
  period_levels <- c("1960-80", "1980-2000", "2000-20", "Full sample")
  
  make_block <- function(d, period_name) {
    # GDP level (divided by 1000 -> thousands of millions)
    d_gdp <- d %>% filter(!is.na(gdp))
    # YoY log growth
    d_yoy <- d %>% filter(!is.na(yoy_log_4q))
    # YoY pct growth
    d_yoy_pct <- d %>% filter(!is.na(yoy_pct))
    
    stat <- function(dd, var) {
      w <- dd %>% filter(winner_group == "Winner")
      n <- dd %>% filter(winner_group == "Non-winner")
      tibble(
        w_mean = mean(w[[var]], na.rm = TRUE),
        w_sd   = sd(w[[var]], na.rm = TRUE),
        w_n    = nrow(w),
        n_mean = mean(n[[var]], na.rm = TRUE),
        n_sd   = sd(n[[var]], na.rm = TRUE),
        n_n    = nrow(n)
      )
    }
    
    gdp_stats <- stat(d_gdp %>% mutate(gdp_k = gdp / 1000), "gdp_k")
    yoy_log_stats <- stat(d_yoy, "yoy_log_4q")
    yoy_pct_stats <- stat(d_yoy_pct, "yoy_pct")
    
    bind_rows(
      gdp_stats %>% mutate(variable = "GDP (thousands of millions)", period = period_name),
      yoy_log_stats %>% mutate(variable = "YoY GDP growth (log diff × 100)", period = period_name),
      yoy_pct_stats %>% mutate(variable = "YoY GDP growth (pct change)", period = period_name)
    )
  }
  
  results <- bind_rows(
    make_block(panel %>% filter(period == "1960-80"), "1960-80"),
    make_block(panel %>% filter(period == "1980-2000"), "1980-2000"),
    make_block(panel %>% filter(period == "2000-20"), "2000-20"),
    make_block(panel, "Full sample")
  ) %>%
    mutate(measure_id = measure_id) %>%
    select(measure_id, period, variable, everything())
  
  results
}

# ═══════════════════════════════════════════════════════════
# 7) Run for all combos
# ═══════════════════════════════════════════════════════════

all_results <- list()
all_coverage <- list()

for (i in seq_len(nrow(combos))) {
  mid <- combos$measure_id[i]
  cat("Processing:", mid, "...")
  
  df_combo <- gdp_all %>%
    filter(flow == combos$flow[i],
           price_base == combos$price_base[i],
           unit_measure == combos$unit_measure[i])
  
  panel <- tryCatch(build_gdp_panel(df_combo), error = function(e) NULL)
  
  if (is.null(panel) || nrow(panel) == 0) {
    cat(" SKIPPED (no data after filters)\n")
    next
  }
  
  # Coverage info
  cov <- panel %>%
    summarise(
      n_countries = n_distinct(country),
      n_obs = n(),
      n_winners = n_distinct(country[winner_group == "Winner"]),
      n_nonwinners = n_distinct(country[winner_group == "Non-winner"]),
      min_quarter = min(quarter),
      max_quarter = max(quarter)
    ) %>%
    mutate(measure_id = mid)
  
  all_coverage[[i]] <- cov
  
  summ <- tryCatch(compute_summary(panel, mid), error = function(e) {
    cat(" ERROR:", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(summ)) {
    all_results[[i]] <- summ
    cat(" OK (", cov$n_countries, "countries,", cov$n_obs, "obs)\n")
  }
}

results_df <- bind_rows(all_results)
coverage_df <- bind_rows(all_coverage)

# ═══════════════════════════════════════════════════════════
# 8) Format and display comparison tables
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  COVERAGE SUMMARY PER GDP MEASURE\n")
cat("══════════════════════════════════════════════════════════════\n")
print(as.data.frame(coverage_df), row.names = FALSE)

# Format results for nice comparison
results_fmt <- results_df %>%
  mutate(
    Winner = sprintf("%.2f (%.2f)", w_mean, w_sd),
    `Non-winner` = sprintf("%.2f (%.2f)", n_mean, n_sd),
    W_obs = w_n,
    NW_obs = n_n
  ) %>%
  select(measure_id, period, variable, Winner, `Non-winner`, W_obs, NW_obs)

# Print by variable type
cat("\n\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  GDP LEVEL COMPARISON (thousands of millions)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

gdp_level_comp <- results_fmt %>%
  filter(str_detect(variable, "^GDP \\(thousands")) %>%
  arrange(period, measure_id)

for (p in c("1960-80", "1980-2000", "2000-20", "Full sample")) {
  cat("--- Period:", p, "---\n")
  block <- gdp_level_comp %>% filter(period == p) %>% select(-period, -variable)
  print(as.data.frame(block), row.names = FALSE)
  cat("\n")
}

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  YoY GDP GROWTH COMPARISON (log diff × 100)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

yoy_log_comp <- results_fmt %>%
  filter(str_detect(variable, "log diff")) %>%
  arrange(period, measure_id)

for (p in c("1960-80", "1980-2000", "2000-20", "Full sample")) {
  cat("--- Period:", p, "---\n")
  block <- yoy_log_comp %>% filter(period == p) %>% select(-period, -variable)
  print(as.data.frame(block), row.names = FALSE)
  cat("\n")
}

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  YoY GDP GROWTH COMPARISON (pct change)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

yoy_pct_comp <- results_fmt %>%
  filter(str_detect(variable, "pct change")) %>%
  arrange(period, measure_id)

for (p in c("1960-80", "1980-2000", "2000-20", "Full sample")) {
  cat("--- Period:", p, "---\n")
  block <- yoy_pct_comp %>% filter(period == p) %>% select(-period, -variable)
  print(as.data.frame(block), row.names = FALSE)
  cat("\n")
}

# ═══════════════════════════════════════════════════════════
# 9) Save full results
# ═══════════════════════════════════════════════════════════

write_csv(results_df, "mello_paper_replication/gdp_measure_comparison_summary_stats.csv")
write_csv(coverage_df, "mello_paper_replication/gdp_measure_comparison_coverage.csv")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Saved to mello_paper_replication/\n")
cat("    - gdp_measure_comparison_summary_stats.csv\n")
cat("    - gdp_measure_comparison_coverage.csv\n")
cat("══════════════════════════════════════════════════════════════\n")

# ═══════════════════════════════════════════════════════════
# 10) Quick reference: YOUR CURRENT VALUES
#     (from existing event study sample for comparison)
# ═══════════════════════════════════════════════════════════

cat("\n\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  YOUR CURRENT PIPELINE VALUES (LR | USD_PPP)\n")
cat("  For reference / cross-check\n")
cat("══════════════════════════════════════════════════════════════\n\n")

existing_es <- tryCatch(
  read_csv("mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE),
  error = function(e) NULL
)

if (!is.null(existing_es)) {
  gdp_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
  yoy_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_log_4q"
  yoy_pct_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct"
  
  es <- existing_es %>%
    mutate(
      winner_group = if_else(country %in% winner_countries, "Winner", "Non-winner"),
      year = as.integer(str_extract(quarter, "\\d{4}")),
      period = period_label(year),
      gdp_k = .data[[gdp_col]] / 1000
    )
  
  for (p in c("1960-80", "1980-2000", "2000-20", "Full sample")) {
    d <- if (p == "Full sample") es else es %>% filter(period == p)
    d <- d %>% filter(!is.na(gdp_k))
    
    w <- d %>% filter(winner_group == "Winner")
    n <- d %>% filter(winner_group == "Non-winner")
    
    cat("--- Period:", p, "---\n")
    cat(sprintf("  GDP level (thousands): Winner %.2f (%.2f) [n=%d]  |  Non-winner %.2f (%.2f) [n=%d]\n",
                mean(w$gdp_k, na.rm=TRUE), sd(w$gdp_k, na.rm=TRUE), nrow(w),
                mean(n$gdp_k, na.rm=TRUE), sd(n$gdp_k, na.rm=TRUE), nrow(n)))
    
    w_yoy <- w %>% filter(!is.na(.data[[yoy_col]]))
    n_yoy <- n %>% filter(!is.na(.data[[yoy_col]]))
    cat(sprintf("  YoY log:              Winner %.2f (%.2f) [n=%d]  |  Non-winner %.2f (%.2f) [n=%d]\n",
                mean(w_yoy[[yoy_col]], na.rm=TRUE), sd(w_yoy[[yoy_col]], na.rm=TRUE), nrow(w_yoy),
                mean(n_yoy[[yoy_col]], na.rm=TRUE), sd(n_yoy[[yoy_col]], na.rm=TRUE), nrow(n_yoy)))
    
    w_pct <- w %>% filter(!is.na(.data[[yoy_pct_col]]))
    n_pct <- n %>% filter(!is.na(.data[[yoy_pct_col]]))
    cat(sprintf("  YoY pct:              Winner %.2f (%.2f) [n=%d]  |  Non-winner %.2f (%.2f) [n=%d]\n\n",
                mean(w_pct[[yoy_pct_col]], na.rm=TRUE), sd(w_pct[[yoy_pct_col]], na.rm=TRUE), nrow(w_pct),
                mean(n_pct[[yoy_pct_col]], na.rm=TRUE), sd(n_pct[[yoy_pct_col]], na.rm=TRUE), nrow(n_pct)))
  }
} else {
  cat("  Could not load existing event study sample for cross-check.\n")
}


cat("\n\nDONE.\n")
cat("Compare the GDP level columns across measures.\n")
cat("For YoY growth: all real measures should give identical log-growth\n")
cat("  (since PPP/exchange-rate scaling cancels in log differences).\n")
cat("The key difference will be in GDP LEVELS and GDP per capita.\n")

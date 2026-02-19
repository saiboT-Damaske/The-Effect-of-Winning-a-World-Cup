# ============================================================
# Replicate Table 2 (Event Study) — single script (NEW DATASET)
# Paper: Mello (Oxford OBES) — World Cup win & GDP event study
#
# Key change vs your old script:
# - NO YoY columns are recalculated
# - We read the already-enriched panel with YoY columns computed
#   on the unpruned data, then do only the remaining steps:
#   trimming, event-time construction, and estimation (fixest + lfe).
# ============================================================

# -----------------------------
# 0) Setup
# -----------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(lfe)

# -----------------------------
# 1) Load enriched panel (already contains *_yoy_log_4q and *_yoy_pct)
# -----------------------------
df0 <- read_csv(
  "../data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
  show_col_types = FALSE
) %>%
  mutate(country = as.character(country))

stopifnot(all(c("country", "quarter") %in% names(df0)))

# -----------------------------
# 2) Trim to paper country set + window (still NO YoY recomputation)
# -----------------------------
controls <- c(
  "ARG","AUS","AUT","BEL","BGR","CAN","CHL","COL","CRI","HRV","CZE","DNK","EST","FIN",
  "GRC","HUN","IND","IDN","ISL","IRL","ISR","LVA","LTU","LUX","NLD","NZL","NOR","POL",
  "PRT","ROU","SAU","SVK","SVN","SWE","CHE","TUR"
)

hosts <- c(
  "BRA","GBR","FRA","DEU","ITA","JPN","MEX","ZAF","KOR","ESP","RUS","USA"
)

paper_countries <- sort(unique(c(controls, hosts)))

df <- df0 %>%
  filter(
    country %in% paper_countries,
    quarter >= "1961-Q1",
    quarter <= "2021-Q4"
  ) %>%
  # Paper special case: Brazil starts 1998-Q2
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  mutate(
    year = if ("year" %in% names(.)) year else as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = if ("qtr"  %in% names(.)) qtr  else as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    tq   = year * 4L + qtr
  ) %>%
  arrange(country, tq)

# -----------------------------
# 3) Define outcome + control (match Eq. (1)) WITHOUT recomputing YoY
#    Outcome: Δ4 log GDP (pp) already exists as *_yoy_log_4q
#    Control: ln(GDP_{t-4}) computed from levels (not a YoY)
# -----------------------------
gdp_level  <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
gdp_yoylog <- paste0(gdp_level, "_yoy_log_4q")

stopifnot(gdp_level %in% names(df), gdp_yoylog %in% names(df))

df <- df %>%
  group_by(country) %>%
  arrange(tq, .by_group = TRUE) %>%
  mutate(
    ln_gdp    = log(.data[[gdp_level]]),
    ln_gdp_l4 = lag(ln_gdp, 4),
    dy_gdp_pp = .data[[gdp_yoylog]],
    host      = as.integer(host)
  ) %>%
  ungroup()

# Keep only rows where dependent variable exists (paper’s Table 2 uses YoY GDP growth)
df <- df %>%
  filter(!is.na(dy_gdp_pp))

# -----------------------------
# 4) World Cup win events (l = 0 in Q2 of winning year)
#    England 1966 mapped to GBR (UK aggregate in OECD)
# -----------------------------
win_events <- tibble::tribble(
  ~country, ~year,
  "ENG", 1966,
  "DEU", 1974,
  "ITA", 1982,
  "DEU", 1990,
  "FRA", 1998,
  "BRA", 2002,
  "ITA", 2006,
  "ESP", 2010,
  "DEU", 2014,
  "FRA", 2018
) %>%
  mutate(
    country = if_else(country == "ENG", "GBR", country),
    qtr = 2L
  )

tq_map <- df %>% distinct(year, qtr, tq)

win_events <- win_events %>%
  left_join(tq_map, by = c("year", "qtr")) %>%
  rename(tq_event = tq)

stopifnot(!any(is.na(win_events$tq_event)))

winner_countries <- win_events %>% distinct(country) %>% pull(country)

df <- df %>%
  mutate(winner = as.integer(country %in% winner_countries))

# -----------------------------
# 5) Relative time construction (nearest win; “restart halfway”)
#    Controls: rel_time is a harmless constant (won’t matter since winner=0)
# -----------------------------
assign_nearest_event <- function(tq_vec, event_tq_vec) {
  sapply(tq_vec, function(tq0) {
    diffs <- tq0 - event_tq_vec
    diffs[which.min(abs(diffs))]
  })
}

df <- df %>%
  group_by(country) %>%
  group_modify(~{
    ctry <- .y$country[[1]]
    ev <- win_events %>% filter(country == ctry) %>% pull(tq_event)
    
    if (length(ev) > 0) {
      .x$rel_time <- assign_nearest_event(.x$tq, ev)
    } else {
      .x$rel_time <- 0L
    }
    .x
  }) %>%
  ungroup() %>%
  mutate(
    rel_time_bin = case_when(
      rel_time <= -16 ~ -16L,
      rel_time >=  16 ~  16L,
      TRUE ~ as.integer(rel_time)
    )
  )

# -----------------------------
# 6) Sanity checks
# -----------------------------
cat("\n--- Sample counts (before estimation) ---\n")
print(df %>% summarise(
  n_total = n(),
  n_winner = sum(winner == 1),
  n_control = sum(winner == 0),
  n_countries = n_distinct(country),
  n_winner_countries = n_distinct(country[winner == 1])
))

cat("\n--- Missingness in regression variables ---\n")
print(df %>% summarise(
  miss_dy = mean(is.na(dy_gdp_pp)),
  miss_lngdp_l4 = mean(is.na(ln_gdp_l4)),
  miss_host = mean(is.na(host)),
  miss_relbin = mean(is.na(rel_time_bin))
))

# Optional: check treated-country composition by event time AFTER complete-case restriction
df_cc <- df %>%
  filter(!is.na(ln_gdp_l4), !is.na(host), !is.na(rel_time_bin))

cat("\n--- Treated composition by event-time (complete cases) ---\n")
print(df_cc %>%
        filter(winner == 1) %>%
        group_by(rel_time_bin) %>%
        summarise(treated_cty = n_distinct(country), treated_n = n(), .groups = "drop") %>%
        arrange(rel_time_bin), n = Inf)

# -----------------------------
# 7) Estimate Table 2 regression (fixest)
# -----------------------------
m_fixest <- feols(
  dy_gdp_pp ~ i(rel_time_bin, winner, ref = 0) + host + ln_gdp_l4 | country + tq,
  cluster = ~country,
  data = df_cc
)

cat("\n--- FIXEST summary ---\n")
print(summary(m_fixest))
cat("\nFIXEST nobs:", nobs(m_fixest), "\n")

# Table-2-like extraction
ct <- as.data.frame(coeftable(m_fixest))
ct$term <- rownames(ct)
rownames(ct) <- NULL

tab2_like_fixest <- ct %>%
  transmute(term, estimate = Estimate, se = `Std. Error`) %>%
  filter(term %in% c("host", "ln_gdp_l4") | str_detect(term, "rel_time_bin::")) %>%
  mutate(
    l = if_else(str_detect(term, "rel_time_bin::"),
                as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
                NA_integer_),
    sort_key = case_when(
      term == "ln_gdp_l4" ~ -999L,
      term == "host"     ~ -998L,
      TRUE ~ l
    )
  ) %>%
  arrange(sort_key) %>%
  select(term, estimate, se)

cat("\n--- Table-2-like (FIXEST) ---\n")
print(tab2_like_fixest, n = Inf)

# -----------------------------
# 8) Estimate Table 2 regression (lfe)
# -----------------------------
df_cc <- df_cc %>%
  mutate(
    rel_time_bin_f = relevel(factor(rel_time_bin), ref = "0")
  )

m_lfe <- felm(
  dy_gdp_pp ~ rel_time_bin_f:winner + host + ln_gdp_l4 | country + tq | 0 | country,
  data = df_cc
)

cat("\n--- LFE summary ---\n")
print(summary(m_lfe))

# Table-2-like extraction (lfe)
ct2 <- summary(m_lfe)$coefficients
ct2 <- data.frame(term = rownames(ct2), estimate = ct2[, 1], se = ct2[, 2], row.names = NULL)

tab2_like_lfe <- ct2 %>%
  filter(term %in% c("host", "ln_gdp_l4") | grepl("^rel_time_bin_f.*:winner$", term)) %>%
  mutate(
    l = ifelse(grepl("^rel_time_bin_f", term),
               as.integer(sub("^rel_time_bin_f(.*):winner$", "\\1", term)),
               NA_integer_),
    sort_key = case_when(
      term == "ln_gdp_l4" ~ -999L,
      term == "host"     ~ -998L,
      TRUE ~ l
    )
  ) %>%
  arrange(sort_key) %>%
  select(term, estimate, se)

cat("\n--- Table-2-like (LFE) ---\n")
print(tab2_like_lfe, n = Inf)

# -----------------------------
# 9) Quick cross-check: any lingering ln_gdp_l4 NA at a specific bin?
# -----------------------------
cat("\n--- NA check example (rel_time_bin == -14, winners only) ---\n")
print(df %>%
        filter(rel_time_bin == -14, winner == 1) %>%
        count(country, is.na(ln_gdp_l4)), n = Inf)

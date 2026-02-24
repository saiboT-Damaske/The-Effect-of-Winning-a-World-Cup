# paper_replicate_event_study.R
# Replicates Mello (2024) Table 2 event study, estimated with both fixest and lfe.

#######################################################
# 0) Setup
#######################################################
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(lfe)

#######################################################
# 1) Load data
#######################################################
df0 <- read_csv(
  "Data/mello_paper_replication/paper_replication_sample.csv",
  show_col_types = FALSE
)

cat("Loaded:", nrow(df0), "rows,", n_distinct(df0$country), "countries\n")

#######################################################
# 2) Winner events and country indicator
#######################################################
# winner=1 only flags the event quarter, so we need to pull out
# which countries won and when, then create a static indicator
win_events <- df0 %>%
  filter(winner == 1) %>%
  select(country, year, qtr, tq) %>%
  rename(tq_event = tq)

cat("\nWin events:\n")
print(as.data.frame(win_events))

winner_countries <- unique(win_events$country)
cat("\nWinner countries:", paste(winner_countries, collapse = ", "), "\n")

df <- df0 %>%
  mutate(
    is_winner = as.integer(country %in% winner_countries),
    host      = as.integer(host)
  )

#######################################################
# 3) Dependent variable
#######################################################
# both dy and the lagged level are pre-computed in the CSV
df <- df %>%
  rename(dy_gdp = gdp_yoy_log_4q)

#######################################################
# 4) Relative time to nearest win event
#######################################################
# for winners: distance to nearest WC win (restarts halfway between wins)
# for non-winners: just set to 0 (interacted away anyway)
assign_nearest_event <- function(tq_vec, event_tq_vec) {
  sapply(tq_vec, function(tq0) {
    diffs <- tq0 - event_tq_vec
    diffs[which.min(abs(diffs))]
  })
}

df <- df %>%
  group_by(country) %>%
  group_modify(~ {
    ctry <- .y$country[[1]]
    ev <- win_events %>% filter(country == ctry) %>% pull(tq_event)
    if (length(ev) > 0) {
      .x$rel_time <- assign_nearest_event(.x$tq, ev)
    } else {
      .x$rel_time <- 0L
    }
    .x
  }) %>%
  ungroup()

# bin endpoints at +-16 like the paper
df <- df %>%
  mutate(
    rel_time_bin = case_when(
      rel_time <= -16 ~ -16L,
      rel_time >=  16 ~  16L,
      TRUE            ~ as.integer(rel_time)
    )
  )

#######################################################
# 5) Estimation sample (complete cases)
#######################################################
df_cc <- df %>%
  filter(!is.na(dy_gdp), !is.na(ln_gdp_l4), !is.na(host), !is.na(rel_time_bin))

cat("\n--- Estimation sample ---\n")
cat("  Observations:", nrow(df_cc), "\n")
cat("  Countries:   ", n_distinct(df_cc$country), "\n")
cat("  Winners:     ", sum(df_cc$is_winner == 1), "\n")
cat("  Non-winners: ", sum(df_cc$is_winner == 0), "\n")

#######################################################
# 6) Estimate with fixest
#######################################################
m_fixest <- feols(
  dy_gdp ~ i(rel_time_bin, is_winner, ref = 0) + host + ln_gdp_l4
         | country + tq,
  cluster = ~country,
  data    = df_cc
)

cat("\n--- FIXEST summary ---\n")
print(summary(m_fixest))
cat("N obs:", nobs(m_fixest), "   Within R²:", fitstat(m_fixest, "wr2")[[1]], "\n")

# pull out the coefficient table in a tidy format
ct_fe <- as.data.frame(coeftable(m_fixest))
ct_fe$term <- rownames(ct_fe)
rownames(ct_fe) <- NULL

tab2_fixest <- ct_fe %>%
  transmute(
    term,
    estimate = Estimate,
    se       = `Std. Error`,
    tval     = `t value`,
    pval     = `Pr(>|t|)`
  ) %>%
  filter(term %in% c("host", "ln_gdp_l4") | str_detect(term, "rel_time_bin::")) %>%
  mutate(
    l = if_else(
      str_detect(term, "rel_time_bin::"),
      as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
      NA_integer_
    ),
    sort_key = case_when(
      term == "ln_gdp_l4" ~ -999L,
      term == "host"       ~ -998L,
      TRUE                 ~ l
    )
  ) %>%
  arrange(sort_key) %>%
  select(term, l, estimate, se, tval, pval)

cat("\n--- Table 2 coefficients (fixest) ---\n")
print(as_tibble(tab2_fixest), n = Inf)

#######################################################
# 7) Estimate with lfe (cross-check)
#######################################################
df_cc <- df_cc %>%
  mutate(rel_time_bin_f = relevel(factor(rel_time_bin), ref = "0"))

m_lfe <- felm(
  dy_gdp ~ rel_time_bin_f:is_winner + host + ln_gdp_l4
         | country + tq | 0 | country,
  data = df_cc
)

cat("\n--- LFE summary ---\n")
print(summary(m_lfe))

ct_lfe <- summary(m_lfe)$coefficients
ct_lfe <- data.frame(
  term     = rownames(ct_lfe),
  estimate = ct_lfe[, 1],
  se       = ct_lfe[, 2],
  tval     = ct_lfe[, 3],
  pval     = ct_lfe[, 4],
  row.names = NULL
)

tab2_lfe <- ct_lfe %>%
  filter(term %in% c("host", "ln_gdp_l4") |
         grepl("^rel_time_bin_f.*:is_winner$", term)) %>%
  mutate(
    l = ifelse(
      grepl("^rel_time_bin_f", term),
      as.integer(sub("^rel_time_bin_f(.*):is_winner$", "\\1", term)),
      NA_integer_
    ),
    sort_key = case_when(
      term == "ln_gdp_l4" ~ -999L,
      term == "host"       ~ -998L,
      TRUE                 ~ l
    )
  ) %>%
  arrange(sort_key) %>%
  select(term, l, estimate, se, tval, pval)

cat("\n--- Table 2 coefficients (lfe) ---\n")
print(as_tibble(tab2_lfe), n = Inf)

#######################################################
# 8) Compare fixest vs lfe
#######################################################
cat("\n--- Comparison (fixest vs lfe) ---\n")
comp <- tab2_fixest %>%
  select(l, est_fixest = estimate, se_fixest = se) %>%
  left_join(
    tab2_lfe %>% select(l, est_lfe = estimate, se_lfe = se),
    by = "l"
  ) %>%
  mutate(
    diff_est = est_fixest - est_lfe,
    diff_se  = se_fixest - se_lfe
  )
print(as_tibble(comp), n = Inf)

#######################################################
# 9) Save results
#######################################################
write_csv(tab2_fixest, "mello_paper_replication/results/event_study_coefficients_R_fixest.csv")
write_csv(tab2_lfe,    "mello_paper_replication/results/event_study_coefficients_R_lfe.csv")

cat("\nResults saved to:\n")
cat("  mello_paper_replication/results/event_study_coefficients_R_fixest.csv\n")
cat("  mello_paper_replication/results/event_study_coefficients_R_lfe.csv\n")

#######################################################
# 10) Mello's Table 2 reference values
#######################################################
cat("\n\n========================================\n")
cat("  Mello (2024) Table 2 reference values\n")
cat("========================================\n")
cat("  GDP(-4) = -1.368 (0.588)\n")
cat("  Host    = -0.591 (0.545)\n")
cat("  l=-1 :  0.125 (0.206)\n")
cat("  l=+1 :  0.454 (0.246)*\n")
cat("  l=+2 :  0.683 (0.370)*\n")
cat("  N = 8,637  |  Within R² = 0.423\n")
cat("========================================\n")

cat("\nDONE.\n")
# ============================================================
# Event Study Replication — ALL 6 GDP Features (fixest)
# Paper: Mello (Oxford OBES 2024)
#
# Features:
#   1. GDP (gross domestic product)
#   2. Private consumption (households)
#   3. Government consumption (general government)
#   4. Capital formation (GFCF)
#   5. Exports
#   6. Imports
#
# Model (Eq. 1 from the paper, applied to each feature y):
#   Δ4 log(y_it) ~ Σ_l β_l · 1(l) · winner_i + host_it + ln(y_{i,t-4}) | country + tq
#   clustered at country level
#
# Outputs:
#   - Console: coefficient table per feature
#   - CSV:  results/event_study_all_features_coefficients.csv
#   - Plot: event_study_plots/event_study_gdp_components_figure2.pdf
# ============================================================

# -----------------------------
# 0) Setup
# -----------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(ggplot2)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# -----------------------------
# 1) Load enriched panel (6-feature + pop + WC + YoY)
#    Note: join_and_calculate_metrics_gdp_pop_wc.R produces this file
#    (input changed to 6-feature panel, but output filename kept)
# -----------------------------
df0 <- read_csv(
  "Data/oecd_processed/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
  show_col_types = FALSE
) %>%
  mutate(country = as.character(country))

stopifnot(all(c("country", "quarter") %in% names(df0)))

# Show available columns for diagnostics
cat("Available columns:\n")
cat(paste(names(df0), collapse = "\n"), "\n\n")

# -----------------------------
# 2) Trim to paper country set + window
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
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  mutate(
    year = if ("year" %in% names(.)) year else as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = if ("qtr"  %in% names(.)) qtr  else as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    tq   = year * 4L + qtr
  ) %>%
  arrange(country, tq)

# -----------------------------
# 3) Define features
# -----------------------------
features <- list(
  list(name = "gdp",
       level = "gdp",
       yoy   = "gdp_yoy_log_4q",
       label = "GDP"),
  list(name = "private_consumption",
       level = "private_consumption",
       yoy   = "private_consumption_yoy_log_4q",
       label = "Private consumption"),
  list(name = "government_consumption",
       level = "government_consumption",
       yoy   = "government_consumption_yoy_log_4q",
       label = "Government consumption"),
  list(name = "capital_formation",
       level = "capital_formation",
       yoy   = "capital_formation_yoy_log_4q",
       label = "Capital formation"),
  list(name = "exports",
       level = "exports",
       yoy   = "exports_yoy_log_4q",
       label = "Exports"),
  list(name = "imports",
       level = "imports",
       yoy   = "imports_yoy_log_4q",
       label = "Imports")
)

# Check which features are available
available_features <- list()
for (f in features) {
  if (f$level %in% names(df) && f$yoy %in% names(df)) {
    available_features <- c(available_features, list(f))
    cat("  OK:", f$label, "(", f$level, ",", f$yoy, ")\n")
  } else {
    cat("  MISSING:", f$label, "- level:", f$level %in% names(df), "yoy:", f$yoy %in% names(df), "\n")
  }
}
cat("\nAvailable features:", length(available_features), "/", length(features), "\n\n")

# -----------------------------
# 4) World Cup win events
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
  mutate(
    winner = as.integer(country %in% winner_countries),
    host   = as.integer(host)
  )

# -----------------------------
# 5) Relative time (nearest win; bin at ±16)
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
# 6) Run fixest for each feature
# -----------------------------

all_coefs <- list()

for (f in available_features) {
  cat("========================================\n")
  cat("  Feature:", f$label, "\n")
  cat("========================================\n")
  
  # Prepare feature-specific columns
  df_f <- df %>%
    group_by(country) %>%
    arrange(tq, .by_group = TRUE) %>%
    mutate(
      dy    = .data[[f$yoy]],
      ln_l4 = lag(log(.data[[f$level]]), 4)
    ) %>%
    ungroup() %>%
    filter(!is.na(dy), !is.na(ln_l4), !is.na(host), !is.na(rel_time_bin))
  
  cat("  Complete cases:", nrow(df_f), "\n")
  cat("  Countries:", n_distinct(df_f$country), "\n")
  
  # Estimate
  m <- feols(
    dy ~ i(rel_time_bin, winner, ref = 0) + host + ln_l4 | country + tq,
    cluster = ~country,
    data = df_f
  )
  
  cat("  N obs:", nobs(m), "\n")
  
  # Extract coefficients
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  
  coef_tbl <- ct %>%
    transmute(
      feature = f$name,
      feature_label = f$label,
      term,
      estimate = Estimate,
      se = `Std. Error`,
      tval = `t value`,
      pval = `Pr(>|t|)`
    ) %>%
    filter(term %in% c("host", "ln_l4") | str_detect(term, "rel_time_bin::")) %>%
    mutate(
      l = if_else(str_detect(term, "rel_time_bin::"),
                  as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
                  NA_integer_)
    ) %>%
    arrange(l)
  
  all_coefs[[f$name]] <- coef_tbl
  
  # Print
  cat("\n  Coefficients (event-time dummies):\n")
  event_coefs <- coef_tbl %>% filter(!is.na(l))
  print(event_coefs %>% select(l, estimate, se, pval), n = Inf)
  cat("\n  Controls:\n")
  print(coef_tbl %>% filter(is.na(l)) %>% select(term, estimate, se), n = Inf)
  cat("\n")
}

# Combine all results
results_df <- bind_rows(all_coefs)

# Save
write_csv(results_df, "mello_paper_replication/results/event_study_all_features_coefficients.csv")
cat("\nSaved: mello_paper_replication/results/event_study_all_features_coefficients.csv\n")
cat("  Total rows:", nrow(results_df), "\n\n")

# Print summary table per feature (just the event-time dummies)
cat("════════════════════════════════════════════════════════════\n")
cat("  SUMMARY: ATT estimates at l = {-1, +1, +2, +4, +8}\n")
cat("════════════════════════════════════════════════════════════\n\n")

key_leads <- c(-1, 1, 2, 4, 8)
summary_tbl <- results_df %>%
  filter(l %in% key_leads) %>%
  mutate(
    stars = case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.10 ~ "*",
      TRUE ~ ""
    ),
    est_str = sprintf("%.3f%s", estimate, stars)
  ) %>%
  select(feature_label, l, est_str) %>%
  pivot_wider(names_from = l, values_from = est_str, names_prefix = "l_")

print(as.data.frame(summary_tbl), row.names = FALSE)


# ============================================================
# 7) FIGURE 2 REPLICATION: 5-panel event study plot
#    (GDP components: private consumption, government consumption,
#     capital formation, exports, imports)
# ============================================================

# Prepare plotting data (event-time dummies only, including l=0 as the reference)
plot_features <- c("private_consumption", "government_consumption",
                   "capital_formation", "exports", "imports")

plot_data <- results_df %>%
  filter(feature %in% plot_features, !is.na(l)) %>%
  mutate(
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  ) %>%
  # Add the reference point (l = 0, estimate = 0)
  bind_rows(
    tibble(
      feature = rep(plot_features, each = 1),
      feature_label = c("Private consumption", "Government consumption",
                        "Capital formation", "Exports", "Imports"),
      l = 0L,
      estimate = 0,
      se = 0,
      ci_lo = 0,
      ci_hi = 0
    )
  ) %>%
  # Order features to match the paper figure
  mutate(
    feature_label = factor(feature_label, levels = c(
      "Private consumption", "Government consumption",
      "Capital formation", "Exports", "Imports"
    ))
  )

# Define colors matching the paper
feature_colors <- c(
  "Private consumption"     = "#E69F00",   # orange
  "Government consumption"  = "#9467BD",   # purple
  "Capital formation"       = "#1F77B4",   # blue
  "Exports"                 = "#2CA02C",   # green
  "Imports"                 = "#D62728"    # red
)

p <- ggplot(plot_data, aes(x = l, y = estimate)) +
  # Horizontal zero line
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  # Vertical dashed line at l=0 (Post World Cup)
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  # Confidence intervals
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, color = feature_label),
                width = 0, linewidth = 0.4, alpha = 0.7) +
  # Point estimates
  geom_point(aes(color = feature_label), size = 1.8) +
  # "Post World Cup" label in each facet
  geom_text(data = data.frame(
    feature_label = factor(
      c("Private consumption", "Government consumption",
        "Capital formation", "Exports", "Imports"),
      levels = c("Private consumption", "Government consumption",
                 "Capital formation", "Exports", "Imports")
    ),
    l = 3, estimate = Inf
  ), aes(label = "Post World Cup"), vjust = 1.5, hjust = 0.3, size = 2.8, color = "grey30") +
  # Facets
  facet_wrap(~ feature_label, ncol = 1, scales = "free_y") +
  # Scales
  scale_x_continuous(breaks = seq(-16, 16, 2)) +
  scale_color_manual(values = feature_colors, guide = "none") +
  # Labels
  labs(
    x = "Quarter to or from the World Cup",
    y = "ATT"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 10, 5, 5)
  )

# Save
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_components_figure2.pdf",
       p, width = 7, height = 12, dpi = 300)
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_components_figure2.png",
       p, width = 7, height = 12, dpi = 300)

cat("\nPlot saved to event_study_plots/event_study_gdp_components_figure2.pdf\n")
cat("Plot saved to event_study_plots/event_study_gdp_components_figure2.png\n")


# ============================================================
# 8) Also make the GDP-only event study plot (Figure 1 equivalent)
# ============================================================

gdp_plot_data <- results_df %>%
  filter(feature == "gdp", !is.na(l)) %>%
  mutate(
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  ) %>%
  bind_rows(
    tibble(feature = "gdp", feature_label = "GDP", l = 0L,
           estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
  )

p_gdp <- ggplot(gdp_plot_data, aes(x = l, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), color = "grey30",
                width = 0, linewidth = 0.4) +
  geom_point(color = "black", size = 2) +
  annotate("text", x = 3, y = Inf, label = "Post World Cup",
           vjust = 1.5, hjust = 0.3, size = 3, color = "grey30") +
  scale_x_continuous(breaks = seq(-16, 16, 2)) +
  labs(
    title = "GDP",
    x = "Quarter to or from the World Cup",
    y = "ATT"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("mello_paper_replication/event_study_plots/event_study_gdp_figure1.pdf",
       p_gdp, width = 7, height = 4, dpi = 300)
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_figure1.png",
       p_gdp, width = 7, height = 4, dpi = 300)

cat("Plot saved to event_study_plots/event_study_gdp_figure1.pdf\n")


cat("\n\nDONE.\n")

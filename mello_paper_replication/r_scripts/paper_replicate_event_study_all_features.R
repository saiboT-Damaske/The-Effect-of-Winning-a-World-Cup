# paper_replicate_event_study_all_features.R
# Runs the Mello (2024) event study for all 6 GDP components (fixest only), produces Figure 1 and Figure 2.

#######################################################
# 0) Setup
#######################################################
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(ggplot2)

#######################################################
# 1) Load data
#######################################################
df0 <- read_csv(
  "Data/mello_paper_replication/paper_replication_sample.csv",
  show_col_types = FALSE
)

cat("Loaded:", nrow(df0), "rows,", n_distinct(df0$country), "countries\n")

#######################################################
# 2) Feature definitions
#######################################################
# each feature needs a YoY growth column (dependent var) and a lagged level (control)
features <- list(
  list(name  = "gdp",
       yoy   = "gdp_yoy_log_4q",
       lag   = "ln_gdp_l4",
       label = "GDP"),
  list(name  = "private_consumption",
       yoy   = "private_consumption_yoy_log_4q",
       lag   = "ln_private_consumption_l4",
       label = "Private consumption"),
  list(name  = "government_consumption",
       yoy   = "government_consumption_yoy_log_4q",
       lag   = "ln_government_consumption_l4",
       label = "Government consumption"),
  list(name  = "capital_formation",
       yoy   = "capital_formation_yoy_log_4q",
       lag   = "ln_capital_formation_l4",
       label = "Capital formation"),
  list(name  = "exports",
       yoy   = "exports_yoy_log_4q",
       lag   = "ln_exports_l4",
       label = "Exports"),
  list(name  = "imports",
       yoy   = "imports_yoy_log_4q",
       lag   = "ln_imports_l4",
       label = "Imports")
)

# quick check that all columns exist
for (f in features) {
  ok_yoy <- f$yoy %in% names(df0)
  ok_lag <- f$lag %in% names(df0)
  cat(sprintf("  %s: yoy=%s lag=%s\n", f$label, ok_yoy, ok_lag))
}

#######################################################
# 3) Winner events and country indicator
#######################################################
win_events <- df0 %>%
  filter(winner == 1) %>%
  select(country, year, qtr, tq) %>%
  rename(tq_event = tq)

cat("\nWin events:", nrow(win_events), "\n")

winner_countries <- unique(win_events$country)
cat("Winner countries:", paste(winner_countries, collapse = ", "), "\n")

df <- df0 %>%
  mutate(
    is_winner = as.integer(country %in% winner_countries),
    host      = as.integer(host)
  )

#######################################################
# 4) Relative time (nearest win, binned at +-16)
#######################################################
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
  ungroup() %>%
  mutate(
    rel_time_bin = case_when(
      rel_time <= -16 ~ -16L,
      rel_time >=  16 ~  16L,
      TRUE            ~ as.integer(rel_time)
    )
  )

#######################################################
# 5) Loop over features and estimate
#######################################################
all_coefs <- list()

for (f in features) {
  cat("\n========================================\n")
  cat("  Feature:", f$label, "\n")
  cat("========================================\n")

  # complete cases only — different features can have different coverage
  df_f <- df %>%
    rename(dy = !!f$yoy, ln_l4 = !!f$lag) %>%
    filter(!is.na(dy), !is.na(ln_l4), !is.na(host), !is.na(rel_time_bin))

  cat("  N =", nrow(df_f), "  Countries =", n_distinct(df_f$country), "\n")

  # Estimate
  m <- feols(
    dy ~ i(rel_time_bin, is_winner, ref = 0) + host + ln_l4
       | country + tq,
    cluster = ~country,
    data    = df_f
  )

  cat("  Within R2 =", round(fitstat(m, "wr2")[[1]], 4), "\n")

  # Extract coefficients
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL

  coef_tbl <- ct %>%
    transmute(
      feature       = f$name,
      feature_label = f$label,
      term,
      estimate = Estimate,
      se       = `Std. Error`,
      tval     = `t value`,
      pval     = `Pr(>|t|)`
    ) %>%
    filter(term %in% c("host", "ln_l4") | str_detect(term, "rel_time_bin::")) %>%
    mutate(
      l = if_else(
        str_detect(term, "rel_time_bin::"),
        as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
        NA_integer_
      )
    ) %>%
    arrange(l)

  all_coefs[[f$name]] <- coef_tbl

  # Print summary
  cat("  Controls: host =", round(coef_tbl$estimate[coef_tbl$term == "host"], 3),
      "  ln_l4 =", round(coef_tbl$estimate[coef_tbl$term == "ln_l4"], 3), "\n")
  cat("  l=+1:", round(coef_tbl$estimate[coef_tbl$l == 1 & !is.na(coef_tbl$l)], 3),
      "  l=+2:", round(coef_tbl$estimate[coef_tbl$l == 2 & !is.na(coef_tbl$l)], 3), "\n")

  df <- df  # restore original names for next iteration
}

# Combine and save
results_df <- bind_rows(all_coefs)
results_df
write_csv(results_df, "mello_paper_replication/results/event_study_all_features_coefficients.csv")
cat("\nSaved: mello_paper_replication/results/event_study_all_features_coefficients.csv\n")
cat("  Total rows:", nrow(results_df), "\n\n")

#######################################################
# 6) Summary table
#######################################################
cat("============================================================\n")
cat("  SUMMARY: ATT estimates at l = {-1, +1, +2, +4, +8}\n")
cat("============================================================\n\n")

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

#######################################################
# 7) Figure 1: GDP-only event study plot
#######################################################
# add back l=0 as the reference point for the plot
gdp_plot_data <- results_df %>%
  filter(feature == "gdp", !is.na(l)) %>%
  mutate(ci_lo = estimate - 1.96 * se,
         ci_hi = estimate + 1.96 * se) %>%
  bind_rows(
    tibble(feature = "gdp", feature_label = "GDP",
           l = 0L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
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
  labs(title = "GDP", x = "Quarter to or from the World Cup", y = "ATT") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave("mello_paper_replication/event_study_plots/event_study_gdp_figure1.pdf",
       p_gdp, width = 7, height = 4, dpi = 300)
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_figure1.png",
       p_gdp, width = 7, height = 4, dpi = 300)
cat("\nPlot saved: event_study_plots/event_study_gdp_figure1.{pdf,png}\n")

#######################################################
# 8) Figure 2: 5-panel GDP components
#######################################################
plot_features <- c("private_consumption", "government_consumption",
                   "capital_formation", "exports", "imports")

plot_data <- results_df %>%
  filter(feature %in% plot_features, !is.na(l)) %>%
  mutate(ci_lo = estimate - 1.96 * se,
         ci_hi = estimate + 1.96 * se) %>%
  bind_rows(
    tibble(
      feature = rep(plot_features, each = 1),
      feature_label = c("Private consumption", "Government consumption",
                        "Capital formation", "Exports", "Imports"),
      l = 0L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0
    )
  ) %>%
  mutate(
    feature_label = factor(feature_label, levels = c(
      "Private consumption", "Government consumption",
      "Capital formation", "Exports", "Imports"
    ))
  )

feature_colors <- c(
  "Private consumption"     = "#E69F00",
  "Government consumption"  = "#9467BD",
  "Capital formation"       = "#1F77B4",
  "Exports"                 = "#2CA02C",
  "Imports"                 = "#D62728"
)

p <- ggplot(plot_data, aes(x = l, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, color = feature_label),
                width = 0, linewidth = 0.4, alpha = 0.7) +
  geom_point(aes(color = feature_label), size = 1.8) +
  geom_text(data = data.frame(
    feature_label = factor(
      plot_features %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_sentence(),
      levels = c("Private consumption", "Government consumption",
                 "Capital formation", "Exports", "Imports")
    ),
    l = 3, estimate = Inf
  ), aes(label = "Post World Cup"), vjust = 1.5, hjust = 0.3, size = 2.8, color = "grey30") +
  facet_wrap(~ feature_label, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-16, 16, 2)) +
  scale_color_manual(values = feature_colors, guide = "none") +
  labs(x = "Quarter to or from the World Cup", y = "ATT") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 10, 5, 5)
  )
p
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_components_figure2.pdf",
       p, width = 7, height = 12, dpi = 300)
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_components_figure2.png",
       p, width = 7, height = 12, dpi = 300)

cat("Plot saved: event_study_plots/event_study_gdp_components_figure2.{pdf,png}\n")

cat("\n\nDONE.\n")
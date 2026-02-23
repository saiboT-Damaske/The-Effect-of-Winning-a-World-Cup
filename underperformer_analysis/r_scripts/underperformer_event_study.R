# ============================================================
# Underperformer Event Study — All features (log YoY)
# Treatment: top-10 ELO team eliminated in WC group stage
# Same specification as Mello (2024) replication event study
#
# Input:  underperformer_analysis/results/underperformer_sample.csv
#         (produced by prepare_underperformer_sample.R)
# Output: underperformer_analysis/results/underperformer_event_study_coefficients_<feature>.csv
#         underperformer_analysis/plots/underperformer_event_study_<feature>.png
# ============================================================

rm(list = ls())

# ---------- 0) Packages ----------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(ggplot2)

# ---------- 1) Load augmented panel ----------
df0 <- read_csv(
  "underperformer_analysis/results/underperformer_sample.csv",
  show_col_types = FALSE
)

cat("Loaded:", nrow(df0), "rows,", n_distinct(df0$country), "countries\n")

# ---------- 2) Identify underperformer events ----------
up_events <- df0 %>%
  filter(underperformer == 1) %>%
  select(country, year, qtr, tq) %>%
  rename(tq_event = tq) %>%
  distinct()

up_countries <- sort(unique(up_events$country))
cat("\nUnderperformer countries:", paste(up_countries, collapse = ", "), "\n")
cat("Number of countries:", length(up_countries), "\n")
cat("Number of events:", nrow(up_events), "\n")

cat("\nUnderperformer events:\n")
print(as.data.frame(up_events))

# ---------- 3) Assign relative time to nearest underperformer event ----------
assign_nearest_event <- function(tq_vec, event_tq_vec) {
  sapply(tq_vec, function(tq0) {
    diffs <- tq0 - event_tq_vec
    diffs[which.min(abs(diffs))]
  })
}

df <- df0 %>%
  mutate(
    is_underperformer = ever_underperformer,
    host = as.integer(host)
  )

df <- df %>%
  group_by(country) %>%
  group_modify(~ {
    ctry <- .y$country[[1]]
    ev <- up_events %>% filter(country == ctry) %>% pull(tq_event)
    if (length(ev) > 0) {
      .x$rel_time <- assign_nearest_event(.x$tq, ev)
    } else {
      .x$rel_time <- 0L
    }
    .x
  }) %>%
  ungroup()

# Bin endpoints at ±16
df <- df %>%
  mutate(
    rel_time_bin = case_when(
      rel_time <= -16 ~ -16L,
      rel_time >=  16 ~  16L,
      TRUE            ~ as.integer(rel_time)
    )
  )

# ---------- 4) Define features ----------
features <- list(
  list(name = "GDP",                       col = "gdp_yoy_log_4q",
       lag_col = "ln_gdp_l4",             file_tag = "gdp"),
  list(name = "Private consumption",       col = "private_consumption_yoy_log_4q",
       lag_col = "ln_private_consumption_l4", file_tag = "private_consumption"),
  list(name = "Government consumption",    col = "government_consumption_yoy_log_4q",
       lag_col = "ln_government_consumption_l4", file_tag = "government_consumption"),
  list(name = "Gross fixed capital formation", col = "capital_formation_yoy_log_4q",
       lag_col = "ln_capital_formation_l4", file_tag = "capital_formation"),
  list(name = "Exports",                   col = "exports_yoy_log_4q",
       lag_col = "ln_exports_l4",          file_tag = "exports"),
  list(name = "Imports",                   col = "imports_yoy_log_4q",
       lag_col = "ln_imports_l4",          file_tag = "imports")
)

# ---------- 5) Loop over features ----------
all_results <- list()

for (f in features) {
  cat("\n\n================================================\n")
  cat("Feature:", f$name, "\n")
  cat("================================================\n")

  # Rename columns for formula
  df_feat <- df %>%
    rename(dy = !!f$col, lag_level = !!f$lag_col)

  # Complete-cases sample
  df_cc <- df_feat %>%
    filter(!is.na(dy), !is.na(lag_level), !is.na(host), !is.na(rel_time_bin))

  cat("  Observations:", nrow(df_cc), "\n")
  cat("  Countries:   ", n_distinct(df_cc$country), "\n")
  n_treat_obs <- sum(df_cc$is_underperformer == 1)
  n_treat_ctry <- n_distinct(df_cc$country[df_cc$is_underperformer == 1])
  cat("  Treated obs: ", n_treat_obs, "from", n_treat_ctry, "countries\n")

  # Estimate
  m <- feols(
    dy ~ i(rel_time_bin, is_underperformer, ref = 0) + host + lag_level
       | country + tq,
    cluster = ~country,
    data    = df_cc
  )

  cat("\n  Within R²:", fitstat(m, "wr2")[[1]], "\n")

  # Extract coefficient table
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL

  tab <- ct %>%
    transmute(
      term,
      estimate = Estimate,
      se       = `Std. Error`,
      tval     = `t value`,
      pval     = `Pr(>|t|)`
    ) %>%
    filter(term %in% c("host", "lag_level") |
           str_detect(term, "rel_time_bin::")) %>%
    mutate(
      l = if_else(
        str_detect(term, "rel_time_bin::"),
        as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
        NA_integer_
      ),
      sort_key = case_when(
        term == "lag_level" ~ -999L,
        term == "host"      ~ -998L,
        TRUE                ~ l
      )
    ) %>%
    arrange(sort_key) %>%
    select(term, l, estimate, se, tval, pval) %>%
    mutate(feature = f$name)

  # Print key coefficients
  for (ll in c(-2, -1, 1, 2, 3, 4)) {
    row <- tab %>% filter(l == ll)
    if (nrow(row) == 1) {
      sig <- ifelse(row$pval < 0.01, "***",
             ifelse(row$pval < 0.05, "**",
             ifelse(row$pval < 0.10, "*", "")))
      cat(sprintf("  l=%+d: %.3f (%.3f) p=%.3f %s\n",
                  ll, row$estimate, row$se, row$pval, sig))
    }
  }

  # Save coefficients
  out_csv <- paste0("underperformer_analysis/results/underperformer_event_study_coefficients_",
                    f$file_tag, ".csv")
  write_csv(tab, out_csv)
  cat("  Saved:", out_csv, "\n")

  all_results[[f$name]] <- tab
}

# ---------- 6) Combined summary table ----------
combined <- bind_rows(all_results)
write_csv(combined, "underperformer_analysis/results/underperformer_event_study_all_features.csv")
cat("\nSaved: underperformer_analysis/results/underperformer_event_study_all_features.csv\n")

# ---------- 7) Combined 6-panel event study plot ----------
# Mirror the Mello replication Figure 2 style: faceted, coloured by feature
feature_labels <- c(
  "GDP"                            = "GDP",
  "Private consumption"            = "Private consumption",
  "Government consumption"         = "Government consumption",
  "Gross fixed capital formation"  = "Capital formation",
  "Exports"                        = "Exports",
  "Imports"                        = "Imports"
)

feature_colors <- c(
  "GDP"                     = "black",
  "Private consumption"     = "#E69F00",
  "Government consumption"  = "#9467BD",
  "Capital formation"       = "#1F77B4",
  "Exports"                 = "#2CA02C",
  "Imports"                 = "#D62728"
)

plot_data <- combined %>%
  filter(!is.na(l)) %>%
  mutate(
    feature_label = recode(feature, !!!feature_labels),
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  ) %>%
  bind_rows(
    tibble(
      feature_label = c("GDP", "Private consumption", "Government consumption",
                        "Capital formation", "Exports", "Imports"),
      l = 0L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0
    )
  ) %>%
  mutate(
    feature_label = factor(feature_label, levels = c(
      "GDP", "Private consumption", "Government consumption",
      "Capital formation", "Exports", "Imports"
    ))
  )

# "Post World Cup" annotation data — one per facet
annot_df <- data.frame(
  feature_label = factor(
    c("GDP", "Private consumption", "Government consumption",
      "Capital formation", "Exports", "Imports"),
    levels = levels(plot_data$feature_label)
  ),
  l = 3, estimate = Inf
)

p_all <- ggplot(plot_data, aes(x = l, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, color = feature_label),
                width = 0, linewidth = 0.4, alpha = 0.7) +
  geom_point(aes(color = feature_label), size = 1.8) +
  geom_text(data = annot_df,
            aes(label = "Post World Cup"),
            vjust = 1.5, hjust = 0.3, size = 2.8, color = "grey30") +
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

ggsave("underperformer_analysis/plots/underperformer_event_study_all_features.png",
       p_all, width = 7, height = 14, dpi = 300)
ggsave("underperformer_analysis/plots/underperformer_event_study_all_features.pdf",
       p_all, width = 7, height = 14, dpi = 300)
cat("Saved: underperformer_analysis/plots/underperformer_event_study_all_features.{png,pdf}\n")

# ---------- 8) Summary ----------
cat("\n========================================\n")
cat("  UNDERPERFORMER EVENT STUDY SUMMARY\n")
cat("========================================\n")
cat("  N countries treated:", length(up_countries), "\n")
cat("  Countries:", paste(up_countries, collapse = ", "), "\n")
cat("  N underperformance events:", nrow(up_events), "\n")

for (f in features) {
  tab <- all_results[[f$name]]
  l1 <- tab %>% filter(l == 1)
  l2 <- tab %>% filter(l == 2)
  if (nrow(l1) == 1 && nrow(l2) == 1) {
    cat(sprintf("  %-35s  l=+1: %6.3f (%5.3f)  l=+2: %6.3f (%5.3f)\n",
                f$name, l1$estimate, l1$se, l2$estimate, l2$se))
  }
}
cat("========================================\n")

cat("\nDONE.\n")

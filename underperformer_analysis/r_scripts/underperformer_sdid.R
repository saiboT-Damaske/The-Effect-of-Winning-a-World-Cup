# ============================================================
# SDiD: Underperformer Treatment — All features
# Treatment: top-10 ELO team eliminated in WC group stage
# Same stacked panel design as paper_replicate_sdid_full_features.R
#
# Input:  underperformer_analysis/results/underperformer_sample.csv
#         (produced by prepare_underperformer_sample.R)
# Output: underperformer_analysis/results/underperformer_sdid_results.csv
#         underperformer_analysis/plots/underperformer_sdid_<feature>.png
# ============================================================

rm(list = ls())

# ---------- 0) Packages ----------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(synthdid)

# ---------- 1) Load augmented panel ----------
df <- read_csv(
  "underperformer_analysis/results/underperformer_sample.csv",
  show_col_types = FALSE
) %>%
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

cat("Loaded:", nrow(df), "rows,", n_distinct(df$country), "countries\n")

# ---------- 2) SDiD event years (1998--2018) ----------
wc_years <- c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L)

events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)
)

# ---------- 3) Underperformer treatment from augmented panel ----------
# Build lookup: countries that underperformed at specific WC years
up_lookup <- df %>%
  filter(underperformer == 1) %>%
  select(country, year) %>%
  distinct() %>%
  mutate(wc_year = as.integer(year)) %>%
  select(country, wc_year)

cat("\nUnderperformer events (from panel, all years):\n")
print(as.data.frame(up_lookup %>% arrange(wc_year, country)))

# Filter to SDiD-eligible years (1998-2018)
up_lookup_sdid <- up_lookup %>%
  filter(wc_year %in% wc_years)

cat("\nUnderperformer events in SDiD window (1998-2018):\n")
print(as.data.frame(up_lookup_sdid %>% arrange(wc_year, country)))
cat("Total:", nrow(up_lookup_sdid), "events from",
    n_distinct(up_lookup_sdid$country), "countries\n")

is_treated_subseries <- function(country, wc_year) {
  any(up_lookup_sdid$country == country &
      up_lookup_sdid$wc_year == wc_year)
}

# ---------- 4) Core builder: stacked 10-quarter subseries ----------
build_sdid_mats <- function(df_in, y_col, drop_host_only_controls = TRUE) {

  stopifnot(y_col %in% names(df_in))

  d <- df_in %>%
    select(country, host, tq_num, !!y_col) %>%
    rename(y = !!y_col) %>%
    tidyr::crossing(events) %>%
    mutate(
      rel_time = as.integer(tq_num - event_tq),
      unit_id  = paste(country, wc_year, sep = "_"),
      treated  = as.integer(mapply(is_treated_subseries, country, wc_year)),
      post     = as.integer(rel_time >= 1L),
      D        = as.integer(treated * post)
    ) %>%
    filter(rel_time >= -7L, rel_time <= 2L)

  if (drop_host_only_controls) {
    d <- d %>%
      group_by(unit_id) %>%
      mutate(host_subseries = max(host, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!(host_subseries == 1L & treated == 0L)) %>%
      select(-host_subseries)
  }

  d <- d %>%
    group_by(unit_id) %>%
    filter(
      n_distinct(rel_time) == 10L,
      all(!is.na(y))
    ) %>%
    ungroup()

  cat("\n--- Sample diagnostics for", y_col, "---\n")
  cat("Units:", n_distinct(d$unit_id), "\n")
  n_treated <- d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow()
  n_control <- d %>% distinct(unit_id, treated) %>% filter(treated == 0L) %>% nrow()
  cat("Treated units:", n_treated, "\n")
  cat("Control units:", n_control, "\n")
  cat("Treatment-path variation (D):\n")
  print(table(d$D))

  if (n_treated == 0) {
    warning("No treated subseries left for ", y_col, " — skipping.")
    return(NULL)
  }

  time_levels <- seq(-7L, 2L, by = 1L)

  Y_wide <- d %>%
    mutate(rel_time = factor(rel_time, levels = time_levels)) %>%
    select(unit_id, treated, rel_time, y) %>%
    distinct(unit_id, treated, rel_time, .keep_all = TRUE) %>%
    tidyr::pivot_wider(names_from = rel_time, values_from = y)

  unit_order <- Y_wide %>%
    distinct(unit_id, treated) %>%
    arrange(treated, unit_id)

  Y_wide <- Y_wide %>%
    right_join(unit_order, by = c("unit_id", "treated")) %>%
    arrange(treated, unit_id)

  time_cols <- as.character(time_levels)
  Y <- as.matrix(Y_wide[, time_cols])

  stopifnot(!anyNA(Y))
  treated_vec <- unit_order$treated
  stopifnot(length(unique(treated_vec)) > 1)

  N0 <- sum(treated_vec == 0L)
  T0 <- sum(time_levels <= 0L)

  list(d = d, Y = Y, N0 = N0, T0 = T0, time_levels = time_levels)
}

# ---------- 5) Runner: estimate ATT, bootstrap SE, p-value, plot ----------
run_one_outcome <- function(df_in, outcome_name, y_col, file_tag, reps = 1000) {

  cat("\n================================================\n")
  cat("Outcome:", outcome_name, "\nColumn:", y_col, "\n")

  mats <- build_sdid_mats(df_in, y_col)
  if (is.null(mats)) return(NULL)

  tau_hat <- synthdid::synthdid_estimate(mats$Y, mats$N0, mats$T0)

  V <- stats::vcov(tau_hat, method = "bootstrap", replications = reps)
  se_hat <- sqrt(V[1, 1])

  ATT <- as.numeric(tau_hat)
  z   <- ATT / se_hat
  p   <- 2 * (1 - stats::pnorm(abs(z)))

  cat("\n--- Metrics ---\n")
  cat(sprintf("ATT     = %.3f\n", ATT))
  cat(sprintf("SE      = %.3f (bootstrap, replications=%d)\n", se_hat, reps))
  cat(sprintf("z       = %.3f\n", z))
  cat(sprintf("p-value = %.3f\n", p))

  # Plot
  p_obj <- synthdid::synthdid_plot(tau_hat)
  color_vals <- c("synthetic control" = "#4269d0", "treated" = "#ff585d")

  subtitle_text <- sprintf("ATT = %.3f, SE = %.3f, p = %.3f%s",
                           ATT, se_hat, p,
                           ifelse(p < 0.05, " *", ifelse(p < 0.10, " .", "")))

  out_png <- paste0("underperformer_analysis/plots/underperformer_sdid_", file_tag, ".png")

  if (inherits(p_obj, "ggplot")) {
    p_final <- p_obj +
      ggplot2::ggtitle(paste0("SDiD: Underperformance effect on YoY ",
                              outcome_name, " growth (pp)")) +
      ggplot2::labs(subtitle = subtitle_text,
                    x = "Quarter to/from World Cup (q = rel_time)",
                    y = "YoY growth (pp)") +
      ggplot2::scale_colour_manual(values = color_vals) +
      ggplot2::scale_fill_manual(values = color_vals)

    ggsave(out_png, p_final, width = 10, height = 6, dpi = 300)
  } else {
    png(out_png, width = 10, height = 6, units = "in", res = 300)
    plot(tau_hat)
    graphics::title(main = paste0("SDiD: Underperformance effect on YoY ",
                                  outcome_name, " growth (pp)"),
                    sub = subtitle_text)
    dev.off()
  }
  cat("Saved:", out_png, "\n")

  N1 <- mats$d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow()

  invisible(list(
    tau_hat = tau_hat, ATT = ATT, SE = se_hat, z = z, p = p,
    N_treated = N1, N_control = mats$N0
  ))
}

# ---------- 6) Run all features ----------
features <- list(
  list(name = "GDP",                         col = "gdp_yoy_log_4q",                    tag = "gdp"),
  list(name = "Private consumption",         col = "private_consumption_yoy_log_4q",     tag = "private_consumption"),
  list(name = "Government consumption",      col = "government_consumption_yoy_log_4q",  tag = "government_consumption"),
  list(name = "Gross fixed capital formation", col = "capital_formation_yoy_log_4q",      tag = "capital_formation"),
  list(name = "Exports",                     col = "exports_yoy_log_4q",                 tag = "exports"),
  list(name = "Imports",                     col = "imports_yoy_log_4q",                 tag = "imports")
)

results_list <- list()
for (f in features) {
  res <- run_one_outcome(df,
                         outcome_name = f$name,
                         y_col = f$col,
                         file_tag = f$tag,
                         reps = 1000)
  if (!is.null(res)) {
    results_list[[f$name]] <- res
  }
}

# ---------- 7) Save results ----------
if (length(results_list) > 0) {
  results_df <- tibble(
    Treatment = "Underperformer",
    Feature   = names(results_list),
    ATT       = sapply(results_list, function(x) x$ATT),
    SE        = sapply(results_list, function(x) x$SE),
    z         = sapply(results_list, function(x) x$z),
    p_value   = sapply(results_list, function(x) x$p),
    CI_lower  = sapply(results_list, function(x) x$ATT - 1.96 * x$SE),
    CI_upper  = sapply(results_list, function(x) x$ATT + 1.96 * x$SE),
    N_treated = sapply(results_list, function(x) x$N_treated),
    N_control = sapply(results_list, function(x) x$N_control)
  )

  write_csv(results_df, "underperformer_analysis/results/underperformer_sdid_results.csv")
  cat("\nSaved: underperformer_analysis/results/underperformer_sdid_results.csv\n")

  cat("\n========================================\n")
  cat("  UNDERPERFORMER SDiD SUMMARY\n")
  cat("========================================\n")
  for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    sig <- ifelse(r$p_value < 0.01, "***",
           ifelse(r$p_value < 0.05, "**",
           ifelse(r$p_value < 0.10, "*", "")))
    cat(sprintf("  %-35s  ATT = %7.3f  SE = %6.3f  p = %.3f %s\n",
                r$Feature, r$ATT, r$SE, r$p_value, sig))
  }
  cat("========================================\n")
} else {
  cat("\nNo features could be estimated (no treated subseries).\n")
}

cat("\nDone.\n")

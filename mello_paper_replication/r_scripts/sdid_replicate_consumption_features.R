# ============================================================
# SDiD replication — Private and Government Consumption only
# Paper: Mello (OBES) "A Kick for the GDP"
# Data: Data/mello_paper_replication/paper_replication_sample.csv
# ============================================================

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(synthdid)

# Load data

df <- read_csv("Data/mello_paper_replication/paper_replication_sample.csv", show_col_types = FALSE) %>%
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

wc_years <- c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L)

events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)
)

is_treated_subseries <- function(country, wc_year) {
  (country == "FRA" && wc_year %in% c(1998L, 2018L)) ||
    (country == "BRA" && wc_year == 2002L) ||
    (country == "ITA" && wc_year == 2006L) ||
    (country == "ESP" && wc_year == 2010L) ||
    (country == "DEU" && wc_year == 2014L)
}

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
  list(
    d = d,
    Y = Y,
    N0 = N0,
    T0 = T0,
    time_levels = time_levels
  )
}

run_one_outcome <- function(df_in, outcome_name, y_col, reps = 1000) {
  cat("\n================================================\n")
  cat("Outcome:", outcome_name, "\nColumn:", y_col, "\n")
  mats <- build_sdid_mats(df_in, y_col)
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

  # Plot and save (treated = red, synthetic control = blue)
  p_obj <- synthdid::synthdid_plot(tau_hat)
  plot_title <- paste0("SDiD: World Cup win effect on YoY ", outcome_name, " growth (pp)")
  plot_path <- paste0("mello_paper_replication/sdid_plots/sdid_plot_", outcome_name, ".png")
  color_vals <- c("synthetic control" = "#4269d0", "treated" = "#ff585d")
  if (inherits(p_obj, "ggplot")) {
    p_final <- p_obj +
      ggplot2::ggtitle(plot_title) +
      ggplot2::labs(x = "Quarter to/from World Cup (q = rel_time)",
                    y = "YoY growth (pp)") +
      ggplot2::scale_colour_manual(values = color_vals) +
      ggplot2::scale_fill_manual(values = color_vals)
    print(p_final)
    ggplot2::ggsave(plot_path, plot = p_final, width = 7, height = 5)
    cat("Plot saved to:", plot_path, "\n")
  } else {
    png(filename = plot_path, width = 700, height = 500)
    graphics::plot(p_obj)
    graphics::title(main = plot_title,
                   xlab = "Quarter to/from World Cup (q = rel_time)",
                   ylab = "YoY growth (pp)")
    dev.off()
    cat("Plot saved to:", plot_path, "\n")
  }

  invisible(list(ATT = ATT, SE = se_hat, CI_lower = ATT - 1.96 * se_hat, CI_upper = ATT + 1.96 * se_hat, z_stat = z, p_value = p))
}

private_res <- run_one_outcome(
  df,
  outcome_name = "Private_consumption",
  y_col = "private_consumption_yoy_log_4q",
  reps = 1000
)

gov_res <- run_one_outcome(
  df,
  outcome_name = "Government_consumption",
  y_col = "government_consumption_yoy_log_4q",
  reps = 1000
)

# Update sdid_results_summary.csv
summary_path <- "mello_paper_replication/sdid_results/sdid_results_summary.csv"
if (file.exists(summary_path)) {
  summary_df <- read_csv(summary_path)
} else {
  summary_df <- tibble(Outcome = character(), ATT_pp = numeric(), SE = numeric(), CI_lower = numeric(), CI_upper = numeric(), z_stat = numeric(), p_value = numeric())
}

private_row <- tibble(Outcome = "Private_consumption", ATT_pp = private_res$ATT, SE = private_res$SE, CI_lower = private_res$CI_lower, CI_upper = private_res$CI_upper, z_stat = private_res$z_stat, p_value = private_res$p_value)
gov_row     <- tibble(Outcome = "Government_consumption", ATT_pp = gov_res$ATT, SE = gov_res$SE, CI_lower = gov_res$CI_lower, CI_upper = gov_res$CI_upper, z_stat = gov_res$z_stat, p_value = gov_res$p_value)

# Remove old rows if present
summary_df <- summary_df %>%
  filter(!(Outcome %in% c("Private_consumption", "Government_consumption")))
summary_df <- bind_rows(summary_df, private_row, gov_row)
write_csv(summary_df, summary_path)
cat("\nUpdated summary written to:", summary_path, "\n")

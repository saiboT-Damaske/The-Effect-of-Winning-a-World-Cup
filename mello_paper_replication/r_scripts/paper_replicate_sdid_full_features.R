# ============================================================
# SDiD replication — GDP + components (Figure + ATT/SE/P-value)
# Paper: Mello (OBES) "A Kick for the GDP"
# Data: Data/mello_paper_replication/paper_replication_sample.csv
# Robust version: NO panel.matrices() (avoids false "no variation")
# ============================================================

rm(list = ls())

# -----------------------------
# 0) Packages
# -----------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# synthdid from GitHub:
# install.packages("remotes")
# remotes::install_github("synth-inference/synthdid")
library(synthdid)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# -----------------------------
# 1) Load data
# -----------------------------
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
    tq_num = 4L * year + (qtr - 1L)  # strictly increasing quarterly index
  )

# -----------------------------
# 2) SDiD event years and event quarter
#    World Cups used in SDiD: 1998, 2002, 2006, 2010, 2014, 2018
#    Set q=0 at Q2 of the WC year (paper convention)
# -----------------------------
wc_years <- c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L)

events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)   # Q2 index on tq_num scale
)

# Winners for those tournaments (treated subseries)
# NOTE: OECD codes: FRA, BRA, ITA, ESP, DEU
is_treated_subseries <- function(country, wc_year) {
  (country == "FRA" && wc_year %in% c(1998L, 2018L)) ||
    (country == "BRA" && wc_year == 2002L) ||
    (country == "ITA" && wc_year == 2006L) ||
    (country == "ESP" && wc_year == 2010L) ||
    (country == "DEU" && wc_year == 2014L)
}

# -----------------------------
# 3) Core builder: stacked 10-quarter subseries, then build Y/N0/T0
#    This is the robust replacement for panel.matrices().
# -----------------------------
build_sdid_mats <- function(df_in, y_col, drop_host_only_controls = TRUE) {
  
  stopifnot(y_col %in% names(df_in))
  
  # 3a) stack: each (country × wc_year) is one unit_id, keep q in [-7,2]
  d <- df_in %>%
    select(country, host, tq_num, !!y_col) %>%
    rename(y = !!y_col) %>%
    tidyr::crossing(events) %>%
    mutate(
      rel_time = as.integer(tq_num - event_tq),          # q in paper
      unit_id  = paste(country, wc_year, sep = "_"),
      treated  = as.integer(mapply(is_treated_subseries, country, wc_year)),
      post     = as.integer(rel_time >= 1L),
      D        = as.integer(treated * post)
    ) %>%
    filter(rel_time >= -7L, rel_time <= 2L)
  
  # 3b) drop host-only controls from donor pool (keep treated even if host)
  if (drop_host_only_controls) {
    d <- d %>%
      group_by(unit_id) %>%
      mutate(host_subseries = max(host, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!(host_subseries == 1L & treated == 0L)) %>%
      select(-host_subseries)
  }
  
  # 3c) enforce balanced panel in this window and NO missing outcomes
  # synthdid cannot handle NA in Y, so we must drop any unit with NA anywhere.
  d <- d %>%
    group_by(unit_id) %>%
    filter(
      n_distinct(rel_time) == 10L,
      all(!is.na(y))
    ) %>%
    ungroup()
  
  # Diagnostics (important with sparse components)
  cat("\n--- Sample diagnostics for", y_col, "---\n")
  cat("Units:", n_distinct(d$unit_id), "\n")
  cat("Treated units:", d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow(), "\n")
  cat("Control units:", d %>% distinct(unit_id, treated) %>% filter(treated == 0L) %>% nrow(), "\n")
  cat("Treatment-path variation (D):\n")
  print(table(d$D))
  cat("rel_time range:", paste(range(d$rel_time), collapse = " to "), "\n")
  
  # Hard stop if treatment units got dropped (common with NA-heavy components)
  if (d %>% distinct(unit_id, treated) %>% summarise(n = sum(treated)) %>% pull(n) == 0) {
    stop("No treated subseries left after NA filtering for this outcome.")
  }
  
  # 3d) build Y matrix: units x time, ordered
  # time order: -7,-6,...,2
  time_levels <- seq(-7L, 2L, by = 1L)
  
  Y_wide <- d %>%
    mutate(rel_time = factor(rel_time, levels = time_levels)) %>%
    select(unit_id, treated, rel_time, y) %>%
    distinct(unit_id, treated, rel_time, .keep_all = TRUE) %>%
    tidyr::pivot_wider(names_from = rel_time, values_from = y)
  
  # unit ordering: controls first, then treated (required by synthdid_estimate interface)
  unit_order <- Y_wide %>%
    distinct(unit_id, treated) %>%
    arrange(treated, unit_id)
  
  Y_wide <- Y_wide %>%
    right_join(unit_order, by = c("unit_id", "treated")) %>%
    arrange(treated, unit_id)
  
  # Extract numeric matrix in correct time column order
  time_cols <- as.character(time_levels)
  Y <- as.matrix(Y_wide[, time_cols])
  
  # Final checks
  stopifnot(!anyNA(Y))
  treated_vec <- unit_order$treated
  stopifnot(length(unique(treated_vec)) > 1)  # both 0 and 1 exist
  
  N0 <- sum(treated_vec == 0L)
  T0 <- sum(time_levels <= 0L)  # pre periods q<=0 => 8 (q=-7..0)
  
  list(
    d = d,
    Y = Y,
    N0 = N0,
    T0 = T0,
    time_levels = time_levels
  )
}

# -----------------------------
# 4) Runner: estimate ATT, bootstrap SE, p-value, plot
# -----------------------------
run_one_outcome <- function(df_in, outcome_name, y_col, reps = 1000,
                            output_dir = "mello_paper_replication/sdid_plots") {
  
  cat("\n================================================\n")
  cat("Outcome:", outcome_name, "\nColumn:", y_col, "\n")
  
  mats <- build_sdid_mats(df_in, y_col)
  
  tau_hat <- synthdid::synthdid_estimate(mats$Y, mats$N0, mats$T0)
  
  # Bootstrap SE via S3 vcov method for synthdid_estimate (called through stats::vcov)
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
  
  # Build output filename from outcome name
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  safe_name <- tolower(gsub("\\s+", "_", outcome_name))
  plot_path <- file.path(output_dir, paste0("sdid_replication_", safe_name, ".png"))
  
  # Plot (treated = red, synthetic control = blue)
  p_obj <- synthdid::synthdid_plot(tau_hat)
  color_vals <- c("synthetic control" = "#4269d0", "treated" = "#ff585d")
  if (inherits(p_obj, "ggplot")) {
    p_final <- p_obj +
      ggplot2::ggtitle(paste0("SDiD: World Cup win effect on YoY ", outcome_name, " growth (pp)")) +
      ggplot2::labs(x = "Quarter to/from World Cup (q = rel_time)",
                    y = "YoY growth (pp)") +
      ggplot2::scale_colour_manual(values = color_vals) +
      ggplot2::scale_fill_manual(values = color_vals)
    print(p_final)
    ggplot2::ggsave(plot_path, plot = p_final, width = 8, height = 5, dpi = 300)
  } else {
    # base graphics path — open a PNG device, re-draw, then close
    grDevices::png(plot_path, width = 2400, height = 1500, res = 300)
    synthdid::synthdid_plot(tau_hat)
    graphics::title(
      main = paste0("SDiD: World Cup win effect on YoY ", outcome_name, " growth (pp)"),
      xlab = "Quarter to/from World Cup (q = rel_time)",
      ylab = "YoY growth (pp)"
    )
    grDevices::dev.off()
  }
  cat(sprintf("Plot saved to: %s\n", plot_path))
  
  invisible(list(tau_hat = tau_hat, ATT = ATT, SE = se_hat, z = z, p = p))
}


# Define features and columns
features <- list(
  list(name = "GDP", col = "gdp_yoy_log_4q"),
  list(name = "Private consumption", col = "private_consumption_yoy_log_4q"),
  list(name = "Government consumption", col = "government_consumption_yoy_log_4q"),
  list(name = "Gross fixed capital formation", col = "capital_formation_yoy_log_4q"),
  list(name = "Exports", col = "exports_yoy_log_4q"),
  list(name = "Imports", col = "imports_yoy_log_4q")
)

# Run and collect results
results_list <- list()
for (f in features) {
  results_list[[f$name]] <- run_one_outcome(
    df,
    outcome_name = f$name,
    y_col = f$col,
    reps = 1000
  )
}

# Convert results to a data frame
results_df <- tibble(
  Feature = names(results_list),
  ATT = sapply(results_list, function(x) x$ATT),
  SE = sapply(results_list, function(x) x$SE),
  z = sapply(results_list, function(x) x$z),
  p_value = sapply(results_list, function(x) x$p),
  CI_lower = sapply(results_list, function(x) x$ATT - 1.96 * x$SE),
  CI_upper = sapply(results_list, function(x) x$ATT + 1.96 * x$SE)
)

# Write results to CSV
write_csv(results_df, "mello_paper_replication/sdid_results/sdid_results_full_features.csv")
cat("\nResults written to: mello_paper_replication/sdid_results/sdid_results_full_features.csv\n")
cat("\nDone.\n")




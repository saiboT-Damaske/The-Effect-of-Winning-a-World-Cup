# ============================================================
# SDiD: Finalist Treatment — GDP only
# Treatment: reaching the World Cup Final (rank1 OR rank2)
# Same stacked panel design as paper_replicate_sdid_full_features.R
# but with finalist treatment and GDP only.
#
# Input:  Data/mello_paper_replication/paper_replication_sample.csv
# Output: finalist_analysis/results/finalist_sdid_gdp_results.csv
#         finalist_analysis/plots/finalist_sdid_gdp.png
# ============================================================

rm(list = ls())

# ---------- 0) Packages ----------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# synthdid from GitHub:
# remotes::install_github("synth-inference/synthdid")
library(synthdid)

# ---------- 1) Load data ----------
df <- read_csv(
  "Data/mello_paper_replication/paper_replication_sample.csv",
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

# ---------- 3) Finalist treatment: reached WC final in that year ----------
# Build lookup from the data itself (rank1 or rank2 at Q2)
finalist_lookup <- df %>%
  filter(rank1 == 1 | rank2 == 1) %>%
  select(country, year) %>%
  distinct() %>%
  mutate(wc_year = as.integer(year)) %>%
  select(country, wc_year)

cat("\nFinalist events in data:\n")
print(as.data.frame(finalist_lookup))

is_finalist_subseries <- function(country, wc_year) {
  any(finalist_lookup$country == country &
      finalist_lookup$wc_year == wc_year)
}

# ---------- 4) Core builder: stacked 10-quarter subseries ----------
build_sdid_mats <- function(df_in, y_col, drop_host_only_controls = TRUE) {

  stopifnot(y_col %in% names(df_in))

  # Stack: each (country × wc_year) is one unit_id, keep q in [-7,2]
  d <- df_in %>%
    select(country, host, tq_num, !!y_col) %>%
    rename(y = !!y_col) %>%
    tidyr::crossing(events) %>%
    mutate(
      rel_time = as.integer(tq_num - event_tq),
      unit_id  = paste(country, wc_year, sep = "_"),
      treated  = as.integer(mapply(is_finalist_subseries, country, wc_year)),
      post     = as.integer(rel_time >= 1L),
      D        = as.integer(treated * post)
    ) %>%
    filter(rel_time >= -7L, rel_time <= 2L)

  # Drop host-only controls (keep treated even if host)
  if (drop_host_only_controls) {
    d <- d %>%
      group_by(unit_id) %>%
      mutate(host_subseries = max(host, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!(host_subseries == 1L & treated == 0L)) %>%
      select(-host_subseries)
  }

  # Enforce balanced panel and no missing outcomes
  d <- d %>%
    group_by(unit_id) %>%
    filter(
      n_distinct(rel_time) == 10L,
      all(!is.na(y))
    ) %>%
    ungroup()

  # Diagnostics
  cat("\n--- Sample diagnostics for", y_col, "---\n")
  cat("Units:", n_distinct(d$unit_id), "\n")
  cat("Treated units:", d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow(), "\n")
  cat("Control units:", d %>% distinct(unit_id, treated) %>% filter(treated == 0L) %>% nrow(), "\n")
  cat("Treatment-path variation (D):\n")
  print(table(d$D))

  if (d %>% distinct(unit_id, treated) %>% summarise(n = sum(treated)) %>% pull(n) == 0) {
    stop("No treated subseries left after NA filtering.")
  }

  # Build Y matrix
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

# ---------- 5) Estimate ATT, bootstrap SE, p-value, plot ----------
cat("\n================================================\n")
cat("SDiD: Finalist treatment — GDP (log YoY)\n")
cat("================================================\n")

mats <- build_sdid_mats(df, "gdp_yoy_log_4q")

tau_hat <- synthdid::synthdid_estimate(mats$Y, mats$N0, mats$T0)

# Bootstrap SE
V <- stats::vcov(tau_hat, method = "bootstrap", replications = 1000)
se_hat <- sqrt(V[1, 1])

ATT <- as.numeric(tau_hat)
z   <- ATT / se_hat
p   <- 2 * (1 - stats::pnorm(abs(z)))

cat("\n--- Metrics ---\n")
cat(sprintf("ATT     = %.3f\n", ATT))
cat(sprintf("SE      = %.3f (bootstrap, replications=1000)\n", se_hat))
cat(sprintf("z       = %.3f\n", z))
cat(sprintf("p-value = %.3f\n", p))

# Plot (treated = red, synthetic control = blue)
p_obj <- synthdid::synthdid_plot(tau_hat)
color_vals <- c("synthetic control" = "#4269d0", "treated" = "#ff585d")

subtitle_text <- sprintf("ATT = %.3f, SE = %.3f, p = %.3f%s",
                         ATT, se_hat, p, ifelse(p < 0.05, " *", ifelse(p < 0.10, " .", "")))

if (inherits(p_obj, "ggplot")) {
  p_final <- p_obj +
    ggplot2::ggtitle("SDiD: Finalist effect on YoY GDP growth (pp)") +
    ggplot2::labs(subtitle = subtitle_text,
                  x = "Quarter to/from World Cup (q = rel_time)",
                  y = "YoY growth (pp)") +
    ggplot2::scale_colour_manual(values = color_vals) +
    ggplot2::scale_fill_manual(values = color_vals)

  ggsave("finalist_analysis/plots/finalist_sdid_gdp.png",
         p_final, width = 10, height = 6, dpi = 300)
  cat("Saved: finalist_analysis/plots/finalist_sdid_gdp.png\n")
} else {
  png("finalist_analysis/plots/finalist_sdid_gdp.png",
      width = 10, height = 6, units = "in", res = 300)
  plot(tau_hat)
  graphics::title(main = "SDiD: Finalist effect on YoY GDP growth (pp)",
                  sub = subtitle_text)
  dev.off()
  cat("Saved: finalist_analysis/plots/finalist_sdid_gdp.png\n")
}

# Save results
N1 <- mats$d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow()

results_df <- tibble(
  Treatment = "Finalist",
  Feature   = "GDP",
  ATT       = ATT,
  SE        = se_hat,
  z         = z,
  p_value   = p,
  CI_lower  = ATT - 1.96 * se_hat,
  CI_upper  = ATT + 1.96 * se_hat,
  N_treated = N1,
  N_control = mats$N0
)

write_csv(results_df, "finalist_analysis/results/finalist_sdid_gdp_results.csv")
cat("Saved: finalist_analysis/results/finalist_sdid_gdp_results.csv\n")

cat("\nDone.\n")

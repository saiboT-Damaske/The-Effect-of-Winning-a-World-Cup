# ============================================================
# Replicate Figure 1 (SDiD) — Mello (OBES)
# Data: Data/paper_replication_event_study_sample.csv
# ============================================================

rm(list = ls())


# -----------------------------
# 0) Packages
# -----------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# synthdid from GitHub:
# install.packages("remotes")
# remotes::install_github("synth-inference/synthdid")
library(synthdid)

# -----------------------------
# 1) Load data
# -----------------------------
df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE) %>%
  mutate(
    country = as.character(country),
    year    = as.integer(year),
    qtr     = as.integer(qtr)
  )

# Outcome used in Figure 1: YoY GDP growth (percentage points)
y_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct"
stopifnot(y_col %in% names(df))

# Create a clean quarterly index (must be strictly increasing in time within country)
df <- df %>%
  mutate(tq_num = 4L * year + (qtr - 1L))

# -----------------------------
# 2) Define SDiD event years and event quarter
#    Paper SDiD uses World Cups: 1998, 2002, 2006, 2010, 2014, 2018
#    and sets q=0 at Q2 of the WC year
# -----------------------------
wc_years <- c(1998, 2002, 2006, 2010, 2014, 2018)

events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)   # Q2 index on tq_num scale
)

# -----------------------------
# 3) Create stacked 10-quarter subseries per country × WC year
#    Keep q in [-7, 2]
# -----------------------------
sdid_df <- df %>%
  select(country, year, qtr, tq_num, host, !!y_col) %>%
  rename(dy_gdp_pp = !!y_col) %>%
  tidyr::crossing(events) %>%
  mutate(
    rel_time = tq_num - event_tq,                 # q in the paper
    unit_id  = paste(country, wc_year, sep = "_") # subseries ID
  ) %>%
  filter(rel_time >= -7, rel_time <= 2)

# -----------------------------
# 4) Define treated subseries (winner × WC-year)
#    Winners in SDiD sample:
#      FRA-1998, BRA-2002, ITA-2006, ESP-2010, DEU-2014, FRA-2018
# -----------------------------
sdid_df <- sdid_df %>%
  mutate(
    treated = as.integer(
      (country == "FRA" & wc_year %in% c(1998, 2018)) |
        (country == "BRA" & wc_year == 2002) |
        (country == "ITA" & wc_year == 2006) |
        (country == "ESP" & wc_year == 2010) |
        (country == "DEU" & wc_year == 2014)
    )
  )

# -----------------------------
# 5) Drop host-only subseries from donor pool
#    Keep France-1998 (host+winner) automatically because treated==1
# -----------------------------
sdid_df <- sdid_df %>%
  group_by(unit_id) %>%
  mutate(host_subseries = max(host, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(host_subseries == 1L & treated == 0L))

# -----------------------------
# 6) Define SDiD treatment path:
#    D_it = 1 for treated units in post quarters (q >= 1), else 0
# -----------------------------
sdid_df <- sdid_df %>%
  mutate(
    post = as.integer(rel_time >= 1),
    D    = as.integer(treated * post)
  )

# -----------------------------
# 7) Hard requirements for synthdid:
#    - balanced 10-quarter panel per unit_id (q=-7..2)
#    - no missing outcomes in the estimation sample
# -----------------------------
sdid_df <- sdid_df %>%
  group_by(unit_id) %>%
  filter(
    n_distinct(rel_time) == 10,
    all(!is.na(dy_gdp_pp))
  ) %>%
  ungroup()

# Sanity checks
cat("\n--- Sanity checks ---\n")
print(table(sdid_df$treated))
print(table(sdid_df$D))
cat("Units:", n_distinct(sdid_df$unit_id), "\n")
cat("Treated units:", sdid_df %>% distinct(unit_id, treated) %>% filter(treated == 1) %>% nrow(), "\n")
cat("rel_time range:", paste(range(sdid_df$rel_time), collapse = " to "), "\n\n")

# -----------------------------
# 8) Critical FIX:
#    Convert tibble -> base data.frame and force atomic vectors
#    (avoids false 'no variation' errors in some setups)
# -----------------------------
sdid_df2 <- sdid_df %>%
  arrange(unit_id, rel_time) %>%
  mutate(
    unit_id  = as.character(unit_id),
    rel_time = as.integer(rel_time),
    dy_gdp_pp = as.numeric(dy_gdp_pp),
    D = as.integer(D)
  )

sdid_df2 <- as.data.frame(sdid_df2)

stopifnot(length(unique(sdid_df2$D)) > 1)

# -----------------------------
# 9) Build panel matrices + estimate SDiD
# -----------------------------
panel <- synthdid::panel.matrices(
  sdid_df2,
  unit = "unit_id",
  time = "rel_time",
  outcome = "dy_gdp_pp",
  treatment = "D"
)

tau_hat <- synthdid::synthdid_estimate(panel$Y, panel$N0, panel$T0)

# Bootstrap SE (paper uses bootstrap; 1000 is standard there)
V <- stats::vcov(tau_hat, method = "bootstrap", replications = 1000)
se_hat <- sqrt(V[1, 1])
se_hat

tau <- as.numeric(tau_hat)
c(lower = tau - 1.96 * se_hat, upper = tau + 1.96 * se_hat)



cat("\n--- SDiD estimate ---\n")
print(tau_hat)
cat("Bootstrap SE:", se_hat, "\n\n")

# -----------------------------
# 10) Plot (Figure 1 style)
# -----------------------------
# Base-graphics plot
synthdid::synthdid_plot(tau_hat)
title(main = "Synthetic DiD: Effect of winning the World Cup on YoY GDP growth",
      xlab = "Quarter to or from the World Cup (q = rel_time)",
      ylab = "YoY GDP growth (pp)")



p <- synthdid::synthdid_plot(tau_hat)

if (inherits(p, "ggplot")) {
  # ggplot path: must print explicitly, and use ggplot labels
  print(
    p +
      ggplot2::ggtitle("Synthetic DiD: Effect of winning the World Cup on YoY GDP growth") +
      ggplot2::labs(
        x = "Quarter to or from the World Cup (q = rel_time)",
        y = "YoY GDP growth (pp)"
      )
  )
} else {
  # base-graphics path: plot was drawn; now add title/labels
  graphics::title(
    main = "Synthetic DiD: Effect of winning the World Cup on YoY GDP growth",
    xlab = "Quarter to or from the World Cup (q = rel_time)",
    ylab = "YoY GDP growth (pp)"
  )
}


## METRICS
ATT <- as.numeric(tau_hat)
ATT


# V <- stats::vcov(tau_hat, method = "bootstrap", replications = 1000)
SE <- sqrt(V[1, 1])
SE


z_stat <- ATT / SE
p_value <- 2 * (1 - pnorm(abs(z_stat)))

c(
  ATT = ATT,
  SE  = SE,
  z   = z_stat,
  p_value = p_value
)



# control-unit weights (length N0)
omega <- synthdid::synthdid_controls(tau_hat)

# time weights (length T0)
lambda <- synthdid::synthdid_times(tau_hat)

# quick summaries
summary(omega)
summary(lambda)

# show top control weights
head(sort(omega, decreasing = TRUE), 20)

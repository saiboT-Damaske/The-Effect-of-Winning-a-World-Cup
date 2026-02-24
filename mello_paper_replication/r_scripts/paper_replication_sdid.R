# paper_replication_sdid.R
# Replicates the SDiD estimate from Mello (2024) Figure 1 for GDP growth.

rm(list = ls())

#######################################################
# 0) Packages
#######################################################
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# synthdid from GitHub:
# install.packages("remotes")
# remotes::install_github("synth-inference/synthdid")
library(synthdid)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

#######################################################
# 1) Load data
#######################################################
df <- read_csv("Data/mello_paper_replication/paper_replication_sample.csv", show_col_types = FALSE) %>%
  mutate(
    country = as.character(country),
    year    = as.integer(year),
    qtr     = as.integer(qtr)
  )

# Outcome: YoY GDP log growth in percentage points
y_col <- "gdp_yoy_log_4q"
stopifnot(y_col %in% names(df))

# quarterly index (must be strictly increasing within country)
df <- df %>%
  mutate(tq_num = 4L * year + (qtr - 1L))

#######################################################
# 2) SDiD event definitions
#######################################################
# paper uses WC 1998–2018, q=0 at Q2 of each WC year
wc_years <- c(1998, 2002, 2006, 2010, 2014, 2018)

events <- tibble(
  wc_year  = wc_years,
  event_tq = 4L * wc_year + (2L - 1L)   # Q2 index on tq_num scale
)

#######################################################
# 3) Stacked subseries (country x WC year)
#######################################################
# each subseries is 10 quarters: q in [-7, 2]
sdid_df <- df %>%
  select(country, year, qtr, tq_num, host, !!y_col) %>%
  rename(dy_gdp_pp = !!y_col) %>%
  tidyr::crossing(events) %>%
  mutate(
    rel_time = tq_num - event_tq,                 # q in the paper
    unit_id  = paste(country, wc_year, sep = "_") # subseries ID
  ) %>%
  filter(rel_time >= -7, rel_time <= 2)

#######################################################
# 4) Define treated subseries
#######################################################
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

#######################################################
# 5) Drop host-only controls from donor pool
#######################################################
# France 1998 stays because it's treated (host + winner)
sdid_df <- sdid_df %>%
  group_by(unit_id) %>%
  mutate(host_subseries = max(host, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(host_subseries == 1L & treated == 0L))

#######################################################
# 6) Treatment indicator D
#######################################################
# D_it = 1 for treated units in post periods (q >= 1)
sdid_df <- sdid_df %>%
  mutate(
    post = as.integer(rel_time >= 1),
    D    = as.integer(treated * post)
  )

#######################################################
# 7) Balanced panel, no missing outcomes
#######################################################
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

#######################################################
# 8) Convert to base data.frame
#######################################################
# tibble + vctrs can cause false "no variation" errors in synthdid
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

#######################################################
# 9) Build panel matrices + estimate SDiD
#######################################################
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

p <- synthdid::synthdid_plot(tau_hat)

#######################################################
# 10) Plot and save
#######################################################
plot_path <- "mello_paper_replication/sdid_plots/sdid_plot_GDP.png"

# Significance indicator
sig_str <- if (p_value < 0.05) {
  "*"
} else if (p_value < 0.10) {
  "."
} else {
  ""
}
subtitle_str <- sprintf("ATT = %.3f, p = %.3f %s", ATT, p_value, sig_str)

p <- synthdid::synthdid_plot(tau_hat)

if (inherits(p, "ggplot")) {
  ggplot2::ggsave(plot_path, plot = p +
    ggplot2::ggtitle("Synthetic DiD: Effect of winning the World Cup on YoY GDP growth") +
    ggplot2::labs(
      x = "Quarter to or from the World Cup (q = rel_time)",
      y = "YoY GDP growth (pp)",
      subtitle = subtitle_str
    ), width = 7, height = 5)
  cat("Plot saved to:", plot_path, "\n")
} else {
  png(plot_path, width = 700, height = 500)
  synthdid::synthdid_plot(tau_hat)
  graphics::title(
    main = "Synthetic DiD: Effect of winning the World Cup on YoY GDP growth",
    xlab = "Quarter to or from the World Cup (q = rel_time)",
    ylab = "YoY GDP growth (pp)",
    sub = subtitle_str
  )
  dev.off()
  cat("Plot saved to:", plot_path, "\n")
}


## save metrics and results
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

# Save SDID results to CSV for comparison with Python
sdid_results <- data.frame(
  method = "R_synthdid",
  outcome = "GDP_yoy_pct",
  ATT = ATT,
  SE = SE,
  z_stat = z_stat,
  p_value = p_value,
  CI_lower = ATT - 1.96 * SE,
  CI_upper = ATT + 1.96 * SE
)
write.csv(sdid_results, "mello_paper_replication/results/sdid_results_R.csv", row.names = FALSE)
cat("\nResults saved to: mello_paper_replication/results/sdid_results_R.csv\n")



# have a look at the unit and time weights
omega <- synthdid::synthdid_controls(tau_hat)

# time weights
lambda <- synthdid::synthdid_times(tau_hat)

summary(omega)
summary(lambda)

# show top control weights
head(sort(omega, decreasing = TRUE), 20)

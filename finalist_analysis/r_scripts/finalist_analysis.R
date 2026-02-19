# ============================================================
# Finalist Analysis (Event Study + SDID variants)
# - Treatment: reaching the World Cup Final (winner OR runner-up)
# - Reference scripts:
#     * paper_replicate_event_study.R   (event study, winner only)
#     * paper_replication_sdid.R       (SDID, winner only)
# ============================================================

rm(list = ls())

# -----------------------------
# 0) Packages
# -----------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(lfe)
library(ggplot2)
library(synthdid)


# ============================================================
# A. COMMON DATA: PANEL + COUNTRY SET
# ============================================================

# We follow the paper_replicate_event_study.R setup for the panel.
# NOTE: this script assumes the enriched OECD panel is available
#       at the same path as in your event-study replication script.

message(">>> Loading enriched OECD panel ...")

df0 <- read_csv(
  "Data/oecd_usd_ppp_real_base_panel_wide_named_plus_pop_plus_wc_eventq_plus_yoy.csv",
  show_col_types = FALSE
) %>%
  mutate(country = as.character(country))

stopifnot(all(c("country", "quarter") %in% names(df0)))

# Paper country sets (same 48 countries as in the main analysis)
controls <- c(
  "ARG","AUS","AUT","BEL","BGR","CAN","CHL","COL","CRI","HRV","CZE","DNK","EST","FIN",
  "GRC","HUN","IND","IDN","ISL","IRL","ISR","LVA","LTU","LUX","NLD","NZL","NOR","POL",
  "PRT","ROU","SAU","SVK","SVN","SWE","CHE","TUR"
)

hosts <- c(
  "BRA","GBR","FRA","DEU","ITA","JPN","MEX","ZAF","KOR","ESP","RUS","USA"
)

paper_countries <- sort(unique(c(controls, hosts)))

message(">>> Trimming to paper country set and 1961–2021 window ...")

df <- df0 %>%
  filter(
    country %in% paper_countries,
    quarter >= "1961-Q1",
    quarter <= "2021-Q4"
  ) %>%
  # Brazil special case: starts 1998-Q2 in paper
  filter(!(country == "BRA" & quarter < "1998-Q2")) %>%
  mutate(
    year = if ("year" %in% names(.)) year else as.integer(str_extract(quarter, "^\\d{4}")),
    qtr  = if ("qtr"  %in% names(.)) qtr  else as.integer(str_extract(quarter, "(?<=-Q)\\d")),
    tq   = year * 4L + qtr
  ) %>%
  arrange(country, tq)


# ============================================================
# B. DEFINE OUTCOME AND FINALIST GROUP
# ============================================================

gdp_level  <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
gdp_yoylog <- paste0(gdp_level, "_yoy_log_4q")
gdp_yoypct <- paste0(gdp_level, "_yoy_pct")

stopifnot(gdp_level %in% names(df), gdp_yoylog %in% names(df), gdp_yoypct %in% names(df))

df <- df %>%
  group_by(country) %>%
  arrange(tq, .by_group = TRUE) %>%
  mutate(
    ln_gdp    = log(.data[[gdp_level]]),
    ln_gdp_l4 = lag(ln_gdp, 4),
    dy_gdp_pp = .data[[gdp_yoylog]],   # Δ4 log GDP (pp) — event study outcome
    dy_gdp_yoy = .data[[gdp_yoypct]], # YoY GDP growth (pp) — SDID outcome
    host      = as.integer(host)
  ) %>%
  ungroup() %>%
  filter(!is.na(dy_gdp_pp))           # keep rows with valid outcome for ES


message(">>> Panel ready: ", nrow(df), " rows, ", dplyr::n_distinct(df$country), " countries.")


# ============================================================
# C. FINALIST EVENTS (WINNERS + RUNNERS-UP)
# ============================================================

# World Cup finals (winner, runner-up) in the OBES sample window.
# We use the same coding as in your Python finalist notebook:
wc_finals <- tibble::tribble(
  ~year, ~winner, ~runner_up,
  1966, "GBR", "DEU",
  1970, "BRA", "ITA",
  1974, "DEU", "NLD",
  1978, "ARG", "NLD",
  1982, "ITA", "DEU",
  1986, "ARG", "DEU",
  1990, "DEU", "ARG",
  1994, "BRA", "ITA",
  1998, "FRA", "BRA",
  2002, "BRA", "DEU",
  2006, "ITA", "FRA",
  2010, "ESP", "NLD",
  2014, "DEU", "ARG",
  2018, "FRA", "HRV"
)

finalist_events <- wc_finals %>%
  tidyr::pivot_longer(cols = c("winner", "runner_up"),
                      names_to = "role", values_to = "country") %>%
  mutate(
    qtr      = 2L,
    tq_event = year * 4L + qtr
  )

finalist_countries <- sort(unique(finalist_events$country))

winner_countries <- c("BRA", "DEU", "ESP", "FRA", "GBR", "ITA")
runner_up_only   <- setdiff(finalist_countries, winner_countries)

message(">>> Finalist countries (ever reached a final):")
print(finalist_countries)
message(">>> Runner-up only countries (no win in sample):")
print(runner_up_only)

df <- df %>%
  mutate(
    finalist = as.integer(country %in% finalist_countries),
    winner   = as.integer(country %in% winner_countries)
  )


# ============================================================
# D. EVENT STUDY: FINALIST TREATMENT
# ============================================================

message(">>> Constructing event-time for finalists ...")

assign_nearest_event <- function(tq_vec, event_tq_vec) {
  sapply(tq_vec, function(tq0) {
    diffs <- tq0 - event_tq_vec
    diffs[which.min(abs(diffs))]
  })
}

df_es <- df %>%
  group_by(country) %>%
  group_modify(~{
    ctry <- .y$country[[1]]
    ev   <- finalist_events %>%
      filter(country == ctry) %>%
      pull(tq_event)
    if (length(ev) > 0) {
      .x$rel_time <- assign_nearest_event(.x$tq, ev)
    } else {
      .x$rel_time <- 0L
    }
    .x
  }) %>%
  ungroup() %>%
  mutate(
    rel_time_bin = dplyr::case_when(
      rel_time <= -16L ~ -16L,
      rel_time >=  16L ~  16L,
      TRUE             ~ as.integer(rel_time)
    )
  )

df_cc <- df_es %>%
  filter(
    !is.na(dy_gdp_pp),
    !is.na(ln_gdp_l4),
    !is.na(host),
    !is.na(rel_time_bin)
  )

message(">>> Event study regression sample: ", nrow(df_cc), " obs, ",
        dplyr::n_distinct(df_cc$country), " countries.")


message(">>> Estimating finalist event study with fixest ...")

# Use i(rel_time_bin, finalist, ref = 0) analogue
m_finalist_es <- feols(
  dy_gdp_pp ~ i(rel_time_bin, finalist, ref = 0) + host + ln_gdp_l4 | country + tq,
  cluster = ~country,
  data = df_cc
)

es_ct <- as.data.frame(coeftable(m_finalist_es))
es_ct$term <- rownames(es_ct)
rownames(es_ct) <- NULL

es_tab <- es_ct %>%
  transmute(term, estimate = Estimate, se = `Std. Error`) %>%
  dplyr::filter(stringr::str_detect(term, "rel_time_bin::")) %>%
  mutate(
    l = as.integer(stringr::str_extract(term, "(?<=rel_time_bin::)-?\\d+"))
  ) %>%
  arrange(l)

message("\n>>> Finalist event study coefficients (first few l):")
print(head(es_tab,20))


# Simple plot (similar to Python finalist_event_study.png)
gg_es <- es_tab %>%
  ggplot(aes(x = l, y = estimate)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_ribbon(aes(ymin = estimate - 1.96 * se,
                  ymax = estimate + 1.96 * se),
              fill = "#bfdbfe", alpha = 0.4) +
  geom_line(colour = "#2563eb", size = 1) +
  geom_point(colour = "#1d4ed8", size = 2) +
  labs(
    title = "Event Study: Effect of Reaching the World Cup Final on GDP growth",
    x     = "Quarters relative to World Cup Final (l = 0, Q2)",
    y     = "Effect on Δ4 log GDP (pp)"
  ) +
  scale_x_continuous(breaks = seq(-16, 16, by = 2)) +
  theme_minimal()
gg_es
ggsave("Finalist_analysis/finalist_event_study_R.png",
       gg_es, width = 8, height = 4.5, dpi = 300)


# ============================================================
# E. SDID: FINALIST TREATMENT - ALL GDP COMPONENTS
# ============================================================

message(">>> Setting up SDID infrastructure for finalists (1998–2018) ...")

wc_years_sdid <- c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L)

events_sdid <- tibble(
  wc_year  = wc_years_sdid,
  event_tq = 4L * wc_year + (2L - 1L)  # Q2 on tq_num scale
)

# Finalist treatment lookup: reached the final in that WC year
# Build once and reuse
finalist_tuples_lookup <- wc_finals %>%
  tidyr::pivot_longer(
    cols = c("winner", "runner_up"),
    names_to = "role",
    values_to = "country"
  ) %>%
  transmute(country = as.character(country),
            wc_year = as.integer(year)) %>%
  distinct()

is_finalist_subseries <- function(country, wc_year) {
  any(finalist_tuples_lookup$country == country & 
      finalist_tuples_lookup$wc_year == wc_year)
}

# Build SDID matrices helper function (adapted from paper_replicate_sdid_full_features.R)
build_sdid_mats_finalist <- function(df_in, y_col, drop_host_only_controls = TRUE) {
  
  stopifnot(y_col %in% names(df_in))
  
  # Stack: each (country × wc_year) is one unit_id, keep q in [-7,2]
  d <- df_in %>%
    mutate(tq_num = 4L * year + (qtr - 1L)) %>%
    select(country, host, tq_num, !!y_col) %>%
    rename(y = !!y_col) %>%
    tidyr::crossing(events_sdid) %>%
    mutate(
      rel_time = as.integer(tq_num - event_tq),
      unit_id  = paste(country, wc_year, sep = "_"),
      treated  = as.integer(mapply(is_finalist_subseries, country, wc_year)),
      post     = as.integer(rel_time >= 1L),
      D        = as.integer(treated * post)
    ) %>%
    filter(rel_time >= -7L, rel_time <= 2L)
  
  # Drop host-only controls from donor pool
  if (drop_host_only_controls) {
    d <- d %>%
      group_by(unit_id) %>%
      mutate(host_subseries = max(host, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!(host_subseries == 1L & treated == 0L)) %>%
      select(-host_subseries)
  }
  
  # Enforce balanced panel and NO missing outcomes
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
  
  if (d %>% distinct(unit_id, treated) %>% summarise(n = sum(treated)) %>% pull(n) == 0) {
    stop("No treated subseries left after NA filtering for this outcome.")
  }
  
  # Build Y matrix: units x time, ordered
  time_levels <- seq(-7L, 2L, by = 1L)
  
  Y_wide <- d %>%
    mutate(rel_time = factor(rel_time, levels = time_levels)) %>%
    select(unit_id, treated, rel_time, y) %>%
    distinct(unit_id, treated, rel_time, .keep_all = TRUE) %>%
    tidyr::pivot_wider(names_from = rel_time, values_from = y)
  
  # Unit ordering: controls first, then treated
  unit_order <- Y_wide %>%
    distinct(unit_id, treated) %>%
    arrange(treated, unit_id)
  
  Y_wide <- Y_wide %>%
    right_join(unit_order, by = c("unit_id", "treated")) %>%
    arrange(treated, unit_id)
  
  # Extract numeric matrix
  time_cols <- as.character(time_levels)
  Y <- as.matrix(Y_wide[, time_cols])
  
  stopifnot(!anyNA(Y))
  treated_vec <- unit_order$treated
  stopifnot(length(unique(treated_vec)) > 1)
  
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

# Runner: estimate ATT, bootstrap SE, p-value, plot with ATE in title
run_one_outcome_finalist <- function(df_in, outcome_name, y_col, reps = 300) {
  
  cat("\n================================================\n")
  cat("Outcome:", outcome_name, "\nColumn:", y_col, "\n")
  
  mats <- build_sdid_mats_finalist(df_in, y_col)
  
  tau_hat <- synthdid::synthdid_estimate(mats$Y, mats$N0, mats$T0)
  
  # Bootstrap SE (reduced to 300 replications)
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
  
  # Plot with ATE stated in title (like paper_replicate_sdid_full_features.R)
  p_obj <- synthdid::synthdid_plot(tau_hat)
  
  # Create title with ATE
  title_text <- sprintf("SDiD: Finalist effect on YoY %s growth (ATT = %.2f pp, SE = %.2f)",
                       outcome_name, ATT, se_hat)
  
  if (inherits(p_obj, "ggplot")) {
    p_final <- p_obj +
      ggplot2::ggtitle(title_text) +
      ggplot2::labs(x = "Quarter to/from World Cup Final (q = rel_time)",
                    y = "YoY growth (pp)")
    print(p_final)
    
    # Save plot
    filename <- paste0("Finalist_analysis/SDID_Finalist_", 
                      gsub(" ", "_", outcome_name), ".png")
    ggsave(filename, p_final, width = 10, height = 6, dpi = 300)
    cat("Saved:", filename, "\n")
  } else {
    # base graphics path
    graphics::title(
      main = title_text,
      xlab = "Quarter to/from World Cup Final (q = rel_time)",
      ylab = "YoY growth (pp)"
    )
    
    # Save base graphics plot
    filename <- paste0("Finalist_analysis/SDID_Finalist_", 
                      gsub(" ", "_", outcome_name), ".png")
    dev.copy(png, filename, width = 10, height = 6, units = "in", res = 300)
    dev.off()
    cat("Saved:", filename, "\n")
  }
  
  N1 <- mats$d %>% distinct(unit_id, treated) %>% filter(treated == 1L) %>% nrow()
  
  invisible(list(tau_hat = tau_hat, ATT = ATT, SE = se_hat, z = z, p = p,
                 N_treated = N1,
                 N_control = mats$N0))
}

# Prepare data for SDID (all components)
df_sdid <- df %>%
  mutate(tq_num = 4L * year + (qtr - 1L))

# Run SDID for all GDP components
results_list <- list()

# GDP
results_list[["GDP"]] <- run_one_outcome_finalist(
  df_sdid,
  outcome_name = "GDP",
  y_col = "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct",
  reps = 300
)

# Final consumption
results_list[["Final_Consumption"]] <- run_one_outcome_finalist(
  df_sdid,
  outcome_name = "Final Consumption",
  y_col = "final_consumption_expenditure_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct",
  reps = 300
)

# Gross fixed capital formation (Investment)
results_list[["Gross_Fixed_Capital"]] <- run_one_outcome_finalist(
  df_sdid,
  outcome_name = "Gross Fixed Capital Formation",
  y_col = "gross_fixed_capital_formation_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct",
  reps = 300
)

# Exports
results_list[["Exports"]] <- run_one_outcome_finalist(
  df_sdid,
  outcome_name = "Exports",
  y_col = "exports_of_goods_and_services_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct",
  reps = 300
)

# Imports
results_list[["Imports"]] <- run_one_outcome_finalist(
  df_sdid,
  outcome_name = "Imports",
  y_col = "imports_of_goods_and_services_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct",
  reps = 300
)

# Compile results into comparison table
comparison_all <- tibble::tibble(
  Outcome = names(results_list),
  ATT_pp = sapply(results_list, function(x) x$ATT),
  SE = sapply(results_list, function(x) x$SE),
  CI_Lower = sapply(results_list, function(x) x$ATT - 1.96 * x$SE),
  CI_Upper = sapply(results_list, function(x) x$ATT + 1.96 * x$SE),
  z_stat = sapply(results_list, function(x) x$z),
  p_value = sapply(results_list, function(x) x$p),
  N_Treated = sapply(results_list, function(x) x$N_treated),
  N_Control = sapply(results_list, function(x) x$N_control)
)

write_csv(comparison_all, "Finalist_analysis/finalist_analysis_results_R.csv")

cat("\n================================================\n")
cat("FINALIST SDID RESULTS SUMMARY\n")
cat("================================================\n")
print(comparison_all)

message("\n>>> Done. All outputs written to 'Finalist_analysis/' folder.")


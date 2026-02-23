# =============================================================================
# paper_summary_stat_table_replication.R
#
# Compute summary statistics for the event-study sample by period and
# winner-country status. Outputs:
#   1. mello_paper_replication/results/table1_summary_statistics.csv  (tidy CSV)
#   2. thesis/tables/table1_summary_statistics.tex                    (LaTeX)
#
# Input: Data/mello_paper_replication/paper_replication_event_study_sample.csv
# Run from repo root:
#   Rscript mello_paper_replication/r_scripts/paper_summary_stat_table_replication.R
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
df <- read_csv(
  "Data/mello_paper_replication/paper_replication_event_study_sample.csv",
  show_col_types = FALSE
)

# Winner countries = those that won at least one WC in the estimation sample
winner_countries <- c("BRA", "DEU", "ESP", "FRA", "GBR", "ITA")

df <- df %>%
  mutate(
    winner_group = if_else(country %in% winner_countries, "Winner", "Non-winner"),
    period = case_when(
      year >= 1962 & year <= 1980 ~ "1962-1980",
      year >  1980 & year <= 2000 ~ "1980-2000",
      year >  2000 & year <= 2021 ~ "2000-2021",
      TRUE ~ NA_character_
    )
  )

# ---------------------------------------------------------------------------
# 2. Variables to summarise
# ---------------------------------------------------------------------------
level_vars <- c(
  "gdp", "private_consumption", "government_consumption",
  "capital_formation", "exports", "imports", "population"
)
growth_vars <- c(
  "gdp_yoy_log_4q", "private_consumption_yoy_log_4q",
  "government_consumption_yoy_log_4q", "capital_formation_yoy_log_4q",
  "exports_yoy_log_4q", "imports_yoy_log_4q"
)
all_vars <- c(level_vars, growth_vars)

# Nice labels for display
var_labels <- c(
  gdp = "GDP (millions PPP USD)",
  private_consumption = "Private consumption",
  government_consumption = "Government consumption",
  capital_formation = "Gross fixed capital formation",
  exports = "Exports",
  imports = "Imports",
  population = "Population",
  gdp_yoy_log_4q = "GDP growth (d4 log)",
  private_consumption_yoy_log_4q = "Private cons. growth (d4 log)",
  government_consumption_yoy_log_4q = "Gov. cons. growth (d4 log)",
  capital_formation_yoy_log_4q = "Investment growth (d4 log)",
  exports_yoy_log_4q = "Exports growth (d4 log)",
  imports_yoy_log_4q = "Imports growth (d4 log)"
)

# ---------------------------------------------------------------------------
# 3. Compute stats by period x winner_group x variable
# ---------------------------------------------------------------------------
compute_block <- function(data, period_label) {
  rows <- list()
  for (v in all_vars) {
    w  <- data %>% filter(winner_group == "Winner")  %>% pull(!!sym(v)) %>% na.omit()
    nw <- data %>% filter(winner_group == "Non-winner") %>% pull(!!sym(v)) %>% na.omit()
    
    tt <- t.test(w, nw)
    p  <- tt$p.value
    stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
    
    rows[[v]] <- tibble(
      period    = period_label,
      variable  = v,
      label     = var_labels[v],
      w_mean    = mean(w),
      w_sd      = sd(w),
      w_n       = length(w),
      nw_mean   = mean(nw),
      nw_sd     = sd(nw),
      nw_n      = length(nw),
      t_stat    = tt$statistic,
      p_value   = p,
      stars     = stars
    )
  }
  
  # Add counts row
  w_countries  <- data %>% filter(winner_group == "Winner")   %>% distinct(country) %>% nrow()
  nw_countries <- data %>% filter(winner_group == "Non-winner") %>% distinct(country) %>% nrow()
  w_obs  <- data %>% filter(winner_group == "Winner")   %>% nrow()
  nw_obs <- data %>% filter(winner_group == "Non-winner") %>% nrow()
  
  rows[["_counts"]] <- tibble(
    period = period_label, variable = "_counts", label = "Counts",
    w_mean = w_countries, w_sd = NA_real_, w_n = w_obs,
    nw_mean = nw_countries, nw_sd = NA_real_, nw_n = nw_obs,
    t_stat = NA_real_, p_value = NA_real_, stars = ""
  )
  
  bind_rows(rows)
}

periods <- list(
  "1962-1980" = df %>% filter(period == "1962-1980"),
  "1980-2000" = df %>% filter(period == "1980-2000"),
  "2000-2021" = df %>% filter(period == "2000-2021"),
  "Full sample" = df
)

results <- bind_rows(lapply(names(periods), function(p) {
  compute_block(periods[[p]], p)
}))
print(results, n = Inf)
# ---------------------------------------------------------------------------
# 4. Save CSV
# ---------------------------------------------------------------------------
csv_path <- "mello_paper_replication/results/table1_summary_statistics.csv"
write_csv(results, csv_path)
cat("Saved CSV:", csv_path, "\n")

# ---------------------------------------------------------------------------
# 5. Generate LaTeX table
# ---------------------------------------------------------------------------
# Focus on the full-sample block for the main thesis table (matching Mello's
# Table 1 format). Sub-period details can go in appendix if desired.

make_latex_row <- function(row) {
  # Format mean (sd) strings
  if (row$variable == "_counts") return(NULL)  # handled separately
  
  # Determine formatting: levels in thousands/millions, growth in 2 decimals
  is_growth <- grepl("yoy_log_4q", row$variable)
  is_pop    <- row$variable == "population"
  
  if (is_pop) {
    w_str  <- sprintf("%.1f (%.1f)", row$w_mean / 1e6, row$w_sd / 1e6)
    nw_str <- sprintf("%.1f (%.1f)", row$nw_mean / 1e6, row$nw_sd / 1e6)
  } else if (!is_growth) {
    # Level vars: display in thousands (i.e. divide by 1000 to get "thousands of millions")
    w_str  <- sprintf("%.0f (%.0f)", row$w_mean / 1e3, row$w_sd / 1e3)
    nw_str <- sprintf("%.0f (%.0f)", row$nw_mean / 1e3, row$nw_sd / 1e3)
  } else {
    w_str  <- sprintf("%.2f (%.2f)", row$w_mean, row$w_sd)
    nw_str <- sprintf("%.2f (%.2f)", row$nw_mean, row$nw_sd)
  }
  
  t_str <- sprintf("%.2f%s", row$t_stat, row$stars)
  sprintf("\\quad %s & %s & %s & %s \\\\", row$label, w_str, nw_str, t_str)
}

# Build LaTeX
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Summary statistics for the event-study sample}",
  "\\label{tab:summary_stats}",
  "\\small",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & \\textbf{Winner} & \\textbf{Non-winner} & \\textbf{$t$-test} \\\\",
  " & Mean (SD) & Mean (SD) & \\\\",
  "\\midrule"
)

period_labels_tex <- c(
  "1962-1980" = "\\textbf{1962--1980}",
  "1980-2000" = "\\textbf{1980--2000}",
  "2000-2021" = "\\textbf{2000--2021}",
  "Full sample" = "\\textbf{Full sample}"
)

for (p in names(period_labels_tex)) {
  block <- results %>% filter(period == p)
  tex_lines <- c(tex_lines, paste0(period_labels_tex[p], " \\\\"))
  
  for (i in seq_len(nrow(block))) {
    row <- block[i, ]
    lr <- make_latex_row(row)
    if (!is.null(lr)) tex_lines <- c(tex_lines, lr)
  }
  
  # Add counts for full sample
  if (p == "Full sample") {
    counts <- block %>% filter(variable == "_counts")
    tex_lines <- c(tex_lines,
      sprintf("\\quad Number of countries & %d & %d & \\\\",
              as.integer(counts$w_mean), as.integer(counts$nw_mean)),
      sprintf("\\quad Number of observations & %d & %d & \\\\",
              counts$w_n, counts$nw_n)
    )
  }
}

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  "\\item \\textit{Notes:} Standard deviations in parentheses. Level variables are in millions of PPP USD (2015 base); population in millions. Growth rates are $\\Delta_4 \\ln$ (year-over-year log differences $\\times 100$). ``Winner'' denotes all observations from countries that won a World Cup during the sample period (BRA, DEU, ESP, FRA, GBR, ITA). *$p<0.10$, **$p<0.05$, ***$p<0.01$.",
  "\\end{tablenotes}",
  "\\end{table}"
)

tex_path <- "thesis/tables/table1_summary_statistics.tex"
writeLines(tex_lines, tex_path)
cat("Saved LaTeX:", tex_path, "\n")

# Also overwrite the old CSV in results
csv_path2 <- "mello_paper_replication/results/summary_statistics_table.csv"
write_csv(results, csv_path2)
cat("Saved CSV copy:", csv_path2, "\n")

cat("\n=== Done ===\n")

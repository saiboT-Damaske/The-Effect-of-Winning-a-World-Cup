# =============================================================================
# event_study_comparison.R
#
# Compare our GDP event study coefficients with Mello (2024) Table 2.
# Outputs:
#   1. Line plot with shaded CIs  -> mello_paper_replication/event_study_plots/
#   2. Full comparison table (CSV) -> mello_paper_replication/results/
#   3. LaTeX table (all 32 leads/lags + controls) -> thesis/tables/
#
# Run from repo root:
#   Rscript mello_paper_replication/r_scripts/event_study_comparison.R
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------------------------------------------------------------------
# 1. Load our fixest results
# ---------------------------------------------------------------------------
ours <- read_csv(
  "mello_paper_replication/results/event_study_coefficients_R_fixest.csv",
  show_col_types = FALSE
)

# Keep only the 32 relative-time dummies (l = -16 ... -1, +1 ... +16)
ours_rt <- ours %>%
  filter(!is.na(l)) %>%
  select(l, estimate, se, pval) %>%
  arrange(l)

# Controls (convergence + host)
ours_controls <- ours %>%
  filter(is.na(l)) %>%
  select(term, estimate, se, pval)

# ---------------------------------------------------------------------------
# 2. Mello (2024) Table 2 — hardcoded from the published paper
#    SE in parentheses; significance from the paper text.
#    Mello reports * p<0.10, ** p<0.05, *** p<0.01
#    Only l=+1 and l=+2 are marked * in the paper.
# ---------------------------------------------------------------------------
mello_rt <- tibble::tribble(
  ~l,  ~estimate,  ~se,
  -16,  0.640,  0.673,
  -15,  0.363,  0.538,
  -14,  0.276,  0.655,
  -13,  0.286,  0.474,
  -12, -0.082,  0.503,
  -11, -0.226,  0.604,
  -10,  0.074,  0.769,
   -9, -0.139,  0.616,
   -8,  0.098,  0.695,
   -7,  0.209,  0.570,
   -6,  0.196,  0.589,
   -5,  0.484,  0.621,
   -4, -0.107,  0.535,
   -3, -0.288,  0.479,
   -2, -0.605,  0.432,
   -1,  0.125,  0.206,
    1,  0.454,  0.246,
    2,  0.683,  0.370,
    3,  0.233,  0.335,
    4,  0.140,  0.317,
    5, -0.189,  0.357,
    6, -0.034,  0.422,
    7, -0.288,  0.761,
    8, -0.314,  0.985,
    9,  0.418,  0.387,
   10, -0.145,  0.421,
   11,  0.021,  0.461,
   12,  0.289,  0.583,
   13, -0.593,  0.606,
   14, -0.320,  0.628,
   15, -0.412,  0.676,
   16, -0.109,  0.477
)

# Mello does not report p-values; we back-compute from t = coeff/SE, two-sided,
# using N = 8637, df ≈ 8637 - 48 - 244 - 34 ≈ 8311 (effectively normal)
mello_rt <- mello_rt %>%
  mutate(
    tval = estimate / se,
    pval = 2 * pnorm(-abs(tval))
  )

mello_controls <- tibble::tribble(
  ~term,       ~estimate,   ~se,
  "ln_gdp_l4", -1.368,     0.588,
  "host",      -0.591,     0.545
) %>%
  mutate(
    tval = estimate / se,
    pval = 2 * pnorm(-abs(tval))
  )

# ---------------------------------------------------------------------------
# 3. Helper: significance stars
# ---------------------------------------------------------------------------
stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE      ~ ""
  )
}

# ---------------------------------------------------------------------------
# 4. Merge for plotting
# ---------------------------------------------------------------------------
plot_df <- bind_rows(
  ours_rt %>%
    mutate(source = "Replication",
           ci_lo  = estimate - 1.96 * se,
           ci_hi  = estimate + 1.96 * se),
  mello_rt %>%
    select(l, estimate, se, pval) %>%
    mutate(source = "Mello (2024)",
           ci_lo  = estimate - 1.96 * se,
           ci_hi  = estimate + 1.96 * se)
)

# ---------------------------------------------------------------------------
# 5. Line plot with shaded CIs
# ---------------------------------------------------------------------------
p <- ggplot(plot_df, aes(x = l, y = estimate, colour = source, fill = source)) +
  # Shaded confidence bands
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, colour = NA) +
  # Lines
  geom_line(linewidth = 0.8) +
  # Points
  geom_point(size = 1.8) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey40", linewidth = 0.4) +
  # Scales
  scale_x_continuous(
    breaks = seq(-16, 16, by = 4),
    labels = function(x) ifelse(x == 0, "0\n(WC)", as.character(x))
  ) +
  scale_colour_manual(values = c("Replication" = "#2166AC", "Mello (2024)" = "#B2182B")) +
  scale_fill_manual(values   = c("Replication" = "#2166AC", "Mello (2024)" = "#B2182B")) +
  # Labels
  labs(
    title    = "Event Study Comparison: GDP Growth",
    subtitle = "Replication (N = 8,633) vs. Mello 2024 (N = 8,637)",
    x        = "Quarters relative to World Cup victory (l)",
    y        = expression(hat(beta)[l] ~ "(pp)"),
    colour   = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = c(0.15, 0.92),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, colour = "grey30")
  )

# Save
dir.create("mello_paper_replication/event_study_plots", showWarnings = FALSE)
ggsave("mello_paper_replication/event_study_plots/event_study_gdp_comparison.png",
       p, width = 10, height = 6, dpi = 300)
cat("Saved plot: mello_paper_replication/event_study_plots/event_study_gdp_comparison.png\n")

# ---------------------------------------------------------------------------
# 6. Full comparison table (all 32 + controls)
# ---------------------------------------------------------------------------
table_rt <- ours_rt %>%
  select(l, estimate, se, pval) %>%
  rename(repl_coeff = estimate, repl_se = se, repl_pval = pval) %>%
  left_join(
    mello_rt %>% select(l, estimate, se, pval) %>%
      rename(mello_coeff = estimate, mello_se = se, mello_pval = pval),
    by = "l"
  ) %>%
  mutate(
    repl_stars  = stars(repl_pval),
    mello_stars = stars(mello_pval),
    diff        = repl_coeff - mello_coeff
  ) %>%
  arrange(l)

table_controls <- ours_controls %>%
  rename(repl_coeff = estimate, repl_se = se, repl_pval = pval) %>%
  left_join(
    mello_controls %>% select(term, estimate, se, pval) %>%
      rename(mello_coeff = estimate, mello_se = se, mello_pval = pval),
    by = "term"
  ) %>%
  mutate(
    repl_stars  = stars(repl_pval),
    mello_stars = stars(mello_pval),
    diff        = repl_coeff - mello_coeff,
    l           = NA_integer_
  )

# Save CSV
full_table <- bind_rows(table_controls, table_rt) %>%
  select(term, l, repl_coeff, repl_se, repl_stars, mello_coeff, mello_se, mello_stars, diff)

write_csv(full_table,
          "mello_paper_replication/results/event_study_gdp_comparison_table.csv")
cat("Saved CSV: mello_paper_replication/results/event_study_gdp_comparison_table.csv\n")

# ---------------------------------------------------------------------------
# 7. LaTeX table
# ---------------------------------------------------------------------------
fmt <- function(x, d = 3) formatC(round(x, d), format = "f", digits = d)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Event study coefficients: Replication vs.\\ \\citet{Mello2024}, GDP growth}",
  "\\label{tab:es_full_comparison}",
  "\\footnotesize",
  "\\begin{tabular}{l rr rr r}",
  "\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Replication}} & \\multicolumn{2}{c}{\\textbf{Mello (2024)}} & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "  & Coeff. & (SE) & Coeff. & (SE) & Diff. \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Controls}} \\\\"
)

# Controls rows
for (i in seq_len(nrow(table_controls))) {
  r <- table_controls[i, ]
  label <- if (r$term == "ln_gdp_l4") "$\\ln \\text{GDP}_{t-4}$" else "Host"
  tex <- c(tex, sprintf(
    "%s & %s%s & (%s) & %s%s & (%s) & %s \\\\",
    label,
    fmt(r$repl_coeff), r$repl_stars, fmt(r$repl_se),
    fmt(r$mello_coeff), r$mello_stars, fmt(r$mello_se),
    fmt(r$diff)
  ))
}

tex <- c(tex, "[4pt]",
         "\\multicolumn{6}{l}{\\textit{Pre-treatment}} \\\\")

# Pre-treatment rows
pre <- table_rt %>% filter(l < 0)
for (i in seq_len(nrow(pre))) {
  r <- pre[i, ]
  label <- if (r$l == -16) "$l = -16$ (binned)" else sprintf("$l = %d$", r$l)
  tex <- c(tex, sprintf(
    "%s & %s%s & (%s) & %s%s & (%s) & %s \\\\",
    label,
    fmt(r$repl_coeff), r$repl_stars, fmt(r$repl_se),
    fmt(r$mello_coeff), r$mello_stars, fmt(r$mello_se),
    fmt(r$diff)
  ))
}

tex <- c(tex, "[4pt]",
         "\\multicolumn{6}{l}{\\textit{Post-treatment}} \\\\")

# Post-treatment rows
post <- table_rt %>% filter(l > 0)
for (i in seq_len(nrow(post))) {
  r <- post[i, ]
  label <- if (r$l == 16) "$l = +16$ (binned)" else sprintf("$l = +%d$", r$l)
  tex <- c(tex, sprintf(
    "%s & %s%s & (%s) & %s%s & (%s) & %s \\\\",
    label,
    fmt(r$repl_coeff), r$repl_stars, fmt(r$repl_se),
    fmt(r$mello_coeff), r$mello_stars, fmt(r$mello_se),
    fmt(r$diff)
  ))
}

tex <- c(tex,
  "\\midrule",
  "Observations & \\multicolumn{2}{c}{8,633} & \\multicolumn{2}{c}{8,637} & \\\\",
  "Adj.\\ $R^2$ & \\multicolumn{2}{c}{0.444} & \\multicolumn{2}{c}{---} & \\\\",
  "Within $R^2$ (Mello) & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{0.423} & \\\\",
  "\\bottomrule",
  "\\multicolumn{6}{l}{\\footnotesize *\\,$p<0.10$,\\; **\\,$p<0.05$,\\; ***\\,$p<0.01$.\\; Clustered SEs at the country level.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Mello $p$-values inferred from reported coefficients and SEs (normal approximation).}",
  "\\end{tabular}",
  "\\end{table}"
)

dir.create("thesis/tables", showWarnings = FALSE)
writeLines(tex, "thesis/tables/event_study_full_comparison.tex")
cat("Saved LaTeX: thesis/tables/event_study_full_comparison.tex\n")

cat("\n=== Done ===\n")

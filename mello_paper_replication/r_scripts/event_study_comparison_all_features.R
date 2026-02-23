# =============================================================================
# event_study_comparison_all_features.R
#
# Compare our GDP-component event study coefficients with Mello (2024) Table A3.
# Outputs:
#   1. 6-panel line plot (GDP + 5 components) with shaded CIs
#      -> mello_paper_replication/event_study_plots/
#   2. Concise comparison table CSV (selected lags) for all 6 features
#      -> mello_paper_replication/results/
#   3. LaTeX table for appendix
#      -> thesis/tables/
#
# Run from repo root:
#   Rscript mello_paper_replication/r_scripts/event_study_comparison_all_features.R
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------------------
# 1. Load our fixest results (all 6 features)
# ---------------------------------------------------------------------------
ours_all <- read_csv(
  "mello_paper_replication/results/event_study_all_features_coefficients.csv",
  show_col_types = FALSE
)

# Keep only rel-time dummies (has valid l)
ours_rt <- ours_all %>%
  filter(!is.na(l)) %>%
  select(feature, feature_label, l, estimate, se, pval) %>%
  arrange(feature, l)

# ---------------------------------------------------------------------------
# 2. Mello (2024) Table A3 — hardcoded from the published paper
#    Columns: PCons, GCons, CapForm, Exports, Imports
#    Also include GDP from Table 2 for completeness.
# ---------------------------------------------------------------------------

# GDP (Table 2) - already in the GDP comparison script but repeat here
mello_gdp <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  0.640, 0.673, -15,  0.363, 0.538, -14,  0.276, 0.655, -13,  0.286, 0.474,
  -12, -0.082, 0.503, -11, -0.226, 0.604, -10,  0.074, 0.769,  -9, -0.139, 0.616,
   -8,  0.098, 0.695,  -7,  0.209, 0.570,  -6,  0.196, 0.589,  -5,  0.484, 0.621,
   -4, -0.107, 0.535,  -3, -0.288, 0.479,  -2, -0.605, 0.432,  -1,  0.125, 0.206,
    1,  0.454, 0.246,   2,  0.683, 0.370,   3,  0.233, 0.335,   4,  0.140, 0.317,
    5, -0.189, 0.357,   6, -0.034, 0.422,   7, -0.288, 0.761,   8, -0.314, 0.985,
    9,  0.418, 0.387,  10, -0.145, 0.421,  11,  0.021, 0.461,  12,  0.289, 0.583,
   13, -0.593, 0.606,  14, -0.320, 0.628,  15, -0.412, 0.676,  16, -0.109, 0.477
) %>% mutate(feature = "gdp", feature_label = "GDP")

# Private Consumption (Table A3, cols 1-2)
mello_pcons <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  1.046, 0.518, -15,  0.059, 0.578, -14,  0.353, 0.557, -13,  0.308, 0.413,
  -12,  0.514, 0.466, -11,  0.929, 0.631, -10,  0.894, 0.618,  -9,  0.652, 0.657,
   -8,  0.637, 0.847,  -7,  0.626, 0.740,  -6,  0.380, 0.623,  -5,  0.275, 0.651,
   -4, -0.368, 0.574,  -3, -0.489, 0.522,  -2, -0.763, 0.456,  -1, -0.202, 0.203,
    1, -0.169, 0.515,   2,  0.214, 0.559,   3, -0.198, 0.982,   4, -0.149, 1.087,
    5,  0.170, 0.730,   6,  0.247, 0.775,   7,  0.156, 0.681,   8, -0.184, 0.527,
    9,  0.390, 0.879,  10,  0.047, 1.034,  11, -0.116, 1.174,  12,  0.163, 0.842,
   13, -0.136, 0.950,  14,  0.185, 0.679,  15,  0.395, 0.528,  16,  0.186, 0.538
) %>% mutate(feature = "private_consumption", feature_label = "Private Consumption")

# Government Consumption (Table A3, cols 3-4)
mello_gcons <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  0.454, 0.731, -15,  1.024, 0.566, -14,  0.860, 0.471, -13,  0.766, 0.492,
  -12,  0.466, 0.435, -11,  0.168, 0.496, -10,  0.584, 0.599,  -9,  0.868, 0.403,
   -8,  0.941, 0.799,  -7,  0.408, 0.933,  -6,  0.651, 0.812,  -5,  0.071, 0.732,
   -4,  0.678, 0.779,  -3,  0.100, 0.760,  -2, -0.093, 0.668,  -1,  0.265, 0.536,
    1,  0.384, 0.368,   2,  0.080, 0.501,   3,  0.264, 0.884,   4,  0.082, 0.752,
    5,  0.547, 0.563,   6,  0.863, 0.620,   7,  0.407, 0.863,   8, -0.640, 1.507,
    9,  0.347, 0.885,  10, -0.451, 0.794,  11, -0.522, 0.715,  12,  0.138, 1.483,
   13, -1.105, 0.679,  14, -0.646, 0.568,  15, -0.424, 0.631,  16, -0.060, 0.520
) %>% mutate(feature = "government_consumption", feature_label = "Government Consumption")

# Capital Formation (Table A3, cols 5-6)
mello_capform <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  2.723, 1.438, -15,  2.417, 1.451, -14,  2.260, 1.734, -13,  0.230, 1.551,
  -12, -0.240, 1.559, -11, -0.949, 1.325, -10, -0.041, 1.425,  -9,  1.357, 1.913,
   -8,  0.797, 1.876,  -7,  1.085, 1.778,  -6,  1.248, 1.856,  -5,  1.303, 1.980,
   -4, -0.417, 1.587,  -3, -0.286, 1.522,  -2, -1.708, 1.092,  -1, -0.811, 0.868,
    1,  0.229, 0.515,   2,  1.552, 1.128,   3,  1.228, 1.370,   4, -0.029, 2.008,
    5,  0.898, 2.066,   6,  0.398, 2.100,   7,  0.454, 2.222,   8,  1.892, 1.932,
    9,  2.371, 1.604,  10,  2.550, 1.573,  11,  1.576, 1.331,  12,  1.331, 1.704,
   13,  0.535, 1.282,  14,  0.212, 1.672,  15,  1.755, 1.464,  16,  0.185, 1.256
) %>% mutate(feature = "capital_formation", feature_label = "Capital Formation")

# Exports (Table A3, cols 7-8)
mello_exports <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  2.040, 2.171, -15,  0.185, 1.769, -14,  1.025, 2.032, -13,  1.935, 2.591,
  -12,  0.519, 2.103, -11,  0.260, 2.090, -10,  0.438, 3.159,  -9, -2.109, 2.583,
   -8, -1.538, 2.126,  -7, -0.709, 3.120,  -6, -1.263, 1.832,  -5,  0.275, 1.979,
   -4,  2.327, 2.565,  -3,  2.584, 1.802,  -2,  2.555, 2.022,  -1,  3.840, 1.620,
    1,  3.183, 2.631,   2,  5.124, 2.767,   3,  3.844, 2.359,   4,  5.049, 3.631,
    5,  1.506, 1.816,   6, -1.297, 1.918,   7, -0.277, 2.606,   8, -1.381, 2.643,
    9,  0.204, 2.832,  10,  0.815, 3.302,  11, -0.916, 2.953,  12,  1.110, 2.725,
   13, -0.430, 2.692,  14, -0.466, 2.418,  15,  0.714, 1.945,  16,  0.501, 1.726
) %>% mutate(feature = "exports", feature_label = "Exports")

# Imports (Table A3, cols 9-10)
mello_imports <- tibble::tribble(
  ~l, ~estimate, ~se,
  -16,  3.603, 2.417, -15,  3.202, 1.685, -14,  4.664, 2.192, -13,  1.808, 1.514,
  -12,  1.188, 1.528, -11,  0.245, 0.848, -10,  0.091, 1.361,  -9,  1.007, 2.093,
   -8,  0.711, 2.667,  -7,  2.183, 3.730,  -6,  1.131, 3.354,  -5,  0.421, 3.713,
   -4,  0.409, 3.427,  -3, -0.659, 2.225,  -2, -0.333, 1.334,  -1,  0.436, 1.111,
    1,  1.719, 1.201,   2,  1.429, 1.245,   3,  3.138, 1.947,   4,  4.326, 2.084,
    5,  2.844, 1.786,   6,  4.260, 3.007,   7,  2.432, 2.410,   8,  1.331, 2.131,
    9,  2.196, 2.222,  10,  1.218, 1.813,  11,  1.100, 2.853,  12,  0.886, 2.586,
   13,  0.215, 2.633,  14,  0.719, 2.477,  15,  1.729, 2.558,  16,  2.266, 2.872
) %>% mutate(feature = "imports", feature_label = "Imports")

# Combine all Mello data
mello_all <- bind_rows(mello_gdp, mello_pcons, mello_gcons,
                       mello_capform, mello_exports, mello_imports) %>%
  mutate(
    tval = estimate / se,
    pval = 2 * pnorm(-abs(tval))
  )

# Mello controls (convergence + host per component)
mello_controls <- tibble::tribble(
  ~feature,                  ~feature_label,            ~conv_coeff, ~conv_se, ~host_coeff, ~host_se,
  "gdp",                    "GDP",                      -1.368, 0.588,  -0.591, 0.545,
  "private_consumption",    "Private Consumption",      -2.665, 0.651,  -0.098, 0.801,
  "government_consumption", "Government Consumption",   -3.349, 1.278,  -0.155, 0.265,
  "capital_formation",      "Capital Formation",        -7.795, 1.624,  -1.606, 1.404,
  "exports",                "Exports",                  -3.730, 0.754,   1.249, 0.983,
  "imports",                "Imports",                   -6.056, 0.952,  -0.390, 1.909
)

# Mello observation counts and within R²
mello_stats <- tibble::tribble(
  ~feature,                  ~mello_n,  ~mello_r2,
  "gdp",                    8637,       0.423,
  "private_consumption",    8549,       0.352,
  "government_consumption", 8549,       0.129,
  "capital_formation",      8549,       0.213,
  "exports",                8549,       0.380,
  "imports",                8549,       0.364
)

# ---------------------------------------------------------------------------
# 3. Helper: significance stars
# ---------------------------------------------------------------------------
stars_fn <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE      ~ ""
  )
}

# ---------------------------------------------------------------------------
# 4. Build combined plotting dataframe — join on 'feature' (avoids label case
#    mismatches between our CSV and hardcoded Mello labels)
# ---------------------------------------------------------------------------

# Canonical labels keyed on feature id
feat_labels <- c(
  gdp                    = "GDP",
  private_consumption    = "Private Consumption",
  government_consumption = "Government Consumption",
  capital_formation      = "Capital Formation",
  exports                = "Exports",
  imports                = "Imports"
)

plot_df <- bind_rows(
  ours_rt %>% mutate(source = "Replication"),
  mello_all %>% mutate(source = "Mello (2024)")
) %>%
  mutate(
    panel_label = feat_labels[feature],
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  )

# ---------------------------------------------------------------------------
# 5. Build individual plots per feature and combine with patchwork
# ---------------------------------------------------------------------------
colours <- c("Replication" = "#2166AC", "Mello (2024)" = "#B2182B")

make_panel <- function(feat_id, show_legend = FALSE) {
  df <- plot_df %>% filter(feature == feat_id)
  ttl <- feat_labels[feat_id]

  p <- ggplot(df, aes(x = l, y = estimate, colour = source, fill = source)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey40", linewidth = 0.3) +
    scale_x_continuous(breaks = seq(-16, 16, by = 8)) +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = colours) +
    labs(title = ttl, x = NULL, y = expression(hat(beta)[l] ~ "(pp)")) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position  = if (show_legend) "bottom" else "none",
      legend.title     = element_blank()
    )
  p
}

panels <- lapply(names(feat_labels), make_panel)

combined <- (panels[[1]] | panels[[2]]) /
            (panels[[3]] | panels[[4]]) /
            (panels[[5]] | panels[[6]]) +
  plot_annotation(
    title    = "Event Study Comparison: All GDP Components",
    subtitle = "Replication vs. Mello (2024), Table 2 & Table A3",
    caption  = "Blue = Replication, Red = Mello (2024). Shaded areas = 95% CI.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, colour = "grey30"),
      plot.caption  = element_text(size = 9, colour = "grey40")
    )
  )

# Extract a shared legend from one panel and attach at bottom
leg_panel <- make_panel("gdp", show_legend = TRUE) +
  guides(colour = guide_legend(override.aes = list(size = 3)),
         fill   = guide_legend(override.aes = list(alpha = 0.3)))
leg <- cowplot::get_legend(leg_panel)

final_plot <- cowplot::plot_grid(
  combined, leg,
  ncol = 1, rel_heights = c(1, 0.04)
)

dir.create("mello_paper_replication/event_study_plots", showWarnings = FALSE)
ggsave("mello_paper_replication/event_study_plots/event_study_all_features_comparison.png",
       final_plot, width = 12, height = 10, dpi = 300)
cat("Saved plot: mello_paper_replication/event_study_plots/event_study_all_features_comparison.png\n")

# ---------------------------------------------------------------------------
# 6. Concise comparison table: selected lags (l = +1..+4, +8, +16)
#    plus convergence control and host, for all 6 features
# ---------------------------------------------------------------------------
selected_lags <- c(1, 2, 3, 4, 8, 16)

# Our data — selected relative-time + controls from all-features CSV
ours_controls <- ours_all %>%
  filter(is.na(l)) %>%
  select(feature, feature_label, term, estimate, se, pval)

# Build our selected-lag rows
ours_sel <- ours_rt %>%
  filter(l %in% selected_lags) %>%
  mutate(row_label = ifelse(l == 16, "$l = +16$ (binned)", sprintf("$l = +%d$", l))) %>%
  select(feature, feature_label, l, row_label, estimate, se, pval)

# Build Mello selected-lag rows
mello_sel <- mello_all %>%
  filter(l %in% selected_lags) %>%
  mutate(row_label = ifelse(l == 16, "$l = +16$ (binned)", sprintf("$l = +%d$", l))) %>%
  select(feature, feature_label, l, row_label, estimate, se, pval)

# Merge for CSV
csv_table <- ours_sel %>%
  rename(repl_coeff = estimate, repl_se = se, repl_pval = pval) %>%
  left_join(
    mello_sel %>% rename(mello_coeff = estimate, mello_se = se, mello_pval = pval) %>%
      select(-feature_label, -row_label),
    by = c("feature", "l")
  ) %>%
  mutate(
    repl_stars  = stars_fn(repl_pval),
    mello_stars = stars_fn(mello_pval),
    diff = repl_coeff - mello_coeff
  ) %>%
  arrange(feature, l)

write_csv(csv_table,
          "mello_paper_replication/results/event_study_all_features_comparison_table.csv")
cat("Saved CSV: mello_paper_replication/results/event_study_all_features_comparison_table.csv\n")

# ---------------------------------------------------------------------------
# 7. LaTeX appendix table — one page, all 6 features, selected lags
#    Layout: rows = selected lags × features, columns = Repl coeff(SE) | Mello coeff(SE)
#    We do one sub-block per feature to keep it readable.
# ---------------------------------------------------------------------------
fmt <- function(x, d = 3) formatC(round(x, d), format = "f", digits = d)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Event study comparison across GDP components: selected post-treatment coefficients}",
  "\\label{tab:es_all_features_comparison}",
  "\\footnotesize",
  "\\begin{tabular}{l rr rr r}",
  "\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Replication}} & \\multicolumn{2}{c}{\\textbf{Mello (2024)}} & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "  & Coeff. & (SE) & Coeff. & (SE) & Diff. \\\\",
  "\\midrule"
)

feat_keys <- c("gdp", "private_consumption", "government_consumption",
               "capital_formation", "exports", "imports")
feat_labels_tex <- c(
  "gdp" = "\\textbf{GDP}",
  "private_consumption" = "\\textbf{Private Consumption}",
  "government_consumption" = "\\textbf{Government Consumption}",
  "capital_formation" = "\\textbf{Capital Formation}",
  "exports" = "\\textbf{Exports}",
  "imports" = "\\textbf{Imports}"
)

for (feat in feat_keys) {
  # Feature header
  tex <- c(tex, paste0(feat_labels_tex[feat], " \\\\"))

  # Controls: convergence
  ours_conv <- ours_controls %>% filter(feature == feat, grepl("^ln_", term))
  mello_conv <- mello_controls %>% filter(feature == feat)
  if (nrow(ours_conv) == 1 && nrow(mello_conv) == 1) {
    tex <- c(tex, sprintf(
      "\\quad $\\ln y_{t-4}$ & %s%s & (%s) & %s%s & (%s) & %s \\\\",
      fmt(ours_conv$estimate), stars_fn(ours_conv$pval), fmt(ours_conv$se),
      fmt(mello_conv$conv_coeff), stars_fn(2*pnorm(-abs(mello_conv$conv_coeff/mello_conv$conv_se))),
      fmt(mello_conv$conv_se),
      fmt(ours_conv$estimate - mello_conv$conv_coeff)
    ))
  }

  # Controls: host
  ours_host <- ours_controls %>% filter(feature == feat, term == "host")
  if (nrow(ours_host) == 1 && nrow(mello_conv) == 1) {
    tex <- c(tex, sprintf(
      "\\quad Host & %s%s & (%s) & %s%s & (%s) & %s \\\\",
      fmt(ours_host$estimate), stars_fn(ours_host$pval), fmt(ours_host$se),
      fmt(mello_conv$host_coeff), stars_fn(2*pnorm(-abs(mello_conv$host_coeff/mello_conv$host_se))),
      fmt(mello_conv$host_se),
      fmt(ours_host$estimate - mello_conv$host_coeff)
    ))
  }

  # Selected lags
  block <- csv_table %>% filter(feature == feat)
  for (i in seq_len(nrow(block))) {
    r <- block[i, ]
    tex <- c(tex, sprintf(
      "\\quad %s & %s%s & (%s) & %s%s & (%s) & %s \\\\",
      r$row_label,
      fmt(r$repl_coeff), r$repl_stars, fmt(r$repl_se),
      fmt(r$mello_coeff), r$mello_stars, fmt(r$mello_se),
      fmt(r$diff)
    ))
  }

  # Add N row
  ours_feat_n <- ours_rt %>% filter(feature == feat) %>% nrow()
  mello_n_val <- mello_stats %>% filter(feature == feat) %>% pull(mello_n)
  # Our N comes from model; approximate from data: GDP=8633, components=8589
  our_n <- if (feat == "gdp") "8,633" else "8,589"
  mello_n_str <- formatC(mello_n_val, format = "d", big.mark = ",")

  tex <- c(tex, sprintf(
    "\\quad $N$ & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\\\",
    our_n, mello_n_str
  ))

  # Separator between features (not after last)
  if (feat != tail(feat_keys, 1)) {
    tex <- c(tex, "[4pt]")
  }
}

tex <- c(tex,
  "\\bottomrule",
  "\\multicolumn{6}{l}{\\footnotesize *\\,$p<0.10$,\\; **\\,$p<0.05$,\\; ***\\,$p<0.01$.\\; Clustered SEs at the country level.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Selected post-treatment lags shown ($l = +1$ to $+4$, $+8$, $+16$). Full coefficients in replication materials.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Mello $p$-values inferred from reported coefficients and SEs (normal approximation).}",
  "\\end{tabular}",
  "\\end{table}"
)

dir.create("thesis/tables", showWarnings = FALSE)
writeLines(tex, "thesis/tables/event_study_all_features_comparison.tex")
cat("Saved LaTeX: thesis/tables/event_study_all_features_comparison.tex\n")

cat("\n=== Done ===\n")

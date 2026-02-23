# ============================================================
# Semi-Finalist Event Study — GDP only (log YoY)
# Treatment: reaching the World Cup Semi-Finals (top 4)
#   = rank1 OR rank2 OR rank3 OR rank4
# Same specification as Mello (2024) replication event study
#
# Input:  Data/mello_paper_replication/paper_replication_sample.csv
# Output: semi_finalist_analysis/results/semi_finalist_event_study_coefficients.csv
#         semi_finalist_analysis/plots/semi_finalist_event_study_gdp.png
# ============================================================

rm(list = ls())

# ---------- 0) Packages ----------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fixest)
library(ggplot2)

# ---------- 1) Load data ----------
df0 <- read_csv(
  "Data/mello_paper_replication/paper_replication_sample.csv",
  show_col_types = FALSE
)

cat("Loaded:", nrow(df0), "rows,", n_distinct(df0$country), "countries\n")

# ---------- 2) Identify semi-finalist events (top 4 at WC) ----------
# rank1 = winner, rank2 = runner-up, rank3 = 3rd place, rank4 = 4th place
semi_finalist_events <- df0 %>%
  filter(rank1 == 1 | rank2 == 1 | rank3 == 1 | rank4 == 1) %>%
  select(country, year, qtr, tq) %>%
  rename(tq_event = tq) %>%
  distinct()

semi_finalist_countries <- sort(unique(semi_finalist_events$country))
cat("\nSemi-finalist countries:", paste(semi_finalist_countries, collapse = ", "), "\n")

cat("\nSemi-finalist events:\n")
print(as.data.frame(semi_finalist_events))

# ---------- 3) Prepare panel ----------
df <- df0 %>%
  mutate(
    is_semi_finalist = as.integer(country %in% semi_finalist_countries),
    host             = as.integer(host)
  ) %>%
  rename(dy_gdp = gdp_yoy_log_4q)

# ---------- 4) Assign relative time to nearest semi-finalist event ----------
assign_nearest_event <- function(tq_vec, event_tq_vec) {
  sapply(tq_vec, function(tq0) {
    diffs <- tq0 - event_tq_vec
    diffs[which.min(abs(diffs))]
  })
}

df <- df %>%
  group_by(country) %>%
  group_modify(~ {
    ctry <- .y$country[[1]]
    ev   <- semi_finalist_events %>% filter(country == ctry) %>% pull(tq_event)
    if (length(ev) > 0) {
      .x$rel_time <- assign_nearest_event(.x$tq, ev)
    } else {
      .x$rel_time <- 0L
    }
    .x
  }) %>%
  ungroup()

# Bin endpoints at ±16
df <- df %>%
  mutate(
    rel_time_bin = case_when(
      rel_time <= -16 ~ -16L,
      rel_time >=  16 ~  16L,
      TRUE            ~ as.integer(rel_time)
    )
  )

# ---------- 5) Complete-cases sample ----------
df_cc <- df %>%
  filter(!is.na(dy_gdp), !is.na(ln_gdp_l4), !is.na(host), !is.na(rel_time_bin))

cat("\n--- Estimation sample ---\n")
cat("  Observations:", nrow(df_cc), "\n")
cat("  Countries:   ", n_distinct(df_cc$country), "\n")
cat("  Semi-finalists:", sum(df_cc$is_semi_finalist == 1), "obs from",
    n_distinct(df_cc$country[df_cc$is_semi_finalist == 1]), "countries\n")

# ---------- 6) Estimate with fixest ----------
m <- feols(
  dy_gdp ~ i(rel_time_bin, is_semi_finalist, ref = 0) + host + ln_gdp_l4
         | country + tq,
  cluster = ~country,
  data    = df_cc
)

cat("\n--- FIXEST summary ---\n")
print(summary(m))
cat("N obs:", nobs(m), "   Within R²:", fitstat(m, "wr2")[[1]], "\n")

# ---------- 7) Extract coefficient table ----------
ct <- as.data.frame(coeftable(m))
ct$term <- rownames(ct)
rownames(ct) <- NULL

tab <- ct %>%
  transmute(
    term,
    estimate = Estimate,
    se       = `Std. Error`,
    tval     = `t value`,
    pval     = `Pr(>|t|)`
  ) %>%
  filter(term %in% c("host", "ln_gdp_l4") |
         str_detect(term, "rel_time_bin::")) %>%
  mutate(
    l = if_else(
      str_detect(term, "rel_time_bin::"),
      as.integer(str_extract(term, "(?<=rel_time_bin::)-?\\d+")),
      NA_integer_
    ),
    sort_key = case_when(
      term == "ln_gdp_l4" ~ -999L,
      term == "host"       ~ -998L,
      TRUE                 ~ l
    )
  ) %>%
  arrange(sort_key) %>%
  select(term, l, estimate, se, tval, pval)

cat("\n--- Semi-finalist event study coefficients ---\n")
print(as_tibble(tab), n = Inf)

# ---------- 8) Save results ----------
write_csv(tab, "semi_finalist_analysis/results/semi_finalist_event_study_coefficients.csv")
cat("\nSaved: semi_finalist_analysis/results/semi_finalist_event_study_coefficients.csv\n")

# ---------- 9) Event study plot ----------
es_plot_data <- tab %>% filter(!is.na(l))

# Add reference point at l = 0
es_plot_data <- bind_rows(
  es_plot_data,
  tibble(term = "ref", l = 0L, estimate = 0, se = 0, tval = 0, pval = 1)
) %>% arrange(l)

es_plot_data <- es_plot_data %>%
  mutate(ci_lo = estimate - 1.96 * se,
         ci_hi = estimate + 1.96 * se)

gg <- ggplot(es_plot_data, aes(x = l, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), color = "grey30",
                width = 0, linewidth = 0.4) +
  geom_point(color = "black", size = 2) +
  annotate("text", x = 3, y = Inf, label = "Post World Cup",
           vjust = 1.5, hjust = 0.3, size = 3, color = "grey30") +
  scale_x_continuous(breaks = seq(-16, 16, by = 2)) +
  labs(
    title = "GDP",
    x     = "Quarter to or from the World Cup",
    y     = "ATT"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave("semi_finalist_analysis/plots/semi_finalist_event_study_gdp.png",
       gg, width = 7, height = 4, dpi = 300)
ggsave("semi_finalist_analysis/plots/semi_finalist_event_study_gdp.pdf",
       gg, width = 7, height = 4, dpi = 300)
cat("Saved: semi_finalist_analysis/plots/semi_finalist_event_study_gdp.{png,pdf}\n")

# ---------- 10) Key summary statistics ----------
cat("\n========================================\n")
cat("  SEMI-FINALIST EVENT STUDY SUMMARY (GDP)\n")
cat("========================================\n")
cat("  N countries treated:", length(semi_finalist_countries), "\n")
cat("  N semi-finalist events: ", nrow(semi_finalist_events), "\n")
cat("  l=+1:", sprintf("%.3f (%.3f)", tab$estimate[tab$l == 1], tab$se[tab$l == 1]), "\n")
cat("  l=+2:", sprintf("%.3f (%.3f)", tab$estimate[tab$l == 2], tab$se[tab$l == 2]), "\n")

# Post-treatment average (l = 1 to 4)
post_coeffs <- tab %>% filter(l %in% 1:4)
cat("  Avg(l=1..4):", sprintf("%.3f", mean(post_coeffs$estimate)), "\n")
cat("========================================\n")

cat("\nDONE.\n")

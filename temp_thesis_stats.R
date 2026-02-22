library(readr)
library(dplyr)

df <- read_csv('Data/mello_paper_replication/paper_replication_event_study_sample.csv', show_col_types=FALSE)

cat("=== SAMPLE OVERVIEW ===\n")
cat("Total rows:", nrow(df), "\n")
cat("Countries:", n_distinct(df$country), "\n")
cat("Country list:", paste(sort(unique(df$country)), collapse=", "), "\n\n")

cat("Winner obs:", sum(df$winner == 1), "\n")
cat("Non-winner obs:", sum(df$winner == 0), "\n\n")

winners <- df %>% filter(winner == 1) %>% distinct(country)
cat("Winner countries:", paste(winners$country, collapse=", "), "\n\n")

ranges <- df %>% group_by(country) %>% summarise(min_q = min(quarter), max_q = max(quarter), n = n(), .groups='drop')
cat("=== COUNTRY COVERAGE ===\n")
print(ranges, n=60)

cat("\n=== FULL SAMPLE SUMMARY STATS ===\n")
features <- c('gdp', 'private_consumption', 'government_consumption', 'capital_formation', 'exports', 'imports', 'population')
yoy_features <- c('gdp_yoy_log_4q', 'private_consumption_yoy_log_4q', 'government_consumption_yoy_log_4q', 'capital_formation_yoy_log_4q', 'exports_yoy_log_4q', 'imports_yoy_log_4q')

for (f in c(features, yoy_features)) {
  w_mean <- mean(df[[f]][df$winner==1], na.rm=TRUE)
  w_sd <- sd(df[[f]][df$winner==1], na.rm=TRUE)
  nw_mean <- mean(df[[f]][df$winner==0], na.rm=TRUE)
  nw_sd <- sd(df[[f]][df$winner==0], na.rm=TRUE)
  cat(sprintf("%-45s W: %12.2f (%12.2f)  NW: %12.2f (%12.2f)\n", f, w_mean, w_sd, nw_mean, nw_sd))
}

cat("\n=== WC EVENTS ===\n")
wc_events <- df %>% filter(winner == 1) %>% distinct(country, year) %>% arrange(year)
cat("Winner events:\n")
print(wc_events, n=20)

host_events <- df %>% filter(host == 1) %>% distinct(country, year) %>% arrange(year)
cat("\nHost events:\n")
print(host_events, n=20)
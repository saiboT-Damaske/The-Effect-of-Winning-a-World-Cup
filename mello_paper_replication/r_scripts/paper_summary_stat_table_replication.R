library(readr)
library(dplyr)
library(stringr)
library(tidyr)

df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)

df2 <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    population_m = population / 1e6,
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    )
  )

# country-level winner status based on ever having rank1==1
winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(winner_group = if_else(is_winner_country, "Winner", "Non-winner"))

summ_block <- function(d) {
  d <- d %>% filter(!is.na(population_m))
  tibble(
    mean = mean(d$population_m, na.rm = TRUE),
    sd   = sd(d$population_m, na.rm = TRUE),
    n_obs = sum(!is.na(d$population_m)),
    n_cty = n_distinct(d$country)
  )
}

# subperiods
pop_sub <- df2 %>%
  filter(!is.na(period)) %>%
  group_by(period, winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup()

# full sample
pop_full <- df2 %>%
  group_by(winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup() %>%
  mutate(period = "Full sample")

pop_all <- bind_rows(pop_sub, pop_full) %>%
  mutate(mean_sd = sprintf("%.2f (%.2f)", mean, sd))

# pretty wide table
pop_pretty <- pop_all %>%
  select(period, winner_group, mean_sd, n_cty, n_obs) %>%
  pivot_wider(
    names_from = winner_group,
    values_from = c(mean_sd, n_cty, n_obs),
    names_glue = "{winner_group}_{.value}"
  ) %>%
  arrange(factor(period, levels = c("1960–80","1980–2000","2000–20","Full sample")))

print(pop_pretty, n = Inf)



##############
# GDP
##############

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)

# --- detect GDP column (use the first column that contains "gross_domestic_product")
gdp_col <- names(df)[str_detect(names(df), "gross_domestic_product")][1]
stopifnot(!is.na(gdp_col))

df2 <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    ),
    # Your GDP is in "millions" -> table wants "thousands of millions" => divide by 1000
    gdp_tbl_units = .data[[gdp_col]] / 1000
  )

# country-level winner status based on ever having rank1==1
winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(winner_group = if_else(is_winner_country, "Winner", "Non-winner"))

summ_block <- function(d) {
  d <- d %>% filter(!is.na(gdp_tbl_units))
  tibble(
    mean = mean(d$gdp_tbl_units, na.rm = TRUE),
    sd   = sd(d$gdp_tbl_units, na.rm = TRUE),
    n_obs = sum(!is.na(d$gdp_tbl_units)),
    n_cty = n_distinct(d$country)
  )
}

# subperiods
gdp_sub <- df2 %>%
  filter(!is.na(period)) %>%
  group_by(period, winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup()

# full sample
gdp_full <- df2 %>%
  group_by(winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup() %>%
  mutate(period = "Full sample")

gdp_all <- bind_rows(gdp_sub, gdp_full) %>%
  mutate(mean_sd = sprintf("%.2f (%.2f)", mean, sd))

gdp_pretty <- gdp_all %>%
  select(period, winner_group, mean_sd, n_cty, n_obs) %>%
  pivot_wider(
    names_from = winner_group,
    values_from = c(mean_sd, n_cty, n_obs),
    names_glue = "{winner_group}_{.value}"
  ) %>%
  arrange(factor(period, levels = c("1960–80","1980–2000","2000–20","Full sample")))

print(gdp_pretty, n = Inf)
cat("\nGDP column used:", gdp_col, "\n")



##############
# GDP YoY log(4q) comparison table
##############

library(readr)
library(dplyr)
library(stringr)
library(tidyr)


df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)
df <- read_csv("../data/mello_paper_replication/paper_replication_dataset_q_1961_2021.csv", show_col_types = FALSE)


target_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_log_4q"
stopifnot(target_col %in% names(df))

df2 <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    ),
    gdp_yoy_log_4q = .data[[target_col]]
  )

# country-level winner status based on ever having rank1==1
winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(winner_group = if_else(is_winner_country, "Winner", "Non-winner"))

# --- Comparison-table block: mean (sd), #countries, #obs
summ_block <- function(d) {
  d <- d %>% filter(!is.na(gdp_yoy_log_4q))
  tibble(
    mean  = mean(d$gdp_yoy_log_4q, na.rm = TRUE),
    sd    = sd(d$gdp_yoy_log_4q, na.rm = TRUE),
    n_obs = sum(!is.na(d$gdp_yoy_log_4q)),
    n_cty = n_distinct(d$country)
  )
}

gdp_sub <- df2 %>%
  filter(!is.na(period)) %>%
  group_by(period, winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup()

gdp_full <- df2 %>%
  group_by(winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup() %>%
  mutate(period = "Full sample")

gdp_all <- bind_rows(gdp_sub, gdp_full) %>%
  mutate(mean_sd = sprintf("%.4f (%.4f)", mean, sd))

gdp_pretty <- gdp_all %>%
  select(period, winner_group, mean_sd, n_cty, n_obs) %>%
  pivot_wider(
    names_from = winner_group,
    values_from = c(mean_sd, n_cty, n_obs),
    names_glue = "{winner_group}_{.value}"
  ) %>%
  arrange(factor(period, levels = c("1960–80","1980–2000","2000–20","Full sample")))

print(gdp_pretty, n = Inf)
cat("\nColumn used:", target_col, "\n")




##############
# GDP per capita – summary statistics table
##############

library(dplyr)
library(stringr)
library(tidyr)

df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE) 
df_es <- df

gdp_level_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
stopifnot(
  gdp_level_col %in% names(df_es),
  "population" %in% names(df_es)
)

df2 <- df_es %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    ),
    # GDP is in "millions" -> convert to USD before per-capita
    gdp_pc = .data[[gdp_level_col]] * 1e6 / population
  )

# winner status (country-level, ever won)
winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(winner_group = if_else(is_winner_country, "Winner", "Non-winner"))

# summary block (FIXED)
summ_block <- function(d) {
  d <- d %>% filter(!is.na(.data[["gdp_pc"]]))
  tibble(
    mean  = mean(d$gdp_pc, na.rm = TRUE),
    sd    = sd(d$gdp_pc, na.rm = TRUE),
    n_obs = sum(!is.na(d$gdp_pc)),
    n_cty = n_distinct(d$country)
  )
}
# subperiods
gdp_pc_sub <- df2 %>%
  filter(!is.na(period)) %>%
  group_by(period, winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup()

# full sample
gdp_pc_full <- df2 %>%
  group_by(winner_group) %>%
  group_modify(~ summ_block(.x)) %>%
  ungroup() %>%
  mutate(period = "Full sample")

gdp_pc_all <- bind_rows(gdp_pc_sub, gdp_pc_full) %>%
  mutate(mean_sd = sprintf("%s (%s)",
                           formatC(mean, format="f", digits=0, big.mark=","),
                           formatC(sd,   format="f", digits=0, big.mark=",")))



gdp_pc_pretty <- gdp_pc_all %>%
  select(period, winner_group, mean_sd, n_cty, n_obs) %>%
  pivot_wider(
    names_from = winner_group,
    values_from = c(mean_sd, n_cty, n_obs),
    names_glue = "{winner_group}_{.value}"
  ) %>%
  arrange(factor(period, levels = c("1960–80","1980–2000","2000–20","Full sample")))

print(gdp_pc_pretty, n = Inf)
cat("\nGDP per capita = ", gdp_level_col, " / population\n", sep = "")





################################################################################
#           FULL TABLE    LATEX (df version below)                            ##
################################################################################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(kableExtra)

df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)

# ---- Variables (edit here only if needed) ----
gdp_level_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
gdp_yoy_pct_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct"

stopifnot(
  all(c("country","quarter","population","rank1") %in% names(df)),
  gdp_level_col %in% names(df),
  gdp_yoy_pct_col %in% names(df)
)

# ---- Prep: periods + winner group + transformed measures ----
df2 <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    )
  )

winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(
    winner_group = if_else(is_winner_country, "Winner", "Non-winner"),
    # Table units:
    gdp_tbl = .data[[gdp_level_col]] / 1000,         # thousands of (USD millions)
    pop_m   = population / 1e6,                      # millions
    gdp_pc  = .data[[gdp_level_col]] * 1e6 / population,  # USD per capita
    gdp_yoy = .data[[gdp_yoy_pct_col]]               # YoY percent
  )

# ---- Helpers ----
stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

fmt_num <- function(x, digits = 2, big = FALSE) {
  if (is.na(x)) return("")
  if (big) formatC(x, format = "f", digits = digits, big.mark = ",")
  else     formatC(x, format = "f", digits = digits)
}

row_block <- function(data, var, digits_mean = 2, digits_sd = 2, big = FALSE) {
  d <- data %>% filter(!is.na(.data[[var]]))
  
  # winner stats
  w <- d %>% filter(winner_group == "Winner")
  n <- d %>% filter(winner_group == "Non-winner")
  
  w_mean <- mean(w[[var]], na.rm = TRUE); w_sd <- sd(w[[var]], na.rm = TRUE)
  n_mean <- mean(n[[var]], na.rm = TRUE); n_sd <- sd(n[[var]], na.rm = TRUE)
  
  # t-test on observations (as in typical Table 1)
  tt <- tryCatch(t.test(w[[var]], n[[var]]), error = function(e) NULL)
  tstat <- if (is.null(tt)) NA_real_ else unname(tt$statistic)
  pval  <- if (is.null(tt)) NA_real_ else tt$p.value
  
  tibble(
    Winner_Mean     = fmt_num(w_mean, digits_mean, big),
    Winner_SD       = paste0("(", fmt_num(w_sd, digits_sd, big), ")"),
    `Non-winner_Mean` = fmt_num(n_mean, digits_mean, big),
    `Non-winner_SD`   = paste0("(", fmt_num(n_sd, digits_sd, big), ")"),
    t_test          = ifelse(is.na(tstat), "", paste0(fmt_num(tstat, 2, FALSE), stars(pval)))
  )
}

# ---- Build Table 1 content ----
period_levels <- c("1960–80","1980–2000","2000–20","Full sample")

make_panel <- function(p) {
  d <- if (p == "Full sample") df2 else df2 %>% filter(period == p)
  
  panel <- bind_rows(
    tibble(row = "GDP (in thousands of 2015 US dollar millions)") %>% bind_cols(row_block(d, "gdp_tbl", 2, 2, big = FALSE)),
    tibble(row = "Population (in millions)")                      %>% bind_cols(row_block(d, "pop_m",   2, 2, big = FALSE)),
    tibble(row = "GDP per capita")                                %>% bind_cols(row_block(d, "gdp_pc",  2, 2, big = TRUE)),
    tibble(row = "Year-on-Year GDP growth")                       %>% bind_cols(row_block(d, "gdp_yoy", 2, 2, big = FALSE))
  ) %>%
    mutate(period = p)
  
  # add the two bottom lines only for Full sample (to match the paper layout)
  if (p == "Full sample") {
    n_cty_w <- d %>% filter(winner_group == "Winner") %>% summarise(n = n_distinct(country)) %>% pull(n)
    n_cty_n <- d %>% filter(winner_group == "Non-winner") %>% summarise(n = n_distinct(country)) %>% pull(n)
    n_obs_w <- d %>% filter(winner_group == "Winner") %>% summarise(n = sum(!is.na(country))) %>% pull(n)
    n_obs_n <- d %>% filter(winner_group == "Non-winner") %>% summarise(n = sum(!is.na(country))) %>% pull(n)
    
    panel <- bind_rows(
      panel,
      tibble(
        period = p, row = "Number of countries",
        Winner_Mean = as.character(n_cty_w), Winner_SD = "",
        `Non-winner_Mean` = as.character(n_cty_n), `Non-winner_SD` = "",
        t_test = ""
      ),
      tibble(
        period = p, row = "Number of observations",
        Winner_Mean = as.character(n_obs_w), Winner_SD = "",
        `Non-winner_Mean` = as.character(n_obs_n), `Non-winner_SD` = "",
        t_test = ""
      )
    )
  }
  
  panel
}

tab_long <- bind_rows(lapply(period_levels, make_panel)) %>%
  mutate(period = factor(period, levels = period_levels))

# ---- Render (paper-like) ----
tab_print <- tab_long %>%
  select(period, row,
         Winner_Mean, Winner_SD,
         `Non-winner_Mean`, `Non-winner_SD`,
         t_test)

kbl <- tab_print %>%
  select(-period) %>%
  kable(
    format = "latex", booktabs = TRUE, linesep = "",
    col.names = c("", "Mean", "SD", "Mean", "SD", "t-test"),
    escape = TRUE,
    caption = "Summary statistics for the event-study sample"
  ) %>%
  add_header_above(c(" " = 1, "Winner" = 2, "Non-winner" = 2, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position"))

# group headers per period (like the paper)
start_rows <- cumsum(c(1, head(as.integer(table(tab_long$period)), -1)))
end_rows   <- cumsum(as.integer(table(tab_long$period)))
period_names <- levels(tab_long$period)

for (i in seq_along(period_names)) {
  kbl <- kbl %>%
    pack_rows(period_names[i], start_row = start_rows[i], end_row = end_rows[i], bold = TRUE, indent = FALSE)
}

kbl



################################################################################
#           FULL TABLE    DF                                                  ##
################################################################################

df <- read_csv("../data/mello_paper_replication/paper_replication_event_study_sample.csv", show_col_types = FALSE)

# --- columns ---
gdp_level_col  <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted"
gdp_yoy_pct_col <- "gross_domestic_product_chain_linked_volume_rebased_us_dollars_ppp_converted_yoy_pct"

stopifnot(
  all(c("country","quarter","population","rank1") %in% names(df)),
  gdp_level_col %in% names(df),
  gdp_yoy_pct_col %in% names(df)
)

# --- helpers ---
stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

fmt_mean_sd <- function(mean, sd, digits = 2, big = FALSE) {
  f <- function(x) {
    if (big) formatC(x, format = "f", digits = digits, big.mark = ",")
    else     formatC(x, format = "f", digits = digits)
  }
  paste0(f(mean), " (", f(sd), ")")
}

summ_row <- function(d, var, label, digits = 2, big = FALSE) {
  d <- d %>% filter(!is.na(.data[[var]]))
  
  w <- d %>% filter(winner_group == "Winner")
  n <- d %>% filter(winner_group == "Non-winner")
  
  w_mean <- mean(w[[var]], na.rm = TRUE); w_sd <- sd(w[[var]], na.rm = TRUE)
  n_mean <- mean(n[[var]], na.rm = TRUE); n_sd <- sd(n[[var]], na.rm = TRUE)
  
  tt <- tryCatch(t.test(w[[var]], n[[var]]), error = function(e) NULL)
  tstat <- if (is.null(tt)) NA_real_ else unname(tt$statistic)
  pval  <- if (is.null(tt)) NA_real_ else tt$p.value
  
  tibble(
    Row = label,
    Winner = fmt_mean_sd(w_mean, w_sd, digits = digits, big = big),
    `Non-winner` = fmt_mean_sd(n_mean, n_sd, digits = digits, big = big),
    `t-test` = ifelse(is.na(tstat), "", paste0(formatC(tstat, format="f", digits=2), stars(pval)))
  )
}

# --- prep data ---
df2 <- df %>%
  mutate(
    year = as.integer(str_extract(quarter, "\\d{4}")),
    period = case_when(
      year >= 1960 & year <= 1980 ~ "1960–80",
      year > 1980  & year <= 2000 ~ "1980–2000",
      year > 2000  & year <= 2020 ~ "2000–20",
      TRUE ~ NA_character_
    )
  )

winner_by_country <- df2 %>%
  group_by(country) %>%
  summarise(is_winner_country = any(rank1 == 1, na.rm = TRUE), .groups = "drop")

df2 <- df2 %>%
  left_join(winner_by_country, by = "country") %>%
  mutate(
    winner_group = if_else(is_winner_country, "Winner", "Non-winner"),
    # units to match paper table:
    gdp_tbl = .data[[gdp_level_col]] / 1000,          # thousands of (USD millions)
    pop_m   = population / 1e6,                       # millions
    gdp_pc  = .data[[gdp_level_col]] * 1e6 / population, # USD per capita
    gdp_yoy = .data[[gdp_yoy_pct_col]]                # YoY percent
  )

period_levels <- c("1960–80","1980–2000","2000–20","Full sample")

make_period_block <- function(p) {
  d <- if (p == "Full sample") df2 else df2 %>% filter(period == p)
  
  block <- bind_rows(
    summ_row(d, "gdp_tbl", "GDP (in thousands of 2015 US dollar millions)", digits = 2, big = FALSE),
    summ_row(d, "pop_m",   "Population (in millions)",                     digits = 2, big = FALSE),
    summ_row(d, "gdp_pc",  "GDP per capita",                               digits = 2, big = TRUE),
    summ_row(d, "gdp_yoy", "Year-on-Year GDP growth",                      digits = 2, big = FALSE)
  ) %>%
    mutate(Period = p, .before = Row)
  
  if (p == "Full sample") {
    n_cty_w <- d %>% filter(winner_group=="Winner") %>% summarise(n=n_distinct(country)) %>% pull(n)
    n_cty_n <- d %>% filter(winner_group=="Non-winner") %>% summarise(n=n_distinct(country)) %>% pull(n)
    n_obs_w <- d %>% filter(winner_group=="Winner") %>% summarise(n=n()) %>% pull(n)
    n_obs_n <- d %>% filter(winner_group=="Non-winner") %>% summarise(n=n()) %>% pull(n)
    
    block <- bind_rows(
      block,
      tibble(Period=p, Row="Number of countries",
             Winner=as.character(n_cty_w), `Non-winner`=as.character(n_cty_n), `t-test`=""),
      tibble(Period=p, Row="Number of observations",
             Winner=as.character(n_obs_w), `Non-winner`=as.character(n_obs_n), `t-test`="")
    )
  }
  
  block
}

table1_df <- bind_rows(lapply(period_levels, make_period_block)) %>%
  mutate(Period = factor(Period, levels = period_levels)) %>%
  arrange(Period)

print(table1_df, n = Inf)
# View(table1_df)

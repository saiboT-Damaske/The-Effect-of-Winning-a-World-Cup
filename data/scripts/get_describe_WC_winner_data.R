library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# read generated dataset
wc <- read_csv("Data/world_cup/world_cup_results_wide_entities.csv", show_col_types = FALSE)

# keep countries only (drop EU/G7/G20)
wc_countries <- wc %>%
  filter(entity_type == "country")

# pivot to long: WC year + role
wc_long <- wc_countries %>%
  pivot_longer(
    cols = starts_with("WC_"),
    names_to = "wc_role",
    values_to = "value"
  ) %>%
  filter(value == 1)

# extract year + role
wc_long <- wc_long %>%
  mutate(
    year = as.integer(str_extract(wc_role, "\\d{4}")),
    role = str_remove(wc_role, "WC_\\d{4}_")
  )

wc_check_pretty <- wc_long %>%
  select(year, role, entity_name) %>%
  group_by(year, role) %>%
  summarise(
    entity_name = paste(entity_name, collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = role,
    values_from = entity_name
  ) %>%
  arrange(year)

print(wc_check_pretty, n = Inf)




#### add host victory
library(readr)
library(dplyr)
library(stringr)

wc <- read_csv("Data/world_cup/world_cup_results_wide_entities.csv", show_col_types = FALSE)

years <- wc %>%
  select(starts_with("WC_")) %>%
  names() %>%
  str_extract("\\d{4}") %>%
  na.omit() %>%
  unique() %>%
  sort()

for (y in years) {
  host_col <- paste0("WC_", y, "_host")
  r1 <- paste0("WC_", y, "_rank1")
  r2 <- paste0("WC_", y, "_rank2")
  r3 <- paste0("WC_", y, "_rank3")
  r4 <- paste0("WC_", y, "_rank4")
  
  if (all(c(host_col, r1, r2, r3, r4) %in% names(wc))) {
    
    wc[[paste0("WC_", y, "_host_won")]] <-
      as.integer(wc[[host_col]] == 1 & wc[[r1]] == 1)
    
    wc[[paste0("WC_", y, "_host_top4")]] <-
      as.integer(wc[[host_col]] == 1 &
                   (wc[[r1]] + wc[[r2]] + wc[[r3]] + wc[[r4]] > 0))
  }
}

write_csv(wc, "Data/world_cup/world_cup_results_wide_entities.csv")

wc %>%
  filter(entity_type == "country") %>%
  select(entity_name, matches("^WC_\\d{4}_host_(won|top4)$")) %>%
  pivot_longer(-entity_name, names_to = "col", values_to = "v") %>%
  filter(v == 1) %>%
  mutate(year = str_extract(col, "\\d{4}")) %>%
  arrange(year, col, entity_name)

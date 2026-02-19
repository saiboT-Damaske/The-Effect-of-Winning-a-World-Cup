# install.packages(c("readr","dplyr","tidyr","janitor","stringr"))
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)


imf <- read.csv("IMF_Data_col.csv")
head(imf)


# identify time columns (X1950.Q1 ... X2025.Q4)
time_cols <- grep("^X\\d{4}\\.Q[1-4]$", names(imf), value = TRUE)

country_coverage <- imf %>%
  select(COUNTRY, all_of(time_cols)) %>%
  pivot_longer(
    cols = all_of(time_cols),
    names_to = "quarter",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(COUNTRY) %>%
  summarise(
    first_quarter = min(quarter),
    last_quarter  = max(quarter),
    .groups = "drop"
  ) %>%
  arrange(COUNTRY)

country_coverage %>% print(n=200)

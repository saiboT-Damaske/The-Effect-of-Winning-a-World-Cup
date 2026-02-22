library(readr)
library(dplyr)
library(janitor)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

base <- "https://sdmx.oecd.org/public/rest/data"
flow <- "OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0"

url_pop <- paste0(
  base, "/", flow, "/all",
  "?startPeriod=1960&endPeriod=2024",
  "&dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

pop_raw <- read_csv(url_pop, show_col_types = FALSE)
pop <- pop_raw %>% clean_names()
pop_total <- pop_raw %>%
  filter(
    MEASURE == "POP",      # total population series
    SEX == "_T",           # total
    AGE == "_T"            # total
  ) %>%
  transmute(
    country_code = REF_AREA,
    country_name = `Reference area`,
    year = as.integer(TIME_PERIOD),
    population_million = OBS_VALUE,          # check unit below
    population = OBS_VALUE * 1e6,            # if unit is “Millions”
    unit_measure = UNIT_MEASURE,
    source = "OECD.ELS.SAE:DSD_POPULATION@DF_POP_HIST"
  )

# quick unit check (important)
pop_total %>% count(unit_measure)

write_csv(pop_total, "Data/oecd_source/oecd_population_annual_1960_2024.csv")



pop <- read_csv("Data/oecd_source/oecd_population_annual_1960_2024.csv", show_col_types = FALSE)

pop_coverage <- pop %>%
  filter(!is.na(population)) %>%
  group_by(country_code, country_name) %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year   = max(year, na.rm = TRUE),
    n_years    = sum(!is.na(population)),
    coverage   = paste0(start_year, "-", end_year),
    .groups = "drop"
  ) %>%
  arrange(country_code)

print(pop_coverage, n = Inf)



library(readr)
library(dplyr)
library(ggplot2)

pop <- read_csv("Data/oecd_source/oecd_population_annual_1960_2024.csv", show_col_types = FALSE)

# pick first five countries alphabetically (stable & reproducible)
countries_5 <- pop %>%
  distinct(country_code) %>%
  arrange(country_code) %>%
  slice(1:5) %>%
  pull(country_code)

pop_5 <- pop %>%
  filter(country_code %in% countries_5)

ggplot(pop_5, aes(x = year, y = population, color = country_code)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(1960, 2020, 20)) +
  labs(
    title = "Population timelines (first five countries)",
    x = "Year",
    y = "Population",
    color = "Country"
  ) +
  theme_minimal()

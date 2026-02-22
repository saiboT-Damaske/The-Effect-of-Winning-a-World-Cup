############################################################
# World Cup Underperformance Dataset (ELO + FIFA comparison)
# ========================================================
#
# Definition: "underperformed" = a team ranked in the TOP 10
#   by ELO rating (among WC participants) before the tournament
#   that was eliminated in the group stage.
#
# Data sources:
#   - Pre-tournament ELO ratings: eloratings.net (TSV endpoint)
#     https://www.eloratings.net/YYYY_World_Cup_start.tsv
#   - FIFA World Rankings (1994–2022): inside.fifa.com API
#     https://inside.fifa.com/api/ranking-overview?locale=en&dateId=...
#   - World Cup group stage results: compiled from historical
#     records (Wikipedia / FIFA archives)
#
# Coverage: 1962–2022 (16 FIFA World Cups)
#   ELO: all 16 WCs (1962–2022)
#   FIFA: 8 WCs (1994–2022, rankings started Dec 1992)
#
# Output:
#   Data/world_cup/wc_elo_all_participants.csv
#   Data/world_cup/wc_underperformance_elo.csv
############################################################

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)

# Working directory = repo root (~/The-Effect-of-Winning-a-World-Cup)
# Set once in your R session, or uncomment:
# setwd("~/The-Effect-of-Winning-a-World-Cup")

# ==========================================================
# 1) World Cup years (post-1960)
# ==========================================================
wc_years <- c(1962, 1966, 1970, 1974, 1978, 1982,
              1986, 1990, 1994, 1998, 2002, 2006,
              2010, 2014, 2018, 2022)

# ==========================================================
# 2) ELO 2-letter code → ISO3 mapping
#    (eloratings.net uses custom 2-letter codes)
# ==========================================================
elo_to_iso3 <- c(
  # Standard nations (active in dataset)
  "AE" = "ARE",  # UAE
  "AO" = "AGO",  # Angola
  "AR" = "ARG",  # Argentina
  "AT" = "AUT",  # Austria
  "AU" = "AUS",  # Australia
  "BA" = "BIH",  # Bosnia and Herzegovina
  "BE" = "BEL",  # Belgium
  "BG" = "BGR",  # Bulgaria
  "BO" = "BOL",  # Bolivia
  "BR" = "BRA",  # Brazil
  "CA" = "CAN",  # Canada
  "CH" = "CHE",  # Switzerland
  "CI" = "CIV",  # Côte d'Ivoire
  "CL" = "CHL",  # Chile
  "CM" = "CMR",  # Cameroon
  "CN" = "CHN",  # China
  "CO" = "COL",  # Colombia
  "CR" = "CRI",  # Costa Rica
  "CS" = "CZE",  # Czechoslovakia → Czech Republic
  "CZ" = "CZE",  # Czech Republic
  "DD" = "DEU",  # East Germany → Germany
  "DE" = "DEU",  # Germany
  "DK" = "DNK",  # Denmark
  "DZ" = "DZA",  # Algeria
  "EC" = "ECU",  # Ecuador
  "EG" = "EGY",  # Egypt
  "EI" = "GBR",
  "EN" = "GBR",  # England → United Kingdom
  "ES" = "ESP",  # Spain
  "FR" = "FRA",  # France
  "GH" = "GHA",  # Ghana
  "GR" = "GRC",  # Greece
  "HN" = "HND",  # Honduras
  "HR" = "HRV",  # Croatia
  "HT" = "HTI",  # Haiti
  "HU" = "HUN",  # Hungary
  "IC" = "ISL",  # Iceland
  "IE" = "IRL",  # Republic of Ireland
  "IL" = "ISR",  # Israel
  "IQ" = "IRQ",  # Iraq
  "IR" = "IRN",  # Iran
  "IT" = "ITA",  # Italy
  "JM" = "JAM",  # Jamaica
  "JP" = "JPN",  # Japan
  "KP" = "PRK",  # North Korea
  "KR" = "KOR",  # South Korea
  "KW" = "KWT",  # Kuwait
  "MA" = "MAR",  # Morocco
  "MX" = "MEX",  # Mexico
  "NG" = "NGA",  # Nigeria
  "NL" = "NLD",  # Netherlands
  "NO" = "NOR",  # Norway
  "NZ" = "NZL",  # New Zealand
  "PA" = "PAN",  # Panama
  "PE" = "PER",  # Peru
  "PL" = "POL",  # Poland
  "PT" = "PRT",  # Portugal
  "PY" = "PRY",  # Paraguay
  "QA" = "QAT",  # Qatar
  "RM" = "SRB",  # Serbia and Montenegro → Serbia
  "RO" = "ROU",  # Romania
  "RS" = "SRB",  # Serbia
  "RU" = "RUS",  # Russia
  "SA" = "SAU",  # Saudi Arabia
  "SE" = "SWE",  # Sweden
  "SI" = "SVN",  # Slovenia
  "SK" = "SVK",  # Slovakia
  "SN" = "SEN",  # Senegal
  "SQ" = "GBR",  # Scotland → United Kingdom
  "SU" = "RUS",  # Soviet Union → Russia
  "SV" = "SLV",  # El Salvador
  "TG" = "TGO",  # Togo
  "TN" = "TUN",  # Tunisia
  "TR" = "TUR",  # Turkey
  "TT" = "TTO",  # Trinidad and Tobago
  "UA" = "UKR",  # Ukraine
  "US" = "USA",  # United States
  "UY" = "URY",  # Uruguay
  "WA" = "GBR",  # Wales → United Kingdom
  "WG" = "DEU",  # West Germany → Germany
  "YU" = "SRB",  # Yugoslavia → Serbia
  "ZA" = "ZAF",  # South Africa
  "ZR" = "COD"   # Zaire → DR Congo
)

# Human-readable team names for the ELO codes
elo_to_name <- c(
  "AE" = "UAE", "AO" = "Angola", "AR" = "Argentina", "AT" = "Austria",
  "AU" = "Australia", "BA" = "Bosnia and Herzegovina", "BE" = "Belgium",
  "BG" = "Bulgaria", "BO" = "Bolivia", "BR" = "Brazil", "CA" = "Canada",
  "CH" = "Switzerland", "CI" = "Côte d'Ivoire", "CL" = "Chile",
  "CM" = "Cameroon", "CN" = "China", "CO" = "Colombia", "CR" = "Costa Rica",
  "CS" = "Czechoslovakia", "CZ" = "Czech Republic", "DD" = "East Germany",
  "DE" = "Germany", "DK" = "Denmark", "DZ" = "Algeria", "EC" = "Ecuador",
  "EG" = "Egypt", "EI" = "Northern Ireland", "EN" = "England",
  "ES" = "Spain", "FR" = "France", "GH" = "Ghana", "GR" = "Greece",
  "HN" = "Honduras", "HR" = "Croatia", "HT" = "Haiti", "HU" = "Hungary",
  "IC" = "Iceland", "IE" = "Republic of Ireland", "IL" = "Israel",
  "IQ" = "Iraq", "IR" = "Iran", "IT" = "Italy", "JM" = "Jamaica",
  "JP" = "Japan", "KP" = "North Korea", "KR" = "South Korea",
  "KW" = "Kuwait", "MA" = "Morocco", "MX" = "Mexico", "NG" = "Nigeria",
  "NL" = "Netherlands", "NO" = "Norway", "NZ" = "New Zealand",
  "PA" = "Panama", "PE" = "Peru", "PL" = "Poland", "PT" = "Portugal",
  "PY" = "Paraguay", "QA" = "Qatar", "RM" = "Serbia and Montenegro",
  "RO" = "Romania", "RS" = "Serbia", "RU" = "Russia", "SA" = "Saudi Arabia",
  "SE" = "Sweden", "SI" = "Slovenia", "SK" = "Slovakia", "SN" = "Senegal",
  "SQ" = "Scotland", "SU" = "Soviet Union", "SV" = "El Salvador",
  "TG" = "Togo", "TN" = "Tunisia", "TR" = "Turkey",
  "TT" = "Trinidad and Tobago", "UA" = "Ukraine", "US" = "USA",
  "UY" = "Uruguay", "WA" = "Wales", "WG" = "West Germany",
  "YU" = "Yugoslavia", "ZA" = "South Africa", "ZR" = "Zaire"
)

# ==========================================================
# 3) Scrape pre-tournament ELO ratings from eloratings.net
#    Uses the undocumented .tsv endpoint (tab-separated).
#    Columns: local_rank, global_rank, elo_code, rating, ...
# ==========================================================
scrape_wc_elo <- function(wc_year, pause = 0.3) {
  url <- paste0("https://www.eloratings.net/", wc_year, "_World_Cup_start.tsv")
  cat("  Fetching ELO for", wc_year, "...\n")

  raw <- tryCatch(
    readLines(url, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      warning("  Failed to fetch ", url, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(raw) || length(raw) == 0) return(NULL)

  # Parse tab-separated: col1=local_rank, col3=elo_code, col4=rating
  parsed <- lapply(raw, function(line) {
    cols <- strsplit(line, "\t")[[1]]
    if (length(cols) >= 4) {
      tibble(
        elo_rank = as.integer(cols[1]),
        elo_code = trimws(cols[3]),
        elo_rating = as.integer(cols[4])
      )
    } else {
      NULL
    }
  })

  df <- bind_rows(parsed) %>%
    filter(!is.na(elo_rank)) %>%
    mutate(wc_year = wc_year)

  Sys.sleep(pause)
  return(df)
}

cat("\n=== Downloading pre-tournament ELO ratings ===\n")
elo_all <- bind_rows(lapply(wc_years, scrape_wc_elo))

cat("\nELO data: ", nrow(elo_all), " team-WC rows across ",
    n_distinct(elo_all$wc_year), " tournaments\n\n")

# Map codes to ISO3 + name
elo_all <- elo_all %>%
  mutate(
    iso3      = elo_to_iso3[elo_code],
    team_name = elo_to_name[elo_code]
  )

# Check for unmapped codes
unmapped <- elo_all %>% filter(is.na(iso3)) %>% distinct(elo_code)
if (nrow(unmapped) > 0) {
  cat("WARNING: Unmapped ELO codes:\n")
  print(unmapped)
}

# ==========================================================
# 4) Teams that ADVANCED past the group stage
#    (knockout round teams for each WC)
#    Source: FIFA / Wikipedia historical records
#
# Any WC participant NOT in this list = group_stage_exit
# ==========================================================
advanced_teams <- list(
  # 1962 Chile (16 teams → 8 to QF)
  "1962" = c("BR", "EN", "CS", "HU", "SU", "WG", "YU", "CL"),

  # 1966 England (16 teams → 8 to QF)
  "1966" = c("EN", "WG", "PT", "SU", "AR", "HU", "UY", "KP"),

  # 1970 Mexico (16 teams → 8 to QF)
  "1970" = c("BR", "EN", "IT", "WG", "UY", "SU", "MX", "PE"),

  # 1974 West Germany (16 teams → 8 to 2nd round)
  "1974" = c("WG", "DD", "YU", "BR", "NL", "SE", "PL", "AR"),

  # 1978 Argentina (16 teams → 8 to 2nd round)
  "1978" = c("WG", "NL", "BR", "IT", "PL", "AR", "PE", "AT"),

  # 1982 Spain (24 teams → 12 to 2nd group round)
  "1982" = c("PL", "IT", "WG", "AT", "BE", "AR", "EN", "FR",
             "EI", "ES", "BR", "SU"),

  # 1986 Mexico (24 teams → 16 to R16)
  "1986" = c("MX", "BG", "BE", "SU", "BR", "PL", "AR", "UY",
             "FR", "IT", "WG", "MA", "EN", "PY", "ES", "DK"),

  # 1990 Italy (24 teams → 16 to R16)
  "1990" = c("IT", "UY", "WG", "NL", "CS", "CR", "CM", "CO",
             "IE", "RO", "ES", "YU", "AR", "BR", "EN", "BE"),

  # 1994 USA (24 teams → 16 to R16)
  "1994" = c("DE", "BE", "ES", "CH", "SA", "SE", "RO", "AR",
             "NL", "IE", "BR", "US", "IT", "NG", "BG", "MX"),

  # 1998 France (32 teams → 16 to R16)
  "1998" = c("IT", "NO", "BR", "CL", "FR", "PY", "NG", "DK",
             "DE", "MX", "NL", "YU", "RO", "HR", "AR", "EN"),

  # 2002 Korea/Japan (32 teams → 16 to R16)
  "2002" = c("DE", "PY", "EN", "DK", "ES", "IE", "SN", "SE",
             "BR", "BE", "TR", "JP", "MX", "US", "KR", "IT"),

  # 2006 Germany (32 teams → 16 to R16)
  "2006" = c("DE", "SE", "AR", "MX", "IT", "AU", "CH", "UA",
             "EN", "EC", "PT", "NL", "BR", "GH", "ES", "FR"),

  # 2010 South Africa (32 teams → 16 to R16)
  "2010" = c("UY", "KR", "US", "GH", "DE", "EN", "AR", "MX",
             "NL", "SK", "BR", "CL", "PY", "JP", "ES", "PT"),

  # 2014 Brazil (32 teams → 16 to R16)
  "2014" = c("BR", "CL", "CO", "UY", "FR", "NG", "DE", "DZ",
             "NL", "MX", "CR", "GR", "AR", "CH", "BE", "US"),

  # 2018 Russia (32 teams → 16 to R16)
  "2018" = c("FR", "AR", "UY", "PT", "ES", "RU", "HR", "DK",
             "BR", "MX", "BE", "JP", "SE", "CH", "CO", "EN"),

  # 2022 Qatar (32 teams → 16 to R16)
  "2022" = c("NL", "US", "AR", "AU", "JP", "HR", "BR", "KR",
             "EN", "SN", "FR", "PL", "MA", "ES", "PT", "CH")
)

# Convert to long dataframe
advanced_df <- bind_rows(
  lapply(names(advanced_teams), function(yr) {
    tibble(wc_year = as.integer(yr), elo_code = advanced_teams[[yr]],
           advanced = 1L)
  })
)

# ==========================================================
# 5) Combine: ELO + advancement status → underperformance
# ==========================================================
result <- elo_all %>%
  left_join(advanced_df, by = c("wc_year", "elo_code")) %>%
  mutate(
    advanced         = coalesce(advanced, 0L),
    group_stage_exit = as.integer(advanced == 0L),
    top10_elo        = as.integer(elo_rank <= 10),
    underperformed   = as.integer(top10_elo == 1L & group_stage_exit == 1L),
    # Unique team code for joining with FIFA (UK nations share iso3=GBR)
    team_code = case_when(
      elo_code == "EN" ~ "ENG",
      elo_code == "SQ" ~ "SCO",
      elo_code == "EI" ~ "NIR",
      TRUE             ~ iso3
    )
  )

# ==========================================================
# 6) FIFA World Rankings (1994–2022) — comparison with ELO
# ==========================================================

# Pre-tournament FIFA ranking dateIds (via inside.fifa.com API)
# Sequential IDs (id1–id152) for 1992–2007, epoch-based (days since 1985-01-01) after
fifa_wc_dateids <- c(
  "1994" = "id10",     # 1994-05-17
  "1998" = "id50",     # 1998-05-20
  "2002" = "id97",     # 2002-05-15
  "2006" = "id145",    # 2006-05-17
  "2010" = "id9276",   # 2010-05-26
  "2014" = "id10747",  # 2014-06-05
  "2018" = "id12210",  # 2018-06-07
  "2022" = "id13792"   # 2022-10-06
)

# FIFA country code → ISO3 mapping (only non-trivial ones; most are identical)
fifa_to_iso3 <- c(
  "GER" = "DEU",  "ENG" = "GBR",  "NED" = "NLD",  "SUI" = "CHE",
  "KSA" = "SAU",  "URU" = "URY",  "CHI" = "CHL",  "PAR" = "PRY",
  "BUL" = "BGR",  "CRC" = "CRI",  "RSA" = "ZAF",  "TRI" = "TTO",
  "TCH" = "CZE",  "YUG" = "SRB",  "DEN" = "DNK",  "POR" = "PRT",
  "SCO" = "GBR",  "NIR" = "GBR",  "WAL" = "GBR",  "ALG" = "DZA",
  "HON" = "HND",  "HAI" = "HTI",  "CRO" = "HRV",  "ANG" = "AGO",
  "GRE" = "GRC",  "TOG" = "TGO",  "UAE" = "ARE",  "SCG" = "SRB",
  "BIH" = "BIH",  "SLV" = "SLV",  "CGO" = "COG",  "COD" = "COD",
  "ZAI" = "COD"
)

# Scrape FIFA rankings for a given dateId
scrape_fifa_ranking <- function(date_id, wc_year, pause = 0.3) {
  url <- paste0("https://inside.fifa.com/api/ranking-overview?locale=en&dateId=", date_id)
  cat("  Fetching FIFA ranking for WC", wc_year, "(", date_id, ")...\n")

  raw <- tryCatch(
    readLines(url, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      warning("  Failed to fetch FIFA ranking: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(raw) || length(raw) == 0) return(NULL)

  json <- tryCatch(fromJSON(paste(raw, collapse = ""), flatten = TRUE),
                    error = function(e) { warning("  JSON parse error"); return(NULL) })
  if (is.null(json) || length(json$rankings) == 0) return(NULL)

  df <- tibble(
    wc_year          = as.integer(wc_year),
    fifa_global_rank = json$rankings$rankingItem.rank,
    fifa_code        = json$rankings$rankingItem.countryCode,
    fifa_name        = json$rankings$rankingItem.name,
    fifa_points      = json$rankings$rankingItem.totalPoints
  )

  Sys.sleep(pause)
  return(df)
}

cat("\n=== Downloading pre-tournament FIFA rankings (1994–2022) ===\n")
fifa_all <- bind_rows(
  lapply(names(fifa_wc_dateids), function(yr) {
    scrape_fifa_ranking(fifa_wc_dateids[[yr]], yr)
  })
)

cat("FIFA data:", nrow(fifa_all), "team-WC rows across",
    n_distinct(fifa_all$wc_year), "tournaments\n\n")

# Map FIFA codes to ISO3, and create a unique team_code for joining
# (UK nations ENG/SCO/NIR/WAL share iso3=GBR but are distinct teams)
fifa_all <- fifa_all %>%
  mutate(
    iso3 = ifelse(fifa_code %in% names(fifa_to_iso3),
                  fifa_to_iso3[fifa_code],
                  fifa_code),  # most FIFA codes == ISO3
    team_code = ifelse(fifa_code %in% c("ENG", "SCO", "NIR", "WAL"),
                       fifa_code, iso3)
  )

# Filter FIFA rankings to only WC participants for each year,
# then rank among WC participants (apples-to-apples with ELO rank)
wc_participants <- result %>% distinct(wc_year, team_code)

fifa_wc <- fifa_all %>%
  semi_join(wc_participants, by = c("wc_year", "team_code")) %>%
  group_by(wc_year) %>%
  arrange(fifa_global_rank) %>%
  mutate(fifa_wc_rank = row_number()) %>%     # rank among WC participants
  ungroup() %>%
  select(wc_year, team_code, fifa_global_rank, fifa_wc_rank, fifa_points)

# Join FIFA rankings into the result (using team_code to avoid GBR duplicates)
result <- result %>%
  left_join(fifa_wc, by = c("wc_year", "team_code"))

# Compute FIFA-based underperformance (top-10 FIFA WC rank + group stage exit)
result <- result %>%
  mutate(
    top10_fifa        = as.integer(!is.na(fifa_wc_rank) & fifa_wc_rank <= 10),
    underperf_fifa    = as.integer(top10_fifa == 1L & group_stage_exit == 1L),
    # Rank difference: positive = FIFA ranked them higher (lower number = better)
    rank_diff         = ifelse(!is.na(fifa_wc_rank), elo_rank - fifa_wc_rank, NA_integer_),
    # Does the underperformance label differ?
    label_differs     = ifelse(!is.na(fifa_wc_rank) & top10_elo == 1,
                               as.integer(underperformed != underperf_fifa), NA_integer_)
  )

# Print FIFA vs ELO comparison summary
cat("=== FIFA vs ELO Comparison (1994–2022) ===\n")
comparison <- result %>%
  filter(!is.na(fifa_wc_rank), top10_elo == 1 | top10_fifa == 1) %>%
  select(wc_year, team_name, elo_rank, fifa_wc_rank, rank_diff,
         group_stage_exit, underperformed, underperf_fifa, label_differs) %>%
  arrange(wc_year, elo_rank)

# Summary stats
label_diffs <- comparison %>% filter(!is.na(label_differs), label_differs == 1)
cat("Teams in top-10 by ELO and/or FIFA (1994–2022):", nrow(comparison), "\n")
cat("Cases where underperformance label differs:", nrow(label_diffs), "\n\n")

if (nrow(label_diffs) > 0) {
  cat("=== Label Differences (ELO says underperformed but FIFA doesn't, or vice versa) ===\n")
  print(label_diffs, n = Inf)
  cat("\n")
}

# Rank correlation per WC year
cat("=== Rank Correlation (ELO vs FIFA, among WC participants) ===\n")
result %>%
  filter(!is.na(fifa_wc_rank)) %>%
  group_by(wc_year) %>%
  summarise(
    n_teams         = n(),
    spearman_corr   = cor(elo_rank, fifa_wc_rank, method = "spearman"),
    mean_abs_diff   = mean(abs(rank_diff)),
    max_abs_diff    = max(abs(rank_diff)),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ==========================================================
# 7) Summary: top-10 ELO teams only
# ==========================================================
top10 <- result %>%
  filter(top10_elo == 1) %>%
  select(wc_year, elo_rank, elo_code, team_name, iso3,
         elo_rating, group_stage_exit, underperformed,
         fifa_global_rank, fifa_wc_rank, fifa_points,
         top10_fifa, underperf_fifa, rank_diff, label_differs) %>%
  arrange(wc_year, elo_rank)

cat("=== Summary ===\n")
cat("Total WC team-year observations:", nrow(result), "\n")
cat("Top-10 ELO observations:", nrow(top10), "\n")
cat("Underperformances (top-10 ELO group-stage exits):", sum(top10$underperformed), "\n\n")

# Show all underperformances
cat("=== Underperformances ===\n")
underperformances <- top10 %>% filter(underperformed == 1)
print(underperformances, n = Inf)

# ==========================================================
# 8) Save outputs
# ==========================================================

# Full dataset (all participants with ELO)
write_csv(result, "Data/world_cup/wc_elo_all_participants.csv")

# Top-10 ELO dataset (main analysis file)
write_csv(top10, "Data/world_cup/wc_underperformance_elo.csv")

cat("\n=== Output files ===\n")
cat("  Data/world_cup/wc_elo_all_participants.csv  (all WC participants + ELO)\n")
cat("  Data/world_cup/wc_underperformance_elo.csv  (top-10 ELO + underperformance flag)\n")
cat("\nDONE.\n")

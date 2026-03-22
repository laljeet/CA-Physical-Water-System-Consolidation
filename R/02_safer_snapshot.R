# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kern
# Created: 2025-08-08
#
# Purpose:
#   Build county-level SAFER snapshots, status breakdowns, 
#   and top failing systems for all counties in the registry.
#   Outputs both per-county CSVs and combined summary CSVs.
#
# Inputs:
#   - data_in/SAFER_RA.csv                        (raw SAFER dataset)
#   - combined/tables/county_registry.csv         (list of counties)
#
# Outputs (per county in counties/<slug>/tables/):
#   - county_snapshot.csv
#   - status_breakdown.csv
#   - top_failing_by_population.csv
#
# Outputs (combined in combined/tables/):
#   - safer_county_snapshot_all.csv
#   - safer_status_breakdown_all.csv
#
# Dependencies:
#   - Requires R/00_paths.R to define p_data_in, p_tables, to_slug(), county_tab()
#   - Packages: readr, dplyr, tidyr, stringr, fs, purrr
#
# Notes:
#   - Status levels are fixed for consistency across counties.
#   - Population and connection counts ignore NA values in sums.
#   - Script will create per-county directories if missing.
# --------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)
library(purrr)
library(tibble)

source("R/00_paths.R")  # path helpers, to_slug(), county_tab()

# ---- Inputs ----
safer_csv <- path(p_data_in, "SAFER_RA.csv")
registry_path <- path(p_tables, "county_registry.csv")

if (!file_exists(safer_csv)) stop("Missing data_in/SAFER_RA.csv")
if (!file_exists(registry_path)) stop("Missing combined/tables/county_registry.csv")

registry <- read_csv(registry_path, show_col_types = FALSE)

# ---- Read SAFER with explicit types ----
safer <- read_csv(
  safer_csv,
  col_types = cols(
    COUNTY = col_character(),
    WATER_SYSTEM_NUMBER = col_character(),
    SYSTEM_NAME = col_character(),
    FINAL_SAFER_STATUS = col_character(),
    POPULATION = col_double(),
    SERVICE_CONNECTIONS = col_double(),
    OWNER_TYPE = col_character(),
    FEDERAL_CLASSIFICATION_TYPE = col_character(),
    FUNDING_RECEIVED_SINCE_2017 = col_double(),
    TECHNICAL_ASSISTANCE_FUNDING_SINCE_2017 = col_double(),
    LATITUDE_MEASURE = col_double(),
    LONGITUDE_MEASURE = col_double(),
    CALENVIRO_SCREEN_SCORE = col_double(),
    MHI = col_double(),
    SERVICE_AREA_ECONOMIC_STATUS = col_character(),
    # Let readr guess all other columns
    .default = col_guess()
  )
)

# Note: Parsing warnings for MHI and CALENVIRO_SCREEN_SCORE are expected
# due to missing data for some systems. These are correctly converted to NA.


# ---- Normalize ----
unique(safer$FINAL_SAFER_STATUS)

status_levels <- c("Failing", "At-Risk", "Potentially At-Risk", "Not Assessed", "Not At-Risk")

safer <- safer %>%
  mutate(
    COUNTY = toupper(str_trim(COUNTY)),
    FINAL_SAFER_STATUS = na_if(str_trim(FINAL_SAFER_STATUS), ""),
    FINAL_SAFER_STATUS = factor(FINAL_SAFER_STATUS, levels = status_levels, ordered = TRUE)
  )

safer_base <- safer %>%
  transmute(
    WATER_SYSTEM_NUMBER = str_trim(as.character(WATER_SYSTEM_NUMBER)),
    SYSTEM_NAME,
    COUNTY,
    LATITUDE_MEASURE,
    LONGITUDE_MEASURE,
    FINAL_SAFER_STATUS,
    SERVICE_CONNECTIONS,
    POPULATION,
    OWNER_TYPE,
    FEDERAL_CLASSIFICATION_TYPE,
    CALENVIRO_SCREEN_SCORE,
    MHI,
    SERVICE_AREA_ECONOMIC_STATUS
  )

readr::write_csv(safer_base, fs::path(p_tables, "safer_system_base_clean.csv"))

# ---- Data Quality Checks ----

# Check 1: Verify all status values match expected levels
stopifnot(setequal(unique(as.character(safer$FINAL_SAFER_STATUS)), status_levels))

# Check 2: Identify counties in registry but missing from SAFER data
county_check <- registry$county_name |>
  purrr::map_chr(\(cn) {
    n <- sum(safer$COUNTY == toupper(cn))
    if (n == 0) cn else NA_character_
  }) |>
  na.omit()

if (length(county_check) > 0) {
  message("INFO: The following counties have no water systems in SAFER data:\n  ",
          paste(county_check, collapse = ", "))
}

#Check 3: Identify counties in SAFER data but not in registry (possible typos)
counties_in_data <- unique(safer$COUNTY)
counties_in_registry <- toupper(registry$county_name)
unexpected_counties <- setdiff(counties_in_data, counties_in_registry)

if (length(unexpected_counties) > 0) {
  warning("ALERT: Unexpected county names found in SAFER data (possible typos):\n  ",
          paste(unexpected_counties, collapse = ", "))
}

# ---- Builders ----
get_county_df <- function(safer, county_name) {
  safer %>% filter(COUNTY == toupper(county_name))
}

build_snapshot <- function(df, county_name) {
  tibble::tibble(
    county = county_name,
    num_systems = nrow(df),
    pop_served = sum(df$POPULATION, na.rm = TRUE),
    failing_systems = sum(df$FINAL_SAFER_STATUS == "Failing", na.rm = TRUE),
    at_risk_systems = sum(df$FINAL_SAFER_STATUS == "At-Risk", na.rm = TRUE),
    potentially_at_risk_systems = sum(df$FINAL_SAFER_STATUS == "Potentially At-Risk", na.rm = TRUE),
    not_assessed_systems = sum(df$FINAL_SAFER_STATUS == "Not Assessed", na.rm = TRUE),
    not_at_risk_systems = sum(df$FINAL_SAFER_STATUS == "Not At-Risk", na.rm = TRUE)
  )
}

build_status_breakdown <- function(df, county_name) {
  df %>%
    mutate(status = as.character(FINAL_SAFER_STATUS)) %>%
    count(status, name = "num_systems") %>%
    complete(status = status_levels, fill = list(num_systems = 0)) %>%
    left_join(
      df %>%
        group_by(status = as.character(FINAL_SAFER_STATUS)) %>%
        summarise(population = sum(POPULATION, na.rm = TRUE), .groups = "drop"),
      by = "status"
    ) %>%
    mutate(population = dplyr::coalesce(population, 0)) %>%
    arrange(factor(status, levels = status_levels)) %>%
    mutate(county = county_name, .before = 1)
}

build_top_failing <- function(df, county_name, n = 10) {
  df %>%
    filter(FINAL_SAFER_STATUS == "Failing") %>%
    transmute(
      county = county_name,
      water_system_number = WATER_SYSTEM_NUMBER,
      system_name = SYSTEM_NAME,
      population = POPULATION,
      service_connections = SERVICE_CONNECTIONS,
      owner_type = OWNER_TYPE,
      classification = FEDERAL_CLASSIFICATION_TYPE
    ) %>%
    arrange(desc(population)) %>%
    slice_head(n = n)
}


# ---- Writers ----
write_snapshot <- function(snap, slug) {
  dir_create(county_tab(slug))
  write_csv(snap, path(county_tab(slug), "county_snapshot.csv"))
}

write_status_breakdown <- function(sta, slug) {
  dir_create(county_tab(slug))
  write_csv(sta, path(county_tab(slug), "status_breakdown.csv"))
}

write_top_failing <- function(topf, slug) {
  dir_create(county_tab(slug))
  write_csv(topf, path(county_tab(slug), "top_failing_by_population.csv"))
}

# ---- Orchestrator for one county ----
process_county <- function(county_name) {
  slug <- to_slug(county_name)
  df <- get_county_df(safer, county_name)
  if (nrow(df) == 0) return(NULL)
  
  snap <- build_snapshot(df, county_name)
  sta  <- build_status_breakdown(df, county_name)
  topf <- build_top_failing(df, county_name)
  
  write_snapshot(snap, slug)
  write_status_breakdown(sta, slug)
  write_top_failing(topf, slug)
  
  list(snapshot = snap, status = sta)
}

# ---- Run for all counties in the registry ----
results <- map(registry$county_name, process_county)

# ---- Combined outputs ----
all_snap   <- bind_rows(map(results, "snapshot"))
all_status <- bind_rows(map(results, "status"))

write_csv(all_snap,   path(p_tables, "safer_county_snapshot_all.csv"))
write_csv(all_status, path(p_tables, "safer_status_breakdown_all.csv"))





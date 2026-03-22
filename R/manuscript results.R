library(readr)
library(dplyr)
library(sf)
source("R/00_paths.R")

counties <- c("Kern", "Kings", "Tulare", "Fresno")

all_costs <- purrr::map_dfr(counties, function(cn) {
  slug <- to_slug(cn)
  f <- path(county_tab(slug), "consolidation_costs_all_with_viability.csv")
  read_csv(f, show_col_types = FALSE) %>%
    mutate(county = cn)
})

# 1. Pairs by county and type
all_costs %>% count(county, costing_case)

# 2. Viability
all_costs %>% count(county, is_fundable)

# 3. Cost summary
all_costs %>%
  group_by(county, costing_case) %>%
  summarise(
    n = n(),
    median_cost = median(total_capital_cost, na.rm = TRUE),
    median_cpc = median(cost_per_connection, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Total capital need (fundable only)
all_costs %>%
  filter(is_fundable) %>%
  group_by(county) %>%
  summarise(
    n_fundable = n(),
    total_capital = sum(total_capital_cost, na.rm = TRUE)
  )

all_costs %>% filter(!is_fundable) %>%
  select(county, joining_name, receiving_name, effective_connections, 
         total_capital_cost, cost_per_connection, costing_case)

# Classification counts per county
purrr::map_dfr(c("Kern", "Kings", "Tulare", "Fresno"), function(cn) {
  slug <- to_slug(cn)
  sys <- st_read(path(county_dir(slug), "water_systems.gpkg"), layer = "systems", quiet = TRUE)
  tibble(
    county = cn,
    total_systems = nrow(sys),
    n_joining = sum(sys$FINAL_SAFER_STATUS == "Failing" & sys$SERVICE_CONNECTIONS <= 1000 |
                      sys$FINAL_SAFER_STATUS == "At-Risk" & sys$SERVICE_CONNECTIONS <= 500, na.rm = TRUE),
    n_receiving = sum(
      (sys$FINAL_SAFER_STATUS == "Failing" & sys$SERVICE_CONNECTIONS > 1000) |
        (sys$FINAL_SAFER_STATUS == "At-Risk" & sys$SERVICE_CONNECTIONS > 500) |
        (sys$FINAL_SAFER_STATUS %in% c("Potentially At-Risk", "Not At-Risk", "Not Assessed") & sys$SERVICE_CONNECTIONS > 500) |
        (is.na(sys$FINAL_SAFER_STATUS) & sys$SERVICE_CONNECTIONS > 500), na.rm = TRUE)
  )
})

mandatory_systems <- c("CA1500518", "CA1502465", "CA1502659", "CA1500579", 
                       "CA1503684", "CA1500494")  # approximate - check your data

all_costs %>%
  filter(county == "Kern") %>%
  filter(grepl("ATHAL|FULLER|EAST WILSON|OASIS|SAN JOAQUIN ESTATES|WILSON ROAD|WINI|DEL ORO|VICTORY|OLD RIVER|SOUTH KERN", 
               joining_name, ignore.case = TRUE)) %>%
  select(joining_name, receiving_name, total_capital_cost, cost_per_connection, is_fundable)

# Tulare
all_costs %>%
  filter(county == "Tulare") %>%
  filter(grepl("CUTLER|OROSI|EAST OROSI|PORTERVILLE|AKIN|CENTRAL MUTUAL|MONSON|SEVILLE|SULTANA|YETTEM|TEVISTON|SIERRA VISTA|ALLENSWORTH", 
               joining_name, ignore.case = TRUE)) %>%
  select(joining_name, receiving_name, total_capital_cost, cost_per_connection, is_fundable)

# Fresno
all_costs %>%
  filter(county == "Fresno") %>%
  filter(grepl("LAS DELTAS", joining_name, ignore.case = TRUE)) %>%
  select(joining_name, receiving_name, total_capital_cost, cost_per_connection, is_fundable)

results <- list()
for (cn in c("Kern", "Kings", "Tulare", "Fresno")) {
  slug <- to_slug(cn)
  joining <- read_csv(path(county_tab(slug), "joining_systems.csv"), show_col_types = FALSE)
  pairs <- read_csv(path(county_tab(slug), "consolidation_pairs_final_2024_method.csv"), show_col_types = FALSE)
  
  matched_ids <- unique(pairs$joining_pwsid)
  unmatched <- joining %>% filter(!SABL_PWSID %in% matched_ids)
  
  message(cn, ": ", nrow(joining), " joining, ", 
          nrow(unmatched), " unmatched, ",
          sum(unmatched$FINAL_SAFER_STATUS == "Failing", na.rm = TRUE), " failing, ",
          sum(unmatched$FINAL_SAFER_STATUS == "At-Risk", na.rm = TRUE), " at-risk")
}

bind_rows(results)

all_costs %>%
  group_by(county, costing_case) %>%
  summarise(
    n = n(),
    median_cost = median(total_capital_cost, na.rm = TRUE),
    median_cpc = median(cost_per_connection, na.rm = TRUE),
    .groups = "drop"
  )

all_costs %>%
  group_by(costing_case) %>%
  summarise(
    n = n(),
    median_cost = median(total_capital_cost, na.rm = TRUE),
    median_cpc = median(cost_per_connection, na.rm = TRUE)
  )

min(all_costs$total_capital_cost)
max(all_costs$total_capital_cost)


source("R/00_paths.R")
library(readr)
library(dplyr)

safer <- read_csv(path(p_tables, "safer_system_base_clean.csv"), show_col_types = FALSE)

# Statewide counts
safer %>% count(FINAL_SAFER_STATUS)

# Four county counts
safer %>%
  filter(COUNTY %in% c("KERN", "KINGS", "TULARE", "FRESNO")) %>%
  group_by(COUNTY, FINAL_SAFER_STATUS) %>%
  summarise(
    n_systems = n(),
    total_pop = sum(POPULATION, na.rm = TRUE),
    .groups = "drop"
  )

# Four county totals
safer %>%
  filter(COUNTY %in% c("KERN", "KINGS", "TULARE", "FRESNO")) %>%
  filter(FINAL_SAFER_STATUS %in% c("Failing", "At-Risk")) %>%
  summarise(
    n_failing = sum(FINAL_SAFER_STATUS == "Failing"),
    n_at_risk = sum(FINAL_SAFER_STATUS == "At-Risk"),
    total_pop = sum(POPULATION, na.rm = TRUE)
  )

safer %>%
  filter(COUNTY %in% c("KERN", "KINGS", "TULARE", "FRESNO")) %>%
  count(COUNTY)

# Total SAFER systems
nrow(safer)

# Total SABL polygons (from your statewide gpkg)
library(sf)
sabl <- st_read(path(p_data_in, "water_systems_ca.gpkg"), layer = "systems", quiet = TRUE)
nrow(sabl)

for (cn in c("Kern", "Kings", "Tulare", "Fresno")) {
  slug <- to_slug(cn)
  roads <- readRDS(path(p_data_in, "osm_roads", paste0(slug, "_roads_osm.rds")))
  message(cn, ": ", nrow(roads$osm_lines), " road segments")
}

# How many systems got estimated boundaries across the four counties
library(sf)
source("R/00_paths.R")

for (cn in c("Kern", "Kings", "Tulare", "Fresno")) {
  slug <- to_slug(cn)
  sys <- st_read(path(county_dir(slug), "water_systems.gpkg"), layer = "systems", quiet = TRUE)
  n_est <- sum(sys$safer_data_available == "yes_estimated_boundary", na.rm = TRUE)
  message(cn, ": ", n_est, " estimated boundaries")
}

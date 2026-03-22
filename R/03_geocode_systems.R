# R/03_load_system_boundaries.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kern
# Created: 2025-02-05
#
# Purpose:
#   Load California water system boundary shapefile, join with SAFER data,
#   create estimated boundaries for systems missing geometry, and produce
#   complete spatial datasets for proximity analysis.
#
# Inputs:
#   - data_in/California_Drinking_Water_System_Area_Boundaries/ (shapefile)
#   - combined/tables/safer_system_base_clean.csv
#   - combined/tables/county_registry.csv
#
# Outputs:
#   - data_in/water_systems_ca.gpkg (complete statewide dataset - 5,050 systems)
#   - counties/<slug>/water_systems.gpkg (per-county spatial datasets)
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: sf, dplyr, readr, fs, purrr
#
# Notes:
#   - All outputs use EPSG:3310 (CA Albers)
#   - Systems missing boundaries get 0.5-mile buffer around coordinates
#   - 21 systems have multiple service area polygons (duplicates retained)
#   - safer_data_available flags: "yes", "no", "yes_estimated_boundary"
# --------------------------------------------------------------
# --------------------------------------------------------------

source("R/00_paths.R")
library(sf)
library(dplyr)
library(readr)
library(fs)
library(purrr)

# ---- File Paths ----
boundaries_shp <- path(p_data_in, "California_Drinking_Water_System_Area_Boundaries", 
                       "California_Drinking_Water_System_Area_Boundaries.shp")
safer_clean <- path(p_tables, "safer_system_base_clean.csv")
registry_path <- path(p_tables, "county_registry.csv")


# ---- Load Data ----

boundaries <- st_read(boundaries_shp, quiet = TRUE)

safer <- read_csv(safer_clean, show_col_types = FALSE)

registry <- read_csv(registry_path, show_col_types = FALSE)

names(boundaries)
names(safer)
names(registry)

# Check a few examples from each
head(boundaries$SABL_PWSID, 10)
head(safer$WATER_SYSTEM_NUMBER, 10)

# Join keeping all boundaries, add suffix to distinguish sources
systems_spatial <- boundaries %>%
  left_join(
    safer,
    by = c("SABL_PWSID" = "WATER_SYSTEM_NUMBER"),
    suffix = c(".boundary", ".safer")
  ) %>%
  # Add the flag
  mutate(safer_data_available = if_else(is.na(SYSTEM_NAME), "no", "yes"))

# Check the result
names(systems_spatial)


systems_clean <- systems_spatial %>%
  select(
    # Core identifiers
    SABL_PWSID,
    SYSTEM_NAME,
    
    # Geographic
    COUNTY.boundary,
    COUNTY.safer,
    LATITUDE_MEASURE,
    LONGITUDE_MEASURE,
    
    # Population (keep both)
    POPULATION.boundary,
    POPULATION.safer,
    SERVICE_CONNECTIONS,
    
    # System characteristics
    OWNER_TYPE.boundary,
    OWNER_TYPE.safer,
    FEDERAL_CLASSIFICATION_TYPE,
    FEDERAL_CL,  # from boundary
    STATE_CLAS,
    REGULATING,
    
    # SAFER risk data
    FINAL_SAFER_STATUS,
    CALENVIRO_SCREEN_SCORE,
    MHI,
    SERVICE_AREA_ECONOMIC_STATUS,
    
    # Additional fields you want to keep
    ADDRESS_CI,
    ADDRESS_ST,
    ADDRESS_ZI,
    ACTIVITY_S,
    Shape__Are,
    Shape__Len,
    
    # Flag
    safer_data_available,
    
    # Geometry
    geometry
  )

# Check result
names(systems_clean)
nrow(systems_clean)


# Check current CRS
st_crs(systems_clean)$epsg

# Transform both to CA Albers (EPSG:3310)
systems_spatial <- st_transform(systems_spatial, 3310)
systems_clean <- st_transform(systems_clean, 3310)

# Verify
st_crs(systems_clean)$epsg
st_crs(systems_spatial)$epsg

safer_missing_boundary <- safer %>%
  anti_join(
    systems_clean %>% st_drop_geometry() %>% select(SABL_PWSID),
    by = c("WATER_SYSTEM_NUMBER" = "SABL_PWSID")
  ) %>%
  select(WATER_SYSTEM_NUMBER, SYSTEM_NAME, COUNTY, FINAL_SAFER_STATUS, 
         POPULATION, SERVICE_CONNECTIONS, LATITUDE_MEASURE, LONGITUDE_MEASURE)

# How many are missing?
nrow(safer_missing_boundary)

missing_geoms <- safer_missing_boundary %>%
  st_as_sf(coords = c("LONGITUDE_MEASURE", "LATITUDE_MEASURE"), 
           crs = 4326) %>%
  st_transform(3310) %>%
  st_buffer(dist = 0.5 * 5280 * 0.3048) %>%
  select(WATER_SYSTEM_NUMBER, geometry) 

# Create the 59 missing systems in the same structure as systems_clean
missing_systems <- safer_missing_boundary %>%
  st_as_sf(coords = c("LONGITUDE_MEASURE", "LATITUDE_MEASURE"), 
           crs = 4326) %>%
  st_transform(3310) %>%
  st_buffer(dist = 0.5 * 5280 * 0.3048) %>%
  # Add back the original coordinates from safer_missing_boundary
  bind_cols(
    safer_missing_boundary %>% 
      select(orig_lat = LATITUDE_MEASURE, orig_lon = LONGITUDE_MEASURE)
  ) %>%
  transmute(
    # Core identifiers
    SABL_PWSID = WATER_SYSTEM_NUMBER,
    SYSTEM_NAME = SYSTEM_NAME,
    
    # Geographic - only have SAFER data
    COUNTY.boundary = COUNTY,
    COUNTY.safer = COUNTY,
    LATITUDE_MEASURE = orig_lat,   # Keep the original coordinates
    LONGITUDE_MEASURE = orig_lon,
    
    # Population - only have SAFER data
    POPULATION.boundary = POPULATION,
    POPULATION.safer = POPULATION,
    SERVICE_CONNECTIONS = SERVICE_CONNECTIONS,
    
    # System characteristics - only have SAFER data
    OWNER_TYPE.boundary = NA_character_,
    OWNER_TYPE.safer = NA_character_,
    FEDERAL_CLASSIFICATION_TYPE = NA_character_,
    FEDERAL_CL = NA_character_,
    STATE_CLAS = NA_character_,
    REGULATING = NA_character_,
    
    # SAFER risk data
    FINAL_SAFER_STATUS = FINAL_SAFER_STATUS,
    CALENVIRO_SCREEN_SCORE = NA_real_,
    MHI = NA_real_,
    SERVICE_AREA_ECONOMIC_STATUS = NA_character_,
    
    # Additional fields
    ADDRESS_CI = NA_character_,
    ADDRESS_ST = NA_character_,
    ADDRESS_ZI = NA_character_,
    ACTIVITY_S = NA_character_,
    Shape__Are = as.numeric(st_area(geometry)),
    Shape__Len = NA_real_,
    
    # Flag
    safer_data_available = "yes_estimated_boundary"
  )

# Now just bind rows
systems_complete <- bind_rows(systems_clean, missing_systems)

# Check
nrow(systems_complete)  # Should be 5050 (4991 + 59)

check_overlap <- safer_missing_boundary %>%
  filter(WATER_SYSTEM_NUMBER %in% systems_clean$SABL_PWSID)

nrow(check_overlap)

# Check for duplicate SABL_PWSID in systems_complete
systems_complete %>%
  st_drop_geometry() %>%
  count(SABL_PWSID) %>%
  filter(n > 1)

### We didnt add any new duplciates. These already exist in the system. 

# ---- Save Complete Datasets ----

# 1. Save the MOST complete version (systems_complete with all 5050)
st_write(systems_complete, 
         path(p_data_in, "water_systems_ca.gpkg"),  # This is your PRIMARY file
         layer = "systems", 
         delete_dsn = TRUE, 
         quiet = TRUE)

# 2. Save the full unfiltered spatial join as backup
st_write(systems_spatial, 
         path(p_data_in, "water_systems_merged_raw.gpkg"), 
         layer = "systems", 
         delete_dsn = TRUE, 
         quiet = TRUE)

# Verify
message("Saved complete dataset: ", nrow(systems_complete), " water systems")
message("  - Systems with SAFER data (boundaries): ", 
        sum(systems_complete$safer_data_available == "yes"))
message("  - Systems with SAFER data (estimated): ", 
        sum(systems_complete$safer_data_available == "yes_estimated_boundary"))
message("  - Systems without SAFER data: ", 
        sum(systems_complete$safer_data_available == "no"))



systems_complete <- systems_complete %>%
  group_by(SABL_PWSID) %>%
  slice_max(order_by = Shape__Are, n = 1, with_ties = FALSE) %>%
  ungroup()

message("After deduplication: ", nrow(systems_complete), " systems")


# ---- Save Per-County Files ----
write_county_systems <- function(county_name) {
  slug <- to_slug(county_name)
  county_sys_dir <- county_dir(slug)
  dir_create(county_sys_dir)
  
  # Use systems_complete (not systems_clean)
  county_systems <- systems_complete %>%
    filter(toupper(COUNTY.boundary) == toupper(county_name))
  
  if (nrow(county_systems) == 0) {
    message("  ", county_name, ": No systems")
    return(NULL)
  }
  
  # Save
  output_path <- path(county_sys_dir, "water_systems.gpkg")
  st_write(county_systems, output_path, layer = "systems", delete_dsn = TRUE, quiet = TRUE)
  
  message("  ", county_name, ": ", nrow(county_systems), " systems")
  invisible(NULL)
}

# Process all counties
message("\nCreating per-county files...")
walk(registry$county_name, write_county_systems)

# R/01_get_counties.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kern
# Created: 2025-08-08
#
# Purpose:
#   Download California counties, save a GeoPackage locally,
#   and build a county registry with name, FIPS, and slug.
#
# Inputs:
#   - none, data pulled via tigris
#
# Outputs:
#   - data_in/counties_ca.gpkg
#   - combined/tables/county_registry.csv
#   - combined/tables/county_registry_excel.csv
#
# Dependencies:
#   - Requires R/00_paths.R for p_data_in and p_tables
#   - Packages: sf, tigris, dplyr, readr, fs, stringr
#
# Notes:
#   - FIPS stored as text padded to 5
#   - Excel safe CSV wraps FIPS like ="06029"
# --------------------------------------------------------------
source("R/00_paths.R")

library(sf)
library(tigris)
library(dplyr)
library(readr)
library(fs)

options(tigris_use_cache = TRUE)

fs::dir_create(p_data_in)
fs::dir_create(p_tables)


ca_counties <- tigris::counties(state = "CA", cb = TRUE, year = 2023, class = "sf") |>
  select(NAME, GEOID) |>
  st_transform(3310)


gpkg_final <- fs::path(p_data_in, "counties_ca.gpkg")
tmp <- tempfile(fileext = ".gpkg")
st_write(ca_counties, dsn = tmp, layer = "ca_counties", quiet = TRUE)
file_move(tmp, gpkg_final)


county_registry <- ca_counties |>
  st_drop_geometry() |>
  transmute(
    county_name = NAME,
    county_fips = stringr::str_pad(as.character(GEOID), 5, pad = "0"),
    county_slug = to_slug(NAME)
  ) |>
  arrange(county_slug)

readr::write_csv(county_registry, fs::path(p_tables, "county_registry.csv"))

registry_excel <- county_registry |>
  mutate(county_fips = paste0('="', county_fips, '"'))
readr::write_csv(registry_excel, fs::path(p_tables, "county_registry_excel.csv"))

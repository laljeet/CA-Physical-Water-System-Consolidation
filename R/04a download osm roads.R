# R/04a_download_osm_roads.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kern
# Created: 2025
#
# Purpose:
#   Download and cache OSM road network data for all California
#   counties. Run ONCE before 04b_statewide_proximity.R.
#   Each county's road data is saved as an .rds file in data_in/osm_roads/.
#
# Inputs:
#   - data_in/counties_ca.gpkg (county boundaries from Script 01)
#   - combined/tables/county_registry.csv
#
# Outputs:
#   - data_in/osm_roads/<slug>_roads_osm.rds (per county)
#   - data_in/osm_roads/_download_log.csv (tracks success/failure)
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: sf, osmdata, dplyr, readr, fs
#
# Notes:
#   - Downloads can take several hours for all 58 counties.
#   - Safe to re-run: skips counties that already have cached files.
#   - If Overpass API throttles you, wait and re-run.
#   - Large counties (San Bernardino, Kern, etc.) may take 5-10 min each.
# --------------------------------------------------------------

source("R/00_paths.R")
library(sf)
library(osmdata)
library(dplyr)
library(readr)
library(fs)

# ---- Setup ----

osm_cache_dir <- path(p_data_in, "osm_roads")
dir_create(osm_cache_dir)

registry <- read_csv(path(p_tables, "county_registry.csv"), show_col_types = FALSE)

ca_counties <- st_read(path(p_data_in, "counties_ca.gpkg"), layer = "ca_counties", quiet = TRUE)

# ---- Download Function ----

download_county_roads <- function(county_name, slug, ca_counties, osm_cache_dir,
                                  force = FALSE) {
  
  cache_file <- path(osm_cache_dir, paste0(slug, "_roads_osm.rds"))
  
  # Skip if already cached (unless force = TRUE)
  if (file_exists(cache_file) && !force) {
    message("  [SKIP] ", county_name, " — already cached")
    return(list(county = county_name, slug = slug, status = "skipped",
                n_roads = NA_integer_, time_sec = 0, error = NA_character_))
  }
  
  t0 <- Sys.time()
  
  tryCatch({
    # Get county boundary and transform to WGS84 for OSM query
    county_boundary <- ca_counties %>%
      filter(NAME == county_name) %>%
      st_transform(4326)
    
    if (nrow(county_boundary) == 0) {
      message("  [WARN] ", county_name, " — not found in counties_ca.gpkg")
      return(list(county = county_name, slug = slug, status = "not_found",
                  n_roads = NA_integer_, time_sec = 0, error = "County not in gpkg"))
    }
    
    # Get bounding box
    bb <- st_bbox(county_boundary)
    
    message("  [DOWN] ", county_name, " — downloading from Overpass API...")
    
    # Download OSM highway data
    roads_osm <- opq(bbox = bb) %>%
      add_osm_feature(key = "highway") %>%
      osmdata_sf()
    
    # Check we got data
    n_roads <- nrow(roads_osm$osm_lines)
    if (is.null(n_roads) || n_roads == 0) {
      message("  [WARN] ", county_name, " — no road data returned")
      return(list(county = county_name, slug = slug, status = "no_data",
                  n_roads = 0L, time_sec = as.numeric(Sys.time() - t0, units = "secs"),
                  error = "No OSM lines returned"))
    }
    
    # Save
    saveRDS(roads_osm, file = cache_file)
    
    elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    message("  [DONE] ", county_name, " — ", formatC(n_roads, big.mark = ","),
            " road segments (", elapsed, "s)")
    
    return(list(county = county_name, slug = slug, status = "success",
                n_roads = n_roads, time_sec = elapsed, error = NA_character_))
    
  }, error = function(e) {
    elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    message("  [FAIL] ", county_name, " — ", conditionMessage(e))
    return(list(county = county_name, slug = slug, status = "error",
                n_roads = NA_integer_, time_sec = elapsed,
                error = conditionMessage(e)))
  })
}

# ---- Run All Counties ----

message("\n", paste(rep("=", 60), collapse = ""))
message("OSM ROAD DATA DOWNLOAD — ", nrow(registry), " counties")
message(paste(rep("=", 60), collapse = ""))
message("Cache directory: ", osm_cache_dir)
message("Start time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(paste(rep("=", 60), collapse = ""), "\n")

log_entries <- vector("list", nrow(registry))

for (i in seq_len(nrow(registry))) {
  county_name <- registry$county_name[i]
  slug <- registry$county_slug[i]
  
  message(sprintf("[%02d/%02d] %s", i, nrow(registry), county_name))
  
  log_entries[[i]] <- download_county_roads(
    county_name = county_name,
    slug = slug,
    ca_counties = ca_counties,
    osm_cache_dir = osm_cache_dir,
    force = FALSE
  )
  
  # Be polite to the Overpass API — pause between downloads
  if (log_entries[[i]]$status == "success") {
    Sys.sleep(5)
  }
}

# ---- Save Download Log ----

download_log <- bind_rows(log_entries)
log_file <- path(osm_cache_dir, "_download_log.csv")
write_csv(download_log, log_file)

message("\n", paste(rep("=", 60), collapse = ""))
message("DOWNLOAD COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("  Success:  ", sum(download_log$status == "success"))
message("  Skipped:  ", sum(download_log$status == "skipped"))
message("  Failed:   ", sum(download_log$status == "error"))
message("  No data:  ", sum(download_log$status == "no_data"))
message("Log saved:  ", log_file)
message("End time:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# ---- Show failures if any ----
failures <- download_log %>% filter(status == "error")
if (nrow(failures) > 0) {
  message("\nFailed counties (re-run script to retry):")
  for (j in seq_len(nrow(failures))) {
    message("  - ", failures$county[j], ": ", failures$error[j])
  }
}

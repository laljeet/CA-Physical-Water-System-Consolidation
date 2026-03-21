# run_all.R
# --------------------------------------------------------------
# Master script to reproduce all results in:
#   "Physical Water System Consolidation is a Necessary but Insufficient
#    Strategy for Achieving Safe Drinking Water Access in the San Joaquin Valley"
#
# Usage:
#   Open SAFER.Rproj in RStudio, then:
#   source("run_all.R")
#
# Prerequisites:
#   1. Place SAFER_RA.csv in data_in/
#   2. Place SABL shapefile in data_in/California_Drinking_Water_System_Area_Boundaries/
#   3. Run 04a_download_osm_roads.R separately first (requires internet, takes ~1 hour)
#
# Notes:
#   - Total runtime: approximately 2-3 hours depending on hardware
#   - 16+ GB RAM recommended for large county road networks
#   - Scripts 04 and 05 loop through four study counties
# --------------------------------------------------------------

message("\n", paste(rep("=", 70), collapse = ""))
message("REPRODUCING ALL RESULTS")
message("Start time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(paste(rep("=", 70), collapse = ""), "\n")

# ---- Step 1: County registry and boundaries ----
message("\n>>> Step 1: County registry")
source("R/01_get_counties.R")

# ---- Step 2: SAFER snapshots ----
message("\n>>> Step 2: SAFER snapshots")
source("R/02_safer_snapshot.R")

# ---- Step 3: Water system spatial dataset ----
message("\n>>> Step 3: Water system boundaries")
source("R/03_geocode_systems.R")

# ---- Step 4 & 5: Proximity analysis and cost estimation ----
# Run for each study county
study_counties <- c("Kern", "Kings", "Tulare", "Fresno")

for (cn in study_counties) {
  message("\n", paste(rep("=", 50), collapse = ""))
  message(">>> Processing: ", toupper(cn))
  message(paste(rep("=", 50), collapse = ""))
  
  # Set county parameters
  county_name <- cn
  slug <- to_slug(county_name)
  
  # Proximity analysis
  message("\n>>> Step 4: Proximity analysis - ", cn)
  source("R/04_proximity_analysis.R", local = TRUE)
  
  # Cost analysis
  message("\n>>> Step 5: Cost analysis - ", cn)
  source("R/05_cost_analysis.R", local = TRUE)
}

# ---- Step 6: Publication figures ----
message("\n>>> Step 7: Publication figures")
source("R/07_publication_figures.R")

# ---- Done ----
message("\n", paste(rep("=", 70), collapse = ""))
message("ALL RESULTS REPRODUCED")
message("End time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(paste(rep("=", 70), collapse = ""))

# R/04_kings_proximity_analysis.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kings
# Created: 2025-09-06
#
# Purpose:
#   Identify potential physical consolidation opportunities in Kings County
#   by analyzing proximity between failing/at-risk systems and potential
#   receiving systems following State Water Board methodology (2024).
#
# Inputs:
#   - counties/kings/water_systems.gpkg
#
# Outputs:
#   - counties/kings/tables/consolidation_pairs_intersect.csv
#   - counties/kings/tables/consolidation_pairs_route.csv
#   - counties/kings/tables/consolidation_pairs_all.csv
#   - counties/kings/tables/joining_systems.csv
#   - counties/kings/tables/receiving_systems.csv
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: sf, dplyr, readr, fs, tibble
#
# Notes:
#   - Step 1: Classify Receiving vs Joining systems (Table 1, page 11)
#   - Step 2a: Intersect analysis (boundaries overlap)
#   - Step 2b: Route analysis (≤3 miles, using 1.3x straight-line multiplier)
#   - All spatial data uses EPSG:3310 (CA Albers)
# --------------------------------------------------------------

source("R/00_paths.R")
library(sf)
library(dplyr)
library(readr)
library(fs)
library(tibble)
library(purrr)
county_name <- "Kings"
slug <- to_slug(county_name)

# ---- Load Kings County Systems ----

kings_systems <- st_read(
  path(county_dir(slug), "water_systems.gpkg"),
  layer = "systems",
  quiet = TRUE
)

message("Loaded ", nrow(kings_systems), " water systems in Kings County")

# ---- Step 1: Classify Receiving vs Joining Systems ----
# Based on State Water Board Table 1 (Page 11):
#
# JOINING SYSTEMS (smaller systems needing help):
# ┌─────────────────────┬──────────────────────────┐
# │ SAFER Status        │ Criteria                 │
# ├─────────────────────┼──────────────────────────┤
# │ Failing PWS         │ ≤ 1,000 connections      │
# │ At-Risk PWS         │ ≤ 500 connections        │
# │ All others          │ Excluded                 │
# └─────────────────────┴──────────────────────────┘
#
# RECEIVING SYSTEMS (larger systems that can absorb joining systems):
# ┌─────────────────────┬──────────────────────────┐
# │ SAFER Status        │ Criteria                 │
# ├─────────────────────┼──────────────────────────┤
# │ Failing PWS         │ > 1,000 connections      │
# │ At-Risk PWS         │ > 500 connections        │
# │ Potentially At-Risk │ > 500 connections        │
# │ Not At-Risk         │ > 500 connections        │
# │ Not Assessed        │ > 500 connections        │
# │ No SAFER data       │ > 500 connections        │
# └─────────────────────┴──────────────────────────┘
#
# Note: This creates a pool of eligible receivers. The actual matching
# (selecting the largest receiver for each joining system) happens in Step 2.

kings_classified <- kings_systems %>%
  mutate(
    # Identify potential JOINING systems
    is_joining = case_when(
      FINAL_SAFER_STATUS == "Failing" & SERVICE_CONNECTIONS <= 1000 ~ TRUE,
      FINAL_SAFER_STATUS == "At-Risk" & SERVICE_CONNECTIONS <= 500 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Identify potential RECEIVING systems (pool of eligible candidates)
    is_receiving = case_when(
      FINAL_SAFER_STATUS == "Failing" & SERVICE_CONNECTIONS > 1000 ~ TRUE,
      FINAL_SAFER_STATUS == "At-Risk" & SERVICE_CONNECTIONS > 500 ~ TRUE,
      FINAL_SAFER_STATUS %in% c("Potentially At-Risk", "Not At-Risk", "Not Assessed") & 
        SERVICE_CONNECTIONS > 500 ~ TRUE,
      is.na(FINAL_SAFER_STATUS) & SERVICE_CONNECTIONS > 500 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Classification summary
message("\n=== Classification Summary ===")
message("Joining systems: ", sum(kings_classified$is_joining))
message("Receiving systems: ", sum(kings_classified$is_receiving))
message("Systems that are neither: ", sum(!kings_classified$is_joining & !kings_classified$is_receiving))

# Separate datasets
joining_systems <- kings_classified %>%
  filter(is_joining == TRUE)

receiving_systems <- kings_classified %>%
  filter(is_receiving == TRUE)

# ---- Step 2a: Intersect Analysis ----

message("\n=== Step 2a: Intersect Analysis ===")
message("Analyzing ", nrow(joining_systems), " joining systems against ", 
        nrow(receiving_systems), " receiving systems...")

# Find intersections (where boundaries overlap)
intersects <- st_intersects(joining_systems, receiving_systems, sparse = FALSE)

# Build consolidation pairs table
consolidation_pairs_intersect <- NULL

for (i in 1:nrow(joining_systems)) {
  receiving_matches <- which(intersects[i, ])
  
  if (length(receiving_matches) > 0) {
    pairs <- tibble(
      joining_pwsid = joining_systems$SABL_PWSID[i],
      joining_name = joining_systems$SYSTEM_NAME[i],
      joining_status = joining_systems$FINAL_SAFER_STATUS[i],
      joining_population = joining_systems$POPULATION.safer[i],
      joining_connections = joining_systems$SERVICE_CONNECTIONS[i],
      receiving_pwsid = receiving_systems$SABL_PWSID[receiving_matches],
      receiving_name = receiving_systems$SYSTEM_NAME[receiving_matches],
      receiving_status = receiving_systems$FINAL_SAFER_STATUS[receiving_matches],
      receiving_population = receiving_systems$POPULATION.safer[receiving_matches],
      receiving_connections = receiving_systems$SERVICE_CONNECTIONS[receiving_matches],
      consolidation_type = "intersect"
    )
    
    consolidation_pairs_intersect <- bind_rows(consolidation_pairs_intersect, pairs)
  }
}

# SELECT LARGEST RECEIVER FOR EACH JOINER
consolidation_pairs_intersect <- consolidation_pairs_intersect %>%
  group_by(joining_pwsid) %>%
  slice_max(order_by = receiving_connections, n = 1, with_ties = FALSE) %>%
  ungroup()

message("Intersect pairs found: ", nrow(consolidation_pairs_intersect))
message("Unique joining systems: ", n_distinct(consolidation_pairs_intersect$joining_pwsid))
message("Unique receiving systems: ", n_distinct(consolidation_pairs_intersect$receiving_pwsid))

# Save intersect results
dir_create(county_tab(slug))
write_csv(consolidation_pairs_intersect, 
          path(county_tab(slug), "consolidation_pairs_intersect.csv"))

# ---- Step 3: Route Analysis using road network ----

# Identify joining systems that DON'T intersect with any receiver
intersects_full <- st_intersects(joining_systems, receiving_systems, sparse = FALSE)
has_any_intersect <- rowSums(intersects_full) > 0

joining_no_intersect <- joining_systems %>%
  filter(!has_any_intersect)

message("Analyzing ", nrow(joining_no_intersect), " non-intersecting joining systems...")


library(osmdata)
library(dodgr)
library(sf)

# ---------------------------------------------------------
# Step 3: Load locally cached OSM road network (Kings County)
# Decision:
# - Use OSM + dodgr for routing
# - Read from local cache so the script runs offline/reproducibly
# ---------------------------------------------------------

osm_cache_file <- file.path(p_data_in, "osm_roads", paste0(slug, "_roads_osm.rds"))
if (!file.exists(osm_cache_file)) {
  stop(
    "Missing required OSM cache file: ", osm_cache_file, "\n",
    "Create it once in a network-enabled environment, then rerun."
  )
}

kings_roads_osm <- readRDS(osm_cache_file)

names(kings_roads_osm)
names(kings_roads_osm$osm_lines)
table(kings_roads_osm$osm_lines$highway, useNA = "ifany") |> sort(decreasing = TRUE) |> head(20)

# 3. Convert the OSM data into a routable network graph
# This prepares the data for local distance calculations
roads_sf <- kings_roads_osm$osm_lines
roads_sf <- st_transform(kings_roads_osm$osm_lines, 4326)

roads_sf2 <- roads_sf %>%
  dplyr::filter(!highway %in% c(
    "construction", "proposed", "raceway", "escape", "crossing",
    "footway", "path", "cycleway", "bridleway", "steps", "pedestrian",
    "corridor", "elevator", "escalator", "platform" # Adding these to be safe
  )) %>%
  dplyr::mutate(
    oneway = tolower(as.character(oneway)),
    oneway = dplyr::case_when(
      oneway %in% c("yes", "true", "1") ~ "yes",
      oneway %in% c("no", "false", "0", "") ~ "no",
      TRUE ~ NA_character_
    )
  )


road_network <- dodgr::weight_streetnet(roads_sf2, wt_profile = "motorcar")
table(roads_sf2$highway) |> head(20)
# Pre-compute road entry points on receiving system boundaries
receiving_boundaries <- st_cast(receiving_systems, "MULTILINESTRING")
roads_3310 <- st_transform(roads_sf2, 3310)
road_entry_pts <- st_intersection(roads_3310, receiving_boundaries)

road_entry_pts <- road_entry_pts %>%
  filter(st_geometry_type(.) %in% c("POINT", "MULTIPOINT")) %>%
  st_cast("POINT")

message("Road entry points computed: ", nrow(road_entry_pts))


get_road_nearest_entry <- function(j_idx, r_idx, joining_pts, receiving_systems, 
                                   road_entry_pts, road_network) {
  this_receiver_id <- receiving_systems$SABL_PWSID[r_idx]
  entry_pts <- road_entry_pts %>% filter(SABL_PWSID == this_receiver_id)
  
  if (nrow(entry_pts) == 0) return(NULL)
  
  from_coord <- st_transform(joining_pts[j_idx, ], 4326) %>% st_coordinates()
  to_coords <- st_transform(entry_pts, 4326) %>% st_coordinates()
  
  dists <- dodgr::dodgr_distances(
    road_network,
    from = from_coord,
    to = to_coords,
    parallel = FALSE
  )
  
  road_dists <- as.numeric(dists[1, ])
  best <- which.min(road_dists)
  
  if (length(best) == 0 || !is.finite(road_dists[best])) return(NULL)
  
  st_geometry(entry_pts[best, ])
}



# Check for disconnected components
graph <- dodgr::dodgr_components(road_network)
message("Number of disconnected road islands in Kings: ", length(unique(graph$component)))

# ---- Step 4: Routing Analysis (Centroid to Road Entry Point) ----
# This step identifies candidate consolidation pairs between:
#   (1) Joining systems (failing or at-risk systems)
#   (2) Receiving systems (eligible larger systems)
#
# The goal is to generate a candidate list that will later be evaluated
# using road-network routing and cost estimation.
#
# This step does NOT determine eligibility or cost. It only creates
# candidate pairs that are geographically plausible.
#
#
# ORIGIN POINT: TRUE CENTROID
# ---------------------------
# Consistent with the SWRCB Physical Consolidation methodology,
# we use the true mathematical centroid (st_centroid) as the
# routing origin, even if it falls outside an irregular polygon.
# dodgr snaps all routing points to the nearest road node.
#
# We store centroid_inside (TRUE/FALSE) as a QA flag to track
# how many centroids fall outside their service area boundary.
#
#
# DESTINATION POINT: ROAD ENTRY POINT
# ------------------------------------
# Consistent with the SWRCB methodology, we identify destination
# points by intersecting the road network with the receiving
# system's service area boundary. This gives the exact points
# where physical roads enter the receiving system.
#
# For each candidate pair, we select the road entry point on
# the receiving system boundary that is closest to the joining
# system centroid.
#
#
# CANDIDATE SEARCH RADIUS
# -----------------------
# Candidate receivers are identified using a 10-mile straight-line
# (Euclidean) distance screen.
#
# This is NOT the consolidation rule.
#
# The actual eligibility rule (applied later) is:
#   • Intersecting service areas qualify automatically
#   • Otherwise, road-network route distance must be ≤ 3 miles
#
# The 10-mile screen is used only to:
#   • Reduce routing computations
#   • Avoid missing valid candidates due to road geometry
#   • Ensure statewide scalability
#
#
# 1. Start Points: Ensure we start INSIDE the polygon
joining_pts_surface <- st_point_on_surface(joining_no_intersect)

# but we grab Centroid coordinates for the PAPER (to be compliant).
joining_centroids <- st_centroid(joining_no_intersect)

## Compute centroid lon/lat for the Methods section
cent_ll <- st_transform(joining_centroids, 4326) %>% st_coordinates()

# This checks whether centroid i lies inside polygon i
m <- st_intersects(joining_centroids, joining_no_intersect, sparse = FALSE)
centroid_inside <- diag(m)

# 2. Build routing points using decision rule
# Start with centroid geometry
joining_pts <- joining_centroids

# Replace geometry where centroid is outside
# st_geometry(joining_pts)[!centroid_inside] <- st_geometry(joining_pts_surface)[!centroid_inside]

# Attach metadata
joining_pts <- joining_pts %>%
  mutate(
    centroid_lon = cent_ll[, 1],
    centroid_lat = cent_ll[, 2],
    centroid_inside = centroid_inside
  )
# 2. Identify Candidates (10-mile search radius)
search_radius_meters <- 10 * 1609.34
candidate_matches <- st_is_within_distance(joining_pts, receiving_systems, dist = search_radius_meters)

potential_pairs_df <- tibble::tibble(
  joining_idx = rep(seq_along(candidate_matches), lengths(candidate_matches)),
  receiving_idx = unlist(candidate_matches)
) %>%
  mutate(
    joining_pwsid  = joining_pts$SABL_PWSID[joining_idx],
    joining_name   = joining_pts$SYSTEM_NAME[joining_idx],
    joining_status = joining_pts$FINAL_SAFER_STATUS[joining_idx],
    joining_pop    = joining_pts$POPULATION.safer[joining_idx],
    joining_conn   = joining_pts$SERVICE_CONNECTIONS[joining_idx],
    receiving_pwsid = receiving_systems$SABL_PWSID[receiving_idx],
    receiving_name  = receiving_systems$SYSTEM_NAME[receiving_idx],
    centroid_lon    = joining_pts$centroid_lon[joining_idx],
    centroid_lat    = joining_pts$centroid_lat[joining_idx],
    centroid_inside = joining_pts$centroid_inside[joining_idx]
  )

message("Candidate pairs within 10-mile radius: ", nrow(potential_pairs_df))

# 3. Specific Perimeter Points for every candidate pair
# pair_perimeters <- st_nearest_points(
#   st_geometry(joining_pts)[potential_pairs_df$joining_idx],
#   st_geometry(receiving_systems)[potential_pairs_df$receiving_idx],
#   pairwise = TRUE
# ) %>%
#   st_cast("POINT") %>%
#   .[seq(2, length(.), by = 2)] %>% 
#   st_sfc(crs = 3310)


pair_destinations <- purrr::map2(
  potential_pairs_df$joining_idx,
  potential_pairs_df$receiving_idx,
  function(j_idx, r_idx) {
    get_road_nearest_entry(j_idx, r_idx, joining_pts, receiving_systems,
                           road_entry_pts, road_network)
  }
)


# Flag pairs with no road entry point
no_entry <- sapply(pair_destinations, is.null)
message("Pairs with no road entry point: ", sum(no_entry))

# Filter out NULL pairs and update potential_pairs_df
potential_pairs_df <- potential_pairs_df[!no_entry, ]
pair_perimeters <- do.call(c, pair_destinations[!no_entry])

# Cache entry points for reuse in connection_lines and connection_routes
entry_point_cache <- setNames(
  pair_destinations[!no_entry],
  paste0(potential_pairs_df$joining_pwsid, "_", potential_pairs_df$receiving_pwsid)
)

# 4. Matrix Road Distance Calculation
from_coords <- st_transform(joining_pts[potential_pairs_df$joining_idx, ], 4326) %>% st_coordinates()
to_coords   <- st_transform(pair_perimeters, 4326) %>% st_coordinates()

dist_matrix <- dodgr::dodgr_distances(
  road_network,
  from = from_coords,
  to = to_coords,
  parallel = FALSE
)

# 5. Apply the "Finite" Filter and 3-Mile Threshold
road_matches_all <- potential_pairs_df %>%
  mutate(road_miles = diag(dist_matrix) * 0.000621371) %>%
  # Use is.finite() to handle dodgr's Inf returns for unreachable nodes
  filter(is.finite(road_miles) & road_miles <= 3) %>%
  left_join(
    receiving_systems %>% st_drop_geometry() %>% 
      select(receiving_pwsid = SABL_PWSID, receiving_conn = SERVICE_CONNECTIONS, 
             receiving_status = FINAL_SAFER_STATUS, receiving_pop = POPULATION.safer),
    by = "receiving_pwsid"
  ) %>%
  mutate(consolidation_type = "route")

# 6. Refine to Largest System Receiver
consolidation_pairs_road_largest <- road_matches_all %>%
  group_by(joining_pwsid) %>%
  slice_max(order_by = receiving_conn, n = 1, with_ties = FALSE) %>%
  ungroup()


# Combine intersect + road network results
all_pairs_final <- bind_rows(
  consolidation_pairs_intersect %>%
    mutate(joining_economic_status = NA_character_),  # Add this column to match structure
  consolidation_pairs_road_largest %>%
    transmute(
      joining_pwsid, 
      joining_name, 
      joining_status = joining_status,
      joining_economic_status = NA_character_,  # We'll add this properly below
      joining_population = joining_pop, 
      joining_connections = joining_conn,
      receiving_pwsid, 
      receiving_name, 
      receiving_status,
      receiving_population = receiving_pop, 
      receiving_connections = receiving_conn,
      consolidation_type, 
      road_miles
    )
) %>%
  mutate(road_miles = ifelse(consolidation_type == "intersect", NA_real_, road_miles))

# Now add economic status from the original kings_systems data
all_pairs_final <- all_pairs_final %>%
  left_join(
    kings_systems %>% 
      st_drop_geometry() %>% 
      select(SABL_PWSID, SERVICE_AREA_ECONOMIC_STATUS),
    by = c("joining_pwsid" = "SABL_PWSID")
  ) %>%
  mutate(joining_economic_status = SERVICE_AREA_ECONOMIC_STATUS) %>%
  select(-SERVICE_AREA_ECONOMIC_STATUS)  # Remove the joined column


message("Total consolidation pairs: ", nrow(all_pairs_final))
message("  - Intersect: ", sum(all_pairs_final$consolidation_type == "intersect"))
message("  - Road network: ", sum(all_pairs_final$consolidation_type == "route"))

# Compare distances: centroid vs point-on-surface for the 3 problem systems
problem_ids <- joining_no_intersect %>%
  filter(!centroid_inside) %>%
  pull(SABL_PWSID)

# Check if any of the 3 ended up in your final consolidation pairs
all_pairs_final %>%
  filter(joining_pwsid %in% problem_ids)

# Save combined results
write_csv(all_pairs_final, 
          path(county_tab(slug), "consolidation_pairs_final_2024_method.csv"))

# Save joining and receiving system lists

joining_summary <- joining_systems %>%
  st_drop_geometry() %>%
  select(SABL_PWSID, SYSTEM_NAME, FINAL_SAFER_STATUS, SERVICE_AREA_ECONOMIC_STATUS,
         POPULATION.safer, SERVICE_CONNECTIONS, COUNTY.safer)

receiving_summary <- receiving_systems %>%
  st_drop_geometry() %>%
  select(SABL_PWSID, SYSTEM_NAME, FINAL_SAFER_STATUS, SERVICE_AREA_ECONOMIC_STATUS,
         POPULATION.safer, SERVICE_CONNECTIONS, COUNTY.safer)

write_csv(joining_summary, path(county_tab(slug), "joining_systems.csv"))
write_csv(receiving_summary, path(county_tab(slug), "receiving_systems.csv"))

# ---- Step 6: Geospatial Export (GeoPackage) ----
message("Exporting geospatial layers...")

# Layer 1: Joiner polygons with partner data
opportunities_polygons <- kings_systems %>%
  inner_join(all_pairs_final %>% select(-joining_name), by = c("SABL_PWSID" = "joining_pwsid"))

# Layer 2: Connection Lines for the Road Matches
connection_lines <- purrr::map_dfr(1:nrow(consolidation_pairs_road_largest), function(i) {
  row <- consolidation_pairs_road_largest[i, ]
  j_idx <- which(joining_pts$SABL_PWSID == row$joining_pwsid)
  r_idx <- which(receiving_systems$SABL_PWSID == row$receiving_pwsid)
  
  j_pt <- joining_pts %>% filter(SABL_PWSID == row$joining_pwsid)
  cache_key <- paste0(row$joining_pwsid, "_", row$receiving_pwsid)
  entry_geom <- entry_point_cache[[cache_key]]
  if (is.null(entry_geom)) {
    entry_geom <- get_road_nearest_entry(j_idx, r_idx, joining_pts, receiving_systems,
                                         road_entry_pts, road_network)
  }
  line <- st_nearest_points(j_pt, st_sf(geometry = entry_geom, crs = 3310))
  st_sf(joining_pwsid = row$joining_pwsid, receiving_pwsid = row$receiving_pwsid,
        road_miles = row$road_miles, geom = line)
})

connection_routes <- purrr::map_dfr(1:nrow(consolidation_pairs_road_largest), function(i) {
  row <- consolidation_pairs_road_largest[i, ]
  
  j_idx <- which(joining_pts$SABL_PWSID == row$joining_pwsid)
  r_idx <- which(receiving_systems$SABL_PWSID == row$receiving_pwsid)
  
  from_pt <- st_transform(joining_pts[j_idx, ], 4326) %>% st_coordinates()
  
  # Road-nearest entry point
  cache_key <- paste0(row$joining_pwsid, "_", row$receiving_pwsid)
  entry_geom <- entry_point_cache[[cache_key]]
  if (is.null(entry_geom)) {
    entry_geom <- get_road_nearest_entry(j_idx, r_idx, joining_pts, receiving_systems,
                                         road_entry_pts, road_network)
  }
  to_pt <- st_transform(entry_geom, 4326) %>% st_coordinates()
  
  path <- dodgr::dodgr_paths(road_network, from = from_pt, to = to_pt, vertices = FALSE)
  
  if (!is.null(path[[1]][[1]]) && length(path[[1]][[1]]) > 0) {
    path_edges <- road_network[path[[1]][[1]], ]
    edge_lines <- lapply(1:nrow(path_edges), function(e) {
      st_linestring(matrix(c(
        path_edges$from_lon[e], path_edges$from_lat[e],
        path_edges$to_lon[e], path_edges$to_lat[e]
      ), ncol = 2, byrow = TRUE))
    })
    route_geom <- st_sfc(edge_lines, crs = 4326) %>%
      st_union() %>% st_cast("LINESTRING") %>% st_transform(3310)
    st_sf(joining_pwsid = row$joining_pwsid, receiving_pwsid = row$receiving_pwsid,
          road_miles = row$road_miles, geom = route_geom)
  } else {
    j_pt <- joining_pts %>% filter(SABL_PWSID == row$joining_pwsid)
    line <- st_nearest_points(j_pt, st_sf(geometry = entry_geom, crs = 3310))
    st_sf(joining_pwsid = row$joining_pwsid, receiving_pwsid = row$receiving_pwsid,
          road_miles = row$road_miles, geom = line)
  }
})

# ---- Save Results for Script 05 (Cost Analysis) and Script 06 (Visualization) ----

message("\n=== Saving Analysis State ===")

routing_crs <- st_crs(joining_pts)

save(
  # Core datasets
  kings_systems,              # Full original dataset (for lookups)
  joining_systems,           # All joining systems
  receiving_systems,         # All receiving systems
  
  # Consolidation results
  consolidation_pairs_intersect,      # Intersect pairs
  consolidation_pairs_road_largest,   # Route pairs
  all_pairs_final,                    # Combined results
  potential_pairs_df,
  
  # Spatial objects for mapping
  joining_pts,               # Joining system points (with centroid metadata)
  opportunities_polygons,    # Joiner polygons with consolidation data
  connection_routes,         # Actual road route geometries
  connection_lines,
  
  # Road network (for future routing if needed)
  road_network,              # dodgr network object
  
  routing_crs,
  
  file = path(p_data_in, "kings_consolidation_analysis.RData")
)

message("Saved to: ", path(p_data_in, "kings_consolidation_analysis.RData"))
message("Environment saved successfully for downstream analysis")

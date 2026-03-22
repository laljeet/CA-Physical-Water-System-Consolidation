# R/06_kings_visualization_leaflet.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kings
# Created: 2025-02-17
#
# Purpose:
#   Create interactive leaflet map showing ROUTE consolidations only
#   (systems connected via road network, NOT intersecting boundaries)
#
# Inputs:
#   - data_in/kings_consolidation_analysis.RData
#
# Outputs:
#   - Interactive HTML map: counties/kings/figures/route_consolidations_map.html
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: leaflet, sf, dplyr, htmlwidgets
# --------------------------------------------------------------

source("R/00_paths.R")
library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)

county_name <- "Kings"
slug <- to_slug(county_name)

# ---- Load Analysis Results ----

message("\n=== Loading Route Consolidation Data ===")

load(path(p_data_in, "kings_consolidation_analysis.RData"))

message("Route consolidation pairs: ", nrow(consolidation_pairs_road_largest))

# ---- Prepare Data for Mapping ----

# Transform to WGS84 (required for leaflet)
joining_systems_wgs84 <- st_transform(joining_systems, 4326)
receiving_systems_wgs84 <- st_transform(receiving_systems, 4326)
connection_routes_wgs84 <- st_transform(connection_routes, 4326)

# Get only route joiners and their receivers
route_joiners <- joining_systems_wgs84 %>%
  filter(SABL_PWSID %in% consolidation_pairs_road_largest$joining_pwsid)

route_receivers <- receiving_systems_wgs84 %>%
  filter(SABL_PWSID %in% consolidation_pairs_road_largest$receiving_pwsid)

# ---- Create Color Palette ----

color_joiner <- "#fdae61"      # Orange - joining systems
color_receiver <- "#2c7bb6"    # Blue - receiving systems
color_route <- "#d7191c"       # Red - connection routes

# ---- Build Interactive Map ----

message("\n=== Creating Interactive Map ===")

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  # Receiving systems (blue)
  addPolygons(
    data = route_receivers,
    fillColor = color_receiver,
    fillOpacity = 0.3,
    color = color_receiver,
    weight = 2,
    opacity = 0.8,
    popup = ~paste0(
      "<b>RECEIVING SYSTEM</b><br>",
      "<b>", SYSTEM_NAME, "</b><br>",
      "PWSID: ", SABL_PWSID, "<br>",
      "Connections: ", formatC(SERVICE_CONNECTIONS, format = "d", big.mark = ","), "<br>",
      "Population: ", formatC(POPULATION.safer, format = "d", big.mark = ",")
    ),
    label = ~SYSTEM_NAME
  ) %>%
  
  # Joining systems (orange)
  addPolygons(
    data = route_joiners,
    fillColor = color_joiner,
    fillOpacity = 0.6,
    color = "#000000",
    weight = 2,
    opacity = 1,
    popup = ~paste0(
      "<b>JOINING SYSTEM</b><br>",
      "<b>", SYSTEM_NAME, "</b><br>",
      "PWSID: ", SABL_PWSID, "<br>",
      "Status: ", FINAL_SAFER_STATUS, "<br>",
      "Connections: ", formatC(SERVICE_CONNECTIONS, format = "d", big.mark = ","), "<br>",
      "Population: ", formatC(POPULATION.safer, format = "d", big.mark = ",")
    ),
    label = ~SYSTEM_NAME
  ) %>%
  
  # Connection routes (red roads)
  addPolylines(
    data = connection_routes_wgs84,
    color = color_route,
    weight = 3,
    opacity = 0.8,
    popup = ~paste0(
      "<b>Road Network Route</b><br>",
      "From: ", joining_pwsid, "<br>",
      "To: ", receiving_pwsid, "<br>",
      "Distance: ", round(road_miles, 2), " miles"
    ),
    label = ~paste0(round(road_miles, 2), " miles")
  ) %>%
  
  # Map controls
  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addScaleBar(position = "bottomleft") %>%
  
  addLegend(
    position = "topright",
    colors = c(color_receiver, color_joiner, color_route),
    labels = c("Receiving System", "Joining System", "Road Route"),
    title = "Route Consolidations",
    opacity = 0.8
  ) %>%
  
  addControl(
    html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
            <h3 style='margin: 0;'>Kings County - Route Consolidations</h3>
            <p style='margin: 5px 0 0 0; font-size: 14px;'>
            Systems connected via road network (≤3 miles)
            </p></div>",
    position = "topleft"
  )
map
# ---- Save Interactive Map ----

message("\n=== Saving Map ===")

dir_create(path(county_dir(slug), "figures"))

map_file <- path(county_dir(slug), "figures", "route_consolidations_map.html")
saveWidget(map, file = map_file, selfcontained = TRUE)

message("Map saved to: ", map_file)
message("\n=== Complete ===")
message("Route consolidations mapped: ", nrow(consolidation_pairs_road_largest))



# R/07_kings_visualization_static.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kings
# Created: 2025-02-17
#
# Purpose:
#   Create publication-quality static map of route consolidations
#   for papers, reports, and presentations.
#
# Inputs:
#   - data_in/kings_consolidation_analysis.RData
#
# Outputs:
#   - High-resolution PNG: counties/kings/figures/route_consolidations_publication.png
#   - PDF version: counties/kings/figures/route_consolidations_publication.pdf
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: tmap, sf, dplyr
# --------------------------------------------------------------

# R/07_kings_visualization_static.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kings
# Created: 2025-02-17
#
# Purpose:
#   Create publication-quality static map of route consolidations
#   for papers, reports, and presentations.
#
# Inputs:
#   - data_in/kings_consolidation_analysis.RData
#
# Outputs:
#   - High-resolution PNG: counties/kings/figures/route_consolidations_publication.png
#   - PDF version: counties/kings/figures/route_consolidations_publication.pdf
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: tmap, sf, dplyr
# --------------------------------------------------------------

source("R/00_paths.R")
library(tmap)
library(sf)
library(dplyr)

county_name <- "Kings"
slug <- to_slug(county_name)

# ---- Load Analysis Results ----

message("\n=== Loading Route Consolidation Data ===")

load(path(p_data_in, "kings_consolidation_analysis.RData"))

message("Route consolidation pairs: ", nrow(consolidation_pairs_road_largest))

# ---- Prepare Data for Mapping ----

# Get only route joiners and their receivers
route_joiners <- joining_systems %>%
  filter(SABL_PWSID %in% consolidation_pairs_road_largest$joining_pwsid)

route_receivers <- receiving_systems %>%
  filter(SABL_PWSID %in% consolidation_pairs_road_largest$receiving_pwsid)

# Get Kings County boundary for context
kings_boundary <- st_union(kings_systems)

# ---- Create Publication Map ----

message("\n=== Creating Publication Map ===")

# Set tmap to plot mode (static)
tmap_mode("plot")

# Create map
pub_map <- tm_shape(kings_boundary) +
  tm_borders(col = "gray60", lwd = 2) +
  tm_fill(col = "gray95") +
  
  # Receiving systems (blue)
  tm_shape(route_receivers) +
  tm_polygons(
    fill = "#2c7bb6",
    fill_alpha = 0.4,
    col = "#2c7bb6",
    lwd = 1.5
  ) +
  
  # Joining systems (orange)
  tm_shape(route_joiners) +
  tm_polygons(
    fill = "#fdae61",
    fill_alpha = 0.7,
    col = "black",
    lwd = 1.5
  ) +
  
  # Connection routes (red)
  tm_shape(connection_routes) +
  tm_lines(
    col = "#d7191c",
    lwd = 2.5,
    col_alpha = 0.8
  ) +
  
  # Layout
  tm_title(
    text = "Kings County Water System Consolidations",
    size = 1.4,
    fontface = "bold",
    position = c("left", "top")
  ) +
  
  tm_layout(
    frame = TRUE,
    frame.lwd = 2,
    bg.color = "white",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.title.size = 1.1,
    legend.text.size = 0.9,
    legend.bg.color = "white",
    legend.bg.alpha = 0.9,
    legend.frame = TRUE,
    outer.margins = c(0.02, 0.02, 0.02, 0.02)
  ) +
  
  # Add compass
  tm_compass(
    type = "4star",
    position = c("right", "top"),
    size = 2
  ) +
  
  # Add scale bar
  tm_scalebar(
    position = c("left", "bottom"),
    text.size = 0.8,
    breaks = c(0, 10, 20, 30)
  ) +
  
  # Add credits
  tm_credits(
    text = "Route consolidations (≤3 miles via road network)\nState Water Board Physical Consolidation Methodology (2024)\nData: California State Water Resources Control Board",
    position = c("left", "bottom"),
    size = 0.65,
    fontface = "italic"
  ) +
  
  # Add legend manually
  tm_add_legend(
    type = "polygons",
    labels = c("Receiving System", "Joining System", "Road Route"),
    fill = c("#2c7bb6", "#fdae61", "#d7191c"),
    fill_alpha = c(0.4, 0.7, 0.8),
    col = c("#2c7bb6", "black", "#d7191c"),
    lwd = c(1.5, 1.5, 2.5),
    title = "Route Consolidations"
  )

# ---- Save Map ----

message("\n=== Saving Publication Maps ===")

dir_create(path(county_dir(slug), "figures"))

# Save as high-resolution PNG (300 dpi)
png_file <- path(county_dir(slug), "figures", "route_consolidations_publication.png")
tmap_save(
  pub_map,
  filename = png_file,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

message("PNG saved: ", png_file)

# Save as PDF (vector - scales to any size)
pdf_file <- path(county_dir(slug), "figures", "route_consolidations_publication.pdf")
tmap_save(
  pub_map,
  filename = pdf_file,
  width = 10,
  height = 8,
  units = "in"
)

message("PDF saved: ", pdf_file)

# ---- Summary ----

message("\n=== Publication Map Complete ===")
message("Systems mapped:")
message("  - Receiving systems: ", nrow(route_receivers))
message("  - Joining systems: ", nrow(route_joiners))
message("  - Connection routes: ", nrow(connection_routes))
message("\nOutput formats:")
message("  - PNG (300 dpi): For presentations, reports")
message("  - PDF (vector): For journal publications, posters")


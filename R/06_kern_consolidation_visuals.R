source("R/00_paths.R")
library(sf)
library(dplyr)
library(leaflet)

county_name <- "Kings"
slug <- to_slug(county_name)

# 1. Load the Analysis State
load(path(p_data_in, "kings_consolidation_analysis.RData"))#

# 2. Prepare Data for Leaflet (Transform to WGS84)
# Filter for Intersecting Joiners
intersect_joiners <- opportunities_polygons %>%
  filter(consolidation_type == "intersect") %>% #
  st_transform(4326)

# Prepare all potential Receiving Systems
receivers <- receiving_systems %>%
  st_transform(4326) #

# 3. Create the Interactive Map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # LAYER 1: Receiving Systems (The "Partners")
  addPolygons(
    data = receivers,
    fillColor = "#377eb8", # Blue for potential receivers
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.4,
    group = "Receiving Systems",
    popup = ~paste0(
      "<strong>Receiving System:</strong> ", SYSTEM_NAME, "<br>",
      "<strong>PWSID:</strong> ", SABL_PWSID, "<br>",
      "<strong>Connections:</strong> ", SERVICE_CONNECTIONS
    )
  ) %>%
  
  # LAYER 2: Joining Systems (The "Intersect" matches)
  addPolygons(
    data = intersect_joiners,
    fillColor = "#e41a1c", # Red for joining systems
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    group = "Joining Systems (Intersect)",
    popup = ~paste0(
      "<strong>Joining System:</strong> ", SYSTEM_NAME, "<br>",
      "<strong>PWSID:</strong> ", SABL_PWSID, "<br><hr>",
      "<strong>Matched To:</strong> ", receiving_name, "<br>",
      "<strong>Pop:</strong> ", joining_population
    )
  ) %>%
  
  # Layer Controls and Legend
  addLayersControl(
    overlayGroups = c("Receiving Systems", "Joining Systems (Intersect)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#e41a1c", "#377eb8"),
    labels = c("Joining System", "Receiving System"),
    title = "System Role"
  ) %>%
  addControl("<strong>Kings County: Consolidation Map</strong>", position = "topright")

################################################################################
source("R/00_paths.R")
library(sf)
library(dplyr)
library(leaflet)

county_name <- "Kings"
slug <- to_slug(county_name)

# 1. Load the Analysis State (ensuring we have the 'Main' variable names)
load(path(p_data_in, paste0(slug, "_consolidation_analysis.RData")))

# 2. Prepare Data for Leaflet (Transform all to WGS84)
# All Joiners (Intersect + Road)
all_joiners <- opportunities_polygons %>% 
  st_transform(4326)

# All Receivers
receivers <- receiving_systems %>% 
  st_transform(4326)

# Connection Lines for Road Matches
lines_4326 <- connection_lines %>% 
  st_transform(4326)

# 3. Create the Interactive Map
m <-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # LAYER 1: Receiving Systems (Blue)
  addPolygons(
    data = receivers,
    fillColor = "#377eb8", 
    weight = 1, color = "white", fillOpacity = 0.4,
    group = "Receiving Systems",
    popup = ~paste0("<b>Receiver:</b> ", SYSTEM_NAME, "<br><b>PWSID:</b> ", SABL_PWSID)
  ) %>%
  
  # LAYER 2: Joining Systems (Red/Orange)
  addPolygons(
    data = all_joiners,
    fillColor = ~ifelse(consolidation_type == "intersect", "#e41a1c", "#ff7f00"),
    weight = 2, color = "white", fillOpacity = 0.7,
    group = "Joining Systems",
    popup = ~paste0(
      "<b>Joiner:</b> ", SYSTEM_NAME, "<br>",
      "<b>Type:</b> ", consolidation_type, "<br>",
      "<b>Partner:</b> ", receiving_name
    )
  ) %>%
  
  # LAYER 3: Connection Lines (Dashed Black Lines)
  addPolylines(
    data = lines_4326,
    color = "black", weight = 2, dashArray = "5, 10",
    group = "Connection Paths (Road)",
    popup = ~paste0("<b>Distance:</b> ", round(road_miles, 2), " miles")
  ) %>%
  
  # Controls to toggle layers on/off
  addLayersControl(
    overlayGroups = c("Receiving Systems", "Joining Systems", "Connection Paths (Road)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#377eb8", "#e41a1c", "#ff7f00"),
    labels = c("Receiving System", "Joiner (Intersect)", "Joiner (Road Network)"),
    title = "System Classification"
  )
m
library(htmlwidgets)
# Save the map object you created (let's assume you named it 'm')
saveWidget(m, file = "Kings_Consolidation_Map.html", selfcontained = TRUE)


# manuscript_figures.R
# Generates all three publication figures
# Requires: manuscript_results.R to be run first (creates all_costs)

source("R/00_paths.R")
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggspatial)
library(rosm)
library(prettymapr)

# ---- Load all_costs if not already in environment ----
counties <- c("Kern", "Kings", "Tulare", "Fresno")

if (!exists("all_costs")) {
  all_costs <- purrr::map_dfr(counties, function(cn) {
    slug <- to_slug(cn)
    f <- path(county_tab(slug), "consolidation_costs_all_with_viability.csv")
    read_csv(f, show_col_types = FALSE) %>% mutate(county = cn)
  })
}

# ---- Figure 1: Study Area ----
ca <- st_read(path(p_data_in, "counties_ca.gpkg"), layer = "ca_counties", quiet = TRUE)
study_boundary <- ca %>% filter(NAME %in% counties)

ca_4326 <- st_transform(ca, 4326)
study_boundary_4326 <- st_transform(study_boundary, 4326)

fig1 <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 6) +
  geom_sf(data = ca_4326, fill = NA, color = "grey60", linewidth = 0.2) +
  geom_sf(data = study_boundary_4326, aes(fill = NAME), color = "black", 
          linewidth = 0.5, alpha = 0.6) +
  scale_fill_manual(
    values = c("Kern" = "#2c7bb6", "Kings" = "#d7191c", 
               "Tulare" = "#fdae61", "Fresno" = "#1a9641"),
    name = "Study Counties"
  ) +
  annotation_scale(location = "bl", text_cex = 0.7,
                   height = unit(0.15, "cm"), line_width = 0.5,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.10, 0.25),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(path(county_dir("kern"), "figures", "fig1_study_area.png"),
       fig1, width = 7, height = 8, dpi = 300)

# ---- Figure 2: Consolidation Map ----
all_receivers <- list()
all_joiners_intersect <- list()
all_joiners_route <- list()
all_routes <- list()

for (cn in counties) {
  slug <- to_slug(cn)
  env <- new.env()
  load(path(p_data_in, paste0(slug, "_consolidation_analysis.RData")), envir = env)
  
  sys_name <- paste0(slug, "_systems")
  county_sys <- env[[sys_name]]
  receiving_systems <- env[["receiving_systems"]]
  connection_routes <- env[["connection_routes"]]
  
  pairs <- read_csv(path(county_tab(slug), "consolidation_pairs_final_2024_method.csv"),
                    show_col_types = FALSE)
  
  paired_receivers <- receiving_systems %>%
    filter(SABL_PWSID %in% pairs$receiving_pwsid)
  
  intersect_ids <- pairs %>% filter(consolidation_type == "intersect") %>% pull(joining_pwsid)
  route_ids <- pairs %>% filter(consolidation_type == "route") %>% pull(joining_pwsid)
  paired_joiners <- county_sys %>% filter(SABL_PWSID %in% pairs$joining_pwsid)
  
  all_receivers[[cn]] <- paired_receivers
  all_joiners_intersect[[cn]] <- paired_joiners %>% filter(SABL_PWSID %in% intersect_ids)
  all_joiners_route[[cn]] <- paired_joiners %>% filter(SABL_PWSID %in% route_ids)
  
  if (!is.null(connection_routes) && nrow(connection_routes) > 0) {
    all_routes[[cn]] <- connection_routes
  }
}

receivers_sf <- bind_rows(all_receivers)
joiners_int_sf <- bind_rows(all_joiners_intersect)
joiners_rte_sf <- bind_rows(all_joiners_route)
routes_sf <- bind_rows(all_routes)

# Transform AFTER building the combined datasets
receivers_4326 <- st_transform(receivers_sf, 4326)
joiners_int_4326 <- st_transform(joiners_int_sf, 4326)
joiners_rte_4326 <- st_transform(joiners_rte_sf, 4326)
routes_4326 <- st_transform(routes_sf, 4326)

fig2 <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 9) +
  geom_sf(data = study_boundary_4326, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = receivers_4326, aes(fill = "Receiving System"),
          color = "#2c7bb6", alpha = 0.3, linewidth = 0.2) +
  geom_sf(data = joiners_int_4326, aes(fill = "Joining System (Intersect)"),
          color = "#d7191c", alpha = 0.8, linewidth = 0.5) +
  geom_sf(data = joiners_rte_4326, aes(fill = "Joining System (Route)"),
          color = "#d7191c", alpha = 0.8, linewidth = 0.5) +
  geom_sf(data = routes_4326, aes(color = "Road Route"), linewidth = 0.8) +
  geom_sf_text(data = study_boundary_4326, aes(label = NAME), size = 3.5, 
               fontface = "bold", color = "grey30") +
  scale_fill_manual(name = "",
                    values = c("Receiving System" = "#2c7bb6",
                               "Joining System (Intersect)" = "#d7191c",
                               "Joining System (Route)" = "#e8856c")) +
  scale_color_manual(name = "", values = c("Road Route" = "#fdae61")) +
  annotation_scale(location = "bl", text_cex = 0.7,
                   height = unit(0.15, "cm"), line_width = 0.5,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold"),
    legend.key.size = unit(0.6, "cm"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(path(county_dir("kern"), "figures", "fig2_consolidation_map.png"),
       fig2, width = 9, height = 10, dpi = 300)

# ---- Figure 3: Cost per Connection ----
fig3 <- ggplot(all_costs, aes(x = effective_connections, y = cost_per_connection)) +
  geom_point(aes(color = county, shape = costing_case), size = 2.5, alpha = 0.7) +
  geom_hline(yintercept = 96000, linetype = "dashed", color = "red", linewidth = 0.5) +
  annotate("text", x = max(all_costs$effective_connections, na.rm = TRUE) * 0.7, 
           y = 96000, label = "$96,000 viability threshold (>= 75 connections)", 
           vjust = -0.8, size = 3, color = "red") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = dollar, trans = "log10") +
  scale_color_manual(values = c("Kern" = "#2c7bb6", "Kings" = "#d7191c", 
                                "Tulare" = "#fdae61", "Fresno" = "#1a9641")) +
  scale_shape_manual(values = c("intersect" = 16, "route" = 17),
                     labels = c("Intersect", "Route")) +
  labs(x = "Effective Service Connections (joining system)",
       y = "Cost per Connection ($, log scale)",
       color = "County", shape = "Consolidation Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  )

ggsave(path(county_dir("kern"), "figures", "fig3_cost_per_connection.png"),
       fig3, width = 8, height = 5.5, dpi = 300)

message("All three figures saved.")
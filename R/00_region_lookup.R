# R/00_region_lookup.R
library(dplyr)
library(tibble)

# From Table 7: California Counties Categorized by Generalized Location.
ca_county_region <- tribble(
  ~county, ~region_class, ~regional_multiplier,
  # Rural (0%)
  "Alpine","Rural",0, "Amador","Rural",0, "Butte","Rural",0, "Calaveras","Rural",0,
  "Colusa","Rural",0, "Del Norte","Rural",0, "Fresno","Rural",0, "Glenn","Rural",0,
  "Humboldt","Rural",0, "Imperial","Rural",0, "Inyo","Rural",0, "Kern","Rural",0,
  "Kings","Rural",0, "Lake","Rural",0, "Lassen","Rural",0, "Madera","Rural",0,
  "Mariposa","Rural",0, "Mendocino","Rural",0, "Merced","Rural",0, "Modoc","Rural",0,
  "Mono","Rural",0, "Nevada","Rural",0, "Placer","Rural",0, "Plumas","Rural",0,
  "San Joaquin","Rural",0, "Shasta","Rural",0, "Sierra","Rural",0, "Siskiyou","Rural",0,
  "Stanislaus","Rural",0, "Sutter","Rural",0, "Tehama","Rural",0, "Trinity","Rural",0,
  "Tulare","Rural",0, "Tuolumne","Rural",0, "Yolo","Rural",0, "Yuba","Rural",0,
  
  # Suburban (+30%)
  "Alameda","Suburban",0.30, "Contra Costa","Suburban",0.30, "El Dorado","Suburban",0.30,
  "Marin","Suburban",0.30, "Monterey","Suburban",0.30, "Napa","Suburban",0.30,
  "Orange","Suburban",0.30, "San Benito","Suburban",0.30, "San Bernardino","Suburban",0.30,
  "San Luis Obispo","Suburban",0.30, "Santa Barbara","Suburban",0.30, "Santa Cruz","Suburban",0.30,
  "Solano","Suburban",0.30, "Sonoma","Suburban",0.30,
  
  # Urban (+32%)
  "Los Angeles","Urban",0.32, "Riverside","Urban",0.32, "Sacramento","Urban",0.32,
  "San Diego","Urban",0.32, "San Francisco","Urban",0.32, "San Mateo","Urban",0.32,
  "Santa Clara","Urban",0.32, "Ventura","Urban",0.32
)

get_regional_multiplier <- function(county_name) {
  out <- ca_county_region %>%
    filter(county == county_name) %>%
    summarise(mult = dplyr::first(regional_multiplier)) %>%
    pull(mult)
  
  if (length(out) == 0 || is.na(out)) {
    stop("County not found in ca_county_region: ", county_name)
  }
  out
}

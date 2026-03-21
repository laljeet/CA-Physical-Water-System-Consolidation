# Physical Water System Consolidation: Feasibility and Cost Analysis for California's San Joaquin Valley

## Overview

This repository contains the complete analysis pipeline for assessing physical consolidation feasibility and estimated capital costs for failing and at-risk public water systems in four San Joaquin Valley counties: Kern, Kings, Tulare, and Fresno. The analysis follows the State Water Resources Control Board's 2024 Physical Consolidation Cost Estimate Methodology, implemented entirely in R using publicly available data and open-source tools.

**Associated Publication:**  
Sangha, L. (2026). "Physical Water System Consolidation is a Necessary but Insufficient Strategy for Achieving Safe Drinking Water Access in the San Joaquin Valley." *J. Water Resour. Plann. Manage.* [DOI pending]

## Key Findings

- 114 consolidation candidates identified across four counties
- 113 of 114 (99%) meet state funding viability thresholds
- Estimated total capital need: $413.5 million
- 97 joining systems (46%) lack a physically feasible consolidation partner
- Model results align with all 11 mandatory consolidation projects currently in progress in Kern County

## Repository Structure

```
├── R/                          # Analysis scripts (run in order)
│   ├── 00_paths.R              # Project paths and helper functions
│   ├── 00_region_lookup.R      # Regional cost multiplier lookup
│   ├── 01_get_counties.R       # Download CA county boundaries
│   ├── 02_safer_snapshot.R     # Process SAFER Risk Assessment data
│   ├── 03_geocode_systems.R    # Build water system spatial dataset
│   ├── 03_county_locator_map.R # Generate county locator maps
│   ├── 04_proximity_analysis.R # Consolidation proximity analysis
│   ├── 04a_download_osm_roads.R# Download OSM road network data
│   ├── 05_cost_analysis.R      # Capital cost estimation
│   ├── 06_consolidation_visuals.R # Interactive leaflet maps
│   └── 07_publication_figures.R# Static figures for manuscript
├── data_in/                    # Input data (see Data Sources below)
│   ├── osm_roads/              # Cached OSM road networks per county
│   └── README.md               # Data download instructions
├── counties/                   # Per-county outputs (generated)
│   └── <county_slug>/
│       ├── water_systems.gpkg  # County water systems spatial data
│       └── tables/             # CSV outputs per county
├── combined/
│   └── tables/                 # Statewide summary tables
├── manuscript/
│   ├── figures/                # Publication figures
│   └── tables/                 # Publication tables
├── run_all.R                   # Master script to reproduce all results
├── SAFER.Rproj                 # RStudio project file
└── README.md                   # This file
```

## Data Sources

The analysis requires the following publicly available datasets. Due to file size, input data are not included in this repository and must be downloaded separately.

| Dataset | Source | URL |
|---------|--------|-----|
| SAFER Risk Assessment | SWRCB | https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/needs.html |
| Service Area Boundaries (SABL) | SWRCB | https://gis.data.ca.gov/datasets/CDPH::california-drinking-water-system-area-boundaries |
| County Boundaries (TIGER/Line) | U.S. Census Bureau | Downloaded automatically via `tigris` package |
| OpenStreetMap Road Network | OSM Contributors | Downloaded via script `04a_download_osm_roads.R` |

### Data Setup Instructions

1. Download the SAFER Risk Assessment CSV and place it at `data_in/SAFER_RA.csv`
2. Download the California Drinking Water System Area Boundaries shapefile and extract to `data_in/California_Drinking_Water_System_Area_Boundaries/`
3. Run scripts 01-03 to generate derived datasets
4. Run script 04a to download and cache OSM road data for study counties

## Reproducing Results

### Requirements

- R >= 4.3.0
- RStudio (recommended)
- Required packages are listed in each script header. Key packages include:
  - `sf`, `dplyr`, `readr`, `fs`, `purrr`, `tibble` (core)
  - `dodgr`, `osmdata` (routing)
  - `ggplot2`, `leaflet`, `tmap`, `ggspatial`, `patchwork` (visualization)

### Quick Start

```r
# Open SAFER.Rproj in RStudio, then:
source("run_all.R")
```

### Step-by-Step Execution

Scripts are designed to run sequentially. Each script sources `R/00_paths.R` for project paths.

| Script | Purpose | Approximate Runtime |
|--------|---------|-------------------|
| 01_get_counties.R | Download county boundaries and build registry | < 1 min |
| 02_safer_snapshot.R | Process SAFER data for all counties | < 1 min |
| 03_geocode_systems.R | Build spatial water system dataset | 2-3 min |
| 04a_download_osm_roads.R | Download OSM road data (one-time) | 30-60 min |
| 04_proximity_analysis.R | Consolidation proximity analysis (per county) | 10-30 min per county |
| 05_cost_analysis.R | Cost estimation (per county) | < 1 min per county |
| 06_consolidation_visuals.R | Interactive maps | 2-5 min per county |
| 07_publication_figures.R | Static publication figures | < 1 min |

**Note:** Script 04 (proximity analysis) is the most computationally intensive step due to road-network routing. For counties with large road networks (e.g., Fresno, Tulare), 16+ GB RAM is recommended. OSM road data downloads in script 04a require internet access and may take several hours for all counties.

### Running for a Specific County

Scripts 04 and 05 are parameterized by county. To run for a specific county, set the county name at the top of each script:

```r
county_name <- "Tulare"  # Change to desired county
slug <- to_slug(county_name)
```

## Methodology

The analysis follows the SWRCB 2024 Physical Consolidation Cost Estimate Methodology with the following implementation:

- **System classification:** Joining and receiving systems identified by SAFER status and service connection thresholds
- **Intersect analysis:** Boundary overlap between joining and receiving systems
- **Route analysis:** Road-network routing using OpenStreetMap data and the `dodgr` package
- **Origin points:** True mathematical centroids (consistent with SWRCB method)
- **Destination points:** Road-boundary intersection points selected by shortest road distance
- **Cost estimation:** Standardized unit costs for pipeline, service lines, connection fees, CEQA, administration, and adder multipliers
- **Viability screening:** SWRCB Table 4 thresholds ($7.2M total for <75 connections; $96K/connection for >=75)

## Citation

If you use this code or data in your research, please cite:

```
Sangha, L. (2026). "Physical Water System Consolidation is a Necessary but 
Insufficient Strategy for Achieving Safe Drinking Water Access in the 
San Joaquin Valley." J. Water Resour. Plann. Manage. [DOI pending]
```

## License

This project is licensed under the MIT License. See LICENSE for details.

## Contact

Laljeet Sangha  
University of California Cooperative Extension, Kern County  
Email: [your email]

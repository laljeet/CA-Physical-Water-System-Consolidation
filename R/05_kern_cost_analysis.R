# R/05_kings_cost_analysis.R
# --------------------------------------------------------------
# Author: Laljeet Sangha, UCCE Kings
# Created: 2025-09-13
#
# Purpose:
#   Calculate estimated physical consolidation project costs following
#   State Water Board 2024 Physical Consolidation Cost Estimate Methodology.
#
#   Phase 1: Intersect consolidations only
#
# Inputs:
#   - counties/kings/tables/consolidation_pairs_final_2024_method.csv
#
# Outputs:
#   - counties/kings/tables/consolidation_costs_intersect.csv
#
# Dependencies:
#   - Requires R/00_paths.R
#   - Packages: dplyr, readr, fs, janitor
#
# Notes:
#   - All costs in 2024 dollars
#   - Kings County treated as Rural (0% regional multiplier)
# --------------------------------------------------------------

source("R/00_paths.R")
source("R/00_region_lookup.R")
library(dplyr)
library(readr)
library(fs)
library(janitor)

county_name <- "Kings"
slug <- to_slug(county_name)

# ============================================================================
# COST ASSUMPTIONS (SWRCB 2024 Methodology)
# ============================================================================
PIPELINE_COST_PER_LF <- 220        # $/linear foot
SERVICE_LINE_COST    <- 6200       # $/connection (service line per connection)
CONNECTION_FEE_PWS   <- 5437       # $/connection (methodology unit assumption)

ADMIN_PCT            <- 0.15       # 15% of (pipeline + service line construction only)
CEQA_INTERSECT       <- 25000      # $/project
CEQA_ROUTE           <- 100000.    # $/project
CONTINGENCY_PCT           <- 0.20  # 20%
PLANNING_CONSTRUCTION_PCT <- 0.10  # 10%
ENGINEERING_PCT           <- 0.15  # 15%
INFLATION_PCT             <- 0.031 # 3.1%

REGIONAL_MULTIPLIER <- get_regional_multiplier("Kings")       # e.g. "Kings"
PIPELINE_DIST_INTERSECT_FT <- 1000  # fixed assumption for intersect PWS

SURROGATE_PPL_PER_CONN <- 3.3       # surrogate: connections = ceiling(pop / 3.3)

adder_multiplier <- 1 +
  CONTINGENCY_PCT +
  PLANNING_CONSTRUCTION_PCT +
  ENGINEERING_PCT +
  INFLATION_PCT

stopifnot(abs(adder_multiplier - 1.481) < 1e-9)

# ============================================================================
# LOAD DATA
# ============================================================================
message("\n=== Loading Consolidation Data ===")

all_pairs <- read_csv(
  path(county_tab(slug), "consolidation_pairs_final_2024_method.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

req_cols <- c(
  "joining_pwsid",
  "receiving_pwsid",
  "consolidation_type",
  "joining_connections",
  "joining_population"
)

missing_cols <- setdiff(req_cols, names(all_pairs))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

message("Total consolidation pairs loaded: ", nrow(all_pairs))
message("  - Intersect: ", sum(all_pairs$consolidation_type == "intersect", na.rm = TRUE))
message("  - Road network: ", sum(all_pairs$consolidation_type == "route", na.rm = TRUE))

# ============================================================================
# FILTER TO INTERSECT ONLY
# ============================================================================
intersect_pairs <- all_pairs %>%
  filter(consolidation_type == "intersect")

message("\n=== Cost Analysis: Intersect Consolidations ===")
message("Analyzing ", nrow(intersect_pairs), " intersect consolidation pairs...")

# Optional guard: keep only 1 receiving per joining (largest receiver),
# if you have already enforced this upstream you can leave it on, it will not change results.
# Adjust receiving_connections column name if needed.
if ("receiving_connections" %in% names(intersect_pairs)) {
  intersect_pairs <- intersect_pairs %>%
    group_by(joining_pwsid) %>%
    slice_max(receiving_connections, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# ============================================================================
# SURROGATE CONNECTIONS (robustness)
# If joining_connections is 0 or NA, fill using ceiling(pop / 3.3).
# Use effective_connections everywhere connections drive cost calculations.
# ============================================================================
intersect_costs <- intersect_pairs %>%
  mutate(
    effective_connections = case_when(
      (is.na(joining_connections) | joining_connections == 0) & !is.na(joining_population) ~
        ceiling(joining_population / SURROGATE_PPL_PER_CONN),
      TRUE ~ joining_connections
    )
  )

# Flag any rows still missing effective connections (rare, but possible)
n_bad_conn <- sum(is.na(intersect_costs$effective_connections) | intersect_costs$effective_connections == 0)
if (n_bad_conn > 0) {
  message("\nWARNING: ", n_bad_conn, " rows have missing/zero effective_connections after surrogate fill.")
  message("These rows will produce NA cost_per_connection.")
}

# ============================================================================
# COST CALCULATION (SWRCB 2024, Intersect)
# Key rules implemented:
# - Pipeline distance: fixed 1,000 ft
# - Regional adjustment applied to pipeline and service line costs only
# - Admin = 15% of (pipeline + service line construction only)
# - CEQA fixed for intersect
# - Adders applied additively with multiplier 1.481
# ============================================================================
intersect_costs <- intersect_costs %>%
  mutate(
    # Distances
    pipeline_distance_ft = PIPELINE_DIST_INTERSECT_FT,
    
    # Pipeline cost (regional)
    pipeline_cost_base = pipeline_distance_ft * PIPELINE_COST_PER_LF,
    pipeline_cost_regional = pipeline_cost_base * (1 + REGIONAL_MULTIPLIER),
    
    # Service line cost (regional), uses effective_connections
    service_line_cost_base = SERVICE_LINE_COST * effective_connections,
    service_line_cost_regional = service_line_cost_base * (1 + REGIONAL_MULTIPLIER),
    
    # Connection fees (not regionally adjusted), uses effective_connections
    connection_fees = CONNECTION_FEE_PWS * effective_connections,
    
    # Pipeline construction cost for admin base (pipeline + service lines only)
    pipeline_construction_cost = pipeline_cost_regional + service_line_cost_regional,
    
    # Admin cost (15% of pipeline construction cost only)
    admin_cost = ADMIN_PCT * pipeline_construction_cost,
    
    # CEQA cost (fixed)
    ceqa_cost = CEQA_INTERSECT,
    
    # Subtotal before adders
    subtotal_before_adders =
      pipeline_construction_cost +
      connection_fees +
      admin_cost +
      ceqa_cost,
    
    # Total capital cost using additive multiplier
    total_capital_cost = subtotal_before_adders * adder_multiplier,
    
    # Unit metric
    cost_per_connection = if_else(
      !is.na(effective_connections) & effective_connections > 0,
      total_capital_cost / effective_connections,
      NA_real_
    )
  )

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================
message("\n=== Cost Analysis Summary ===")
message("Total intersect consolidations analyzed: ", nrow(intersect_costs))

message("\nTotal Capital Cost Estimates:")
message("  Minimum: $", formatC(min(intersect_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Median:  $", formatC(median(intersect_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Maximum: $", formatC(max(intersect_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))

message("\nCost Per Connection:")
message("  Minimum: $", formatC(min(intersect_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Median:  $", formatC(median(intersect_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Maximum: $", formatC(max(intersect_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))

# ============================================================================
# SAVE RESULTS
# ============================================================================
dir_create(county_tab(slug))

out_file <- path(county_tab(slug), "consolidation_costs_intersect.csv")
write_csv(intersect_costs, out_file)

message("\n=== Files Saved ===")
message("Cost estimates: ", out_file)
message("\n=== Cost Analysis Complete ===")


# ============================================================================
# ROUTE COSTING (SWRCB 2024, Route via road network)
# Rules:
# - Pipeline distance: GIS route distance + 1,000 ft buffer
# - CEQA: $100,000
# - Regional adjustment applies to pipeline + service line costs only
# - Admin = 15% of (pipeline + service line construction only)
# - Adders applied additively with multiplier 1.481
# ============================================================================

message("\n=== Cost Analysis: Route Consolidations ===")

route_pairs <- all_pairs %>%
  filter(consolidation_type == "route")

message("Analyzing ", nrow(route_pairs), " route consolidation pairs...")

# Required fields for route
if (!("road_miles" %in% names(route_pairs))) {
  stop("Missing required column for route costing: road_miles")
}

route_costs <- route_pairs %>%
  mutate(
    # Surrogate connections (same logic as intersect)
    effective_connections = case_when(
      (is.na(joining_connections) | joining_connections == 0) & !is.na(joining_population) ~
        ceiling(joining_population / SURROGATE_PPL_PER_CONN),
      TRUE ~ joining_connections
    ),
    
    # Pipeline distance for route: road miles to feet + 1,000 ft buffer
    pipeline_distance_ft = case_when(
      is.na(road_miles) ~ NA_real_,
      !is.finite(road_miles) ~ NA_real_,
      road_miles < 0 ~ NA_real_,
      TRUE ~ road_miles * 5280 + 1000
    ),
    
    # Pipeline cost (regional)
    pipeline_cost_base = pipeline_distance_ft * PIPELINE_COST_PER_LF,
    pipeline_cost_regional = pipeline_cost_base * (1 + REGIONAL_MULTIPLIER),
    
    # Service line cost (regional)
    service_line_cost_base = SERVICE_LINE_COST * effective_connections,
    service_line_cost_regional = service_line_cost_base * (1 + REGIONAL_MULTIPLIER),
    
    # Connection fees (not regionally adjusted)
    connection_fees = CONNECTION_FEE_PWS * effective_connections,
    
    # Admin base: pipeline + service lines only
    pipeline_construction_cost = pipeline_cost_regional + service_line_cost_regional,
    admin_cost = ADMIN_PCT * pipeline_construction_cost,
    
    # CEQA fixed for route
    ceqa_cost = CEQA_ROUTE,
    
    # Subtotal and total (additive multiplier)
    subtotal_before_adders =
      pipeline_construction_cost +
      connection_fees +
      admin_cost +
      ceqa_cost,
    
    total_capital_cost = subtotal_before_adders * adder_multiplier,
    
    cost_per_connection = if_else(
      !is.na(effective_connections) & effective_connections > 0,
      total_capital_cost / effective_connections,
      NA_real_
    )
  )

# Quick diagnostics
n_bad_dist <- sum(is.na(route_costs$pipeline_distance_ft))
if (n_bad_dist > 0) {
  message("WARNING: ", n_bad_dist, " route rows have missing pipeline_distance_ft (bad/missing road_miles).")
}

message("\nRoute Total Capital Cost Estimates:")
message("  Minimum: $", formatC(min(route_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Median:  $", formatC(median(route_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Maximum: $", formatC(max(route_costs$total_capital_cost, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))

message("\nRoute Cost Per Connection:")
message("  Minimum: $", formatC(min(route_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Median:  $", formatC(median(route_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))
message("  Maximum: $", formatC(max(route_costs$cost_per_connection, na.rm = TRUE),
                                format = "f", digits = 0, big.mark = ","))

# Save route outputs
out_route_file <- path(county_tab(slug), "consolidation_costs_route.csv")
write_csv(route_costs, out_route_file)
message("\nRoute cost estimates saved: ", out_route_file)


all_costs <- bind_rows(
  intersect_costs %>% mutate(costing_case = "intersect"),
  route_costs %>% mutate(costing_case = "route")
)


# Save route outputs
out_route_file <- path(county_tab(slug), "consolidation_costs_route.csv")
write_csv(route_costs, out_route_file)
message("\nRoute cost estimates saved: ", out_route_file)


all_costs <- bind_rows(
  intersect_costs %>% mutate(costing_case = "intersect"),
  route_costs %>% mutate(costing_case = "route")
)

out_all_file <- path(county_tab(slug), "consolidation_costs_all.csv")
write_csv(all_costs, out_all_file)
message("Combined cost estimates saved: ", out_all_file)

# ============================================================================
# STEP 4: FUNDING VIABILITY SCREEN (SWRCB Methodology, Table 4)
# ----------------------------------------------------------------------------
# Goal:
#   Classify each consolidation candidate as "fundable" or "not fundable"
#   using the State Water Board funding viability thresholds.
#
# What this block assumes already exists in `all_costs`:
#   - effective_connections: the connection count used for costing and screening
#       - If you implemented the surrogate connection rule, this is the adjusted
#         connection count (ceiling(population/3.3) when connections are missing/0).
#       - If you did not need the surrogate rule, effective_connections equals
#         joining_connections.
#   - total_capital_cost: the final capital cost estimate from Step 3
#   - cost_per_connection: total_capital_cost / effective_connections
#   - costing_case: "intersect" or "route" (helps summarise results by type)
#
# Why the rules look "split":
#   The State uses different caps for small versus larger systems.
#   - For systems with < 75 connections: use a TOTAL project cost cap.
#   - For systems with >= 75 connections: use a COST-PER-CONNECTION cap.
#
# Important interpretation:
#   This is a policy screen, not a "value-for-money" screen.
#   Small systems can pass based on total cost even if cost per connection looks huge.
# ============================================================================

all_costs <- all_costs %>%
  mutate(
    # Assign which viability rule applies to each row (helps with debugging and reporting)
    viability_rule = case_when(
      !is.na(effective_connections) & effective_connections < 75  ~ "cap_total_7p2M",
      !is.na(effective_connections) & effective_connections >= 75 ~ "cap_cpc_96k",
      TRUE ~ "missing_connections"
    ),
    
    # Apply the appropriate threshold from Table 4
    # - <75 connections: project must be <= $7.2M total
    # - >=75 connections: project must be <= $96k per connection
    # - missing connections: default to FALSE so you can explicitly inspect these
    is_fundable = case_when(
      !is.na(effective_connections) & effective_connections < 75  ~ total_capital_cost <= 7200000,
      !is.na(effective_connections) & effective_connections >= 75 ~ cost_per_connection <= 96000,
      TRUE ~ FALSE
    )
  )

# ----------------------------------------------------------------------------
# Quick screen result by consolidation type:
#   This tells you how many projects pass the State viability rule for:
#   - intersect projects
#   - route projects
# ----------------------------------------------------------------------------
all_costs %>%
  count(costing_case, is_fundable)

# ----------------------------------------------------------------------------
# Sanity check: ensure you do not have silent NA values in is_fundable.
#   - n_true: number of projects passing Table 4
#   - n_false: number failing Table 4
#   - n_na: should be 0 if your logic always returns TRUE/FALSE
# ----------------------------------------------------------------------------
all_costs %>%
  summarise(
    n = n(),
    n_true = sum(is_fundable == TRUE, na.rm = TRUE),
    n_false = sum(is_fundable == FALSE, na.rm = TRUE),
    n_na = sum(is.na(is_fundable))
  )

# Save final screened results
out_viability_file <- path(county_tab(slug), "consolidation_costs_all_with_viability.csv")
write_csv(all_costs, out_viability_file)

# ============================================================================
# STORY OUTPUTS (Descriptive diagnostics)
# ----------------------------------------------------------------------------
# Purpose:
#   Provide a transparent narrative of what the model produced:
#   - How many candidates exist by type
#   - How many meet Table 4 viability thresholds
#   - How system size drives cost_per_connection (fixed-cost effect)
#   - Which cases deserve review because per-connection costs are extreme
# ============================================================================

message("\n=== Candidate Counts (by type) ===")
print(all_costs %>% count(costing_case))

message("\n=== Funding Viability (Table 4) ===")
print(all_costs %>% count(costing_case, is_fundable))

message("\n=== Which Viability Rule Applied ===")
print(all_costs %>% count(costing_case, viability_rule))

# ----------------------------------------------------------------------------
# Size distribution explains why cost_per_connection can look extreme.
# Small systems carry fixed costs (CEQA, admin, minimum pipe assumption),
# so cost_per_connection will spike when connections are very low.
# ----------------------------------------------------------------------------
message("\n=== Joining System Size Summary (effective connections) ===")
print(
  all_costs %>%
    summarise(
      n = n(),
      min_conn = min(effective_connections, na.rm = TRUE),
      p10_conn = quantile(effective_connections, 0.10, na.rm = TRUE),
      median_conn = median(effective_connections, na.rm = TRUE),
      p90_conn = quantile(effective_connections, 0.90, na.rm = TRUE),
      max_conn = max(effective_connections, na.rm = TRUE)
    )
)

# ----------------------------------------------------------------------------
# Cost range summary. This is not ranking, it is describing the spread.
# ----------------------------------------------------------------------------
message("\n=== Cost Summary (Total Capital Cost, 2024$) ===")
print(
  all_costs %>%
    summarise(
      min_total = min(total_capital_cost, na.rm = TRUE),
      median_total = median(total_capital_cost, na.rm = TRUE),
      max_total = max(total_capital_cost, na.rm = TRUE)
    )
)

message("\n=== Cost Summary (Cost per Connection, 2024$) ===")
print(
  all_costs %>%
    summarise(
      min_cpc = min(cost_per_connection, na.rm = TRUE),
      median_cpc = median(cost_per_connection, na.rm = TRUE),
      max_cpc = max(cost_per_connection, na.rm = TRUE)
    )
)

# ----------------------------------------------------------------------------
# "Review-needed" flag for interpretability:
# For <75-connection systems, Table 4 uses total cost cap (not cpc).
# This flag does NOT imply unfundable; it identifies cases where
# cost_per_connection is very high and deserves context in reporting.
# ----------------------------------------------------------------------------
all_costs <- all_costs %>%
  mutate(
    review_needed_extreme_cpc = (effective_connections < 75) & (cost_per_connection > 96000)
  )

message("\n=== Review-needed cases: small systems with very high cost per connection ===")
print(
  all_costs %>%
    filter(review_needed_extreme_cpc) %>%
    arrange(desc(cost_per_connection)) %>%
    select(
      costing_case,
      joining_pwsid, receiving_pwsid,
      effective_connections,
      total_capital_cost, cost_per_connection,
      road_miles
    )
)

# ----------------------------------------------------------------------------
# Save final outputs after viability and diagnostics flags.
# ----------------------------------------------------------------------------
out_viability_file <- path(county_tab(slug), "consolidation_costs_all_with_viability.csv")
write_csv(all_costs, out_viability_file)
message("\nFinal screened results saved: ", out_viability_file)


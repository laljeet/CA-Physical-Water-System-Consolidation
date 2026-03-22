# Author: Laljeet Sangha, UCCE Kern
# Created: 2025-08-08
#
# Purpose:
#   Define project root paths and helper functions for county assets.
#   Centralize folder creation and slug rules used by all scripts.
#
# Inputs:
#   - None
#
# Outputs:
#   - Creates base folders if missing:
#       data_in/, counties/, combined/tables/, logs/
#
# Dependencies:
#   - Packages: fs, here
#
# Notes:
#   - Use to_slug to standardize county folder names
#   - All other scripts should source this file first
# --------------------------------------------------------------
library(fs)
# Use here to lock the project root
if (!requireNamespace("here", quietly = TRUE)) stop("Install 'here'")
p_root    <- here::here()                 # project root
p_data_in <- path(p_root, "data_in")
p_counties<- path(p_root, "counties")
p_combined<- path(p_root, "combined")
p_tables  <- path(p_combined, "tables")
p_logs    <- path(p_root, "logs")

dir_create(p_data_in); dir_create(p_counties); dir_create(p_tables); dir_create(p_logs)

to_slug <- function(x) {
  x |>
    trimws() |>
    tolower() |>
    iconv(from = "", to = "ASCII//TRANSLIT", sub = "") |>
    (\(z) gsub("[^a-z0-9]+", "_", z))() |>
    (\(z) gsub("^_+|_+$", "", z))() |>
    (\(z) gsub("_+", "_", z))()
}

county_dir <- function(slug) path(p_counties, slug)
county_maps<- function(slug) path(county_dir(slug), "maps")
county_figs<- function(slug) path(county_dir(slug), "figs")
county_tab <- function(slug) path(county_dir(slug), "tables")
county_meta<- function(slug) path(county_dir(slug), "meta.yml")

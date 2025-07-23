# Activate renv (reproducible environment)
source("renv/activate.R")

# Auto-load packages (safe loading)
packages <- c("dplyr", "stringr", "ggplot2", "readr", 
              "sf", "stringi", "janitor", "rsample",
              "patchwork", "scales", "lubridate",
              "purrr", "forcats", "forecast", "targets", "tarchetypes",
              "ggpubr", "collapse", "vegan", "tidyverse", "ggspatial",
              "tigris", "permute", "viridis", "FactoMineR",
              "scales", "ggfortify", "bookdown", "raster", "tiff")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Missing package: %s", pkg))
  } else {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

message("Packages loaded.")

# Safe sourcing of functions
safe_source <- function(file) {
  tryCatch(source(file), error = function(e) message("Error in ", file, ": ", e$message))
}

function_files <- list.files("src/functions", full.names = TRUE, pattern = "\\.R$")
lapply(function_files, safe_source)

# Set some constants
VN <- "VisioNature"
GN <- "GeoNature"

rm(pkg, packages, function_files)
message(".Rprofile loaded successfully.")

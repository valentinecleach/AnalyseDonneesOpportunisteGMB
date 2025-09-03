# Activate renv if present (for reproducibility)
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
  message("renv activated.")
}

# List of required packages
packages <- c(
  "dplyr", "stringr", "ggplot2", "readr", "sf", "stringi", "janitor", "rsample",
  "patchwork", "scales", "lubridate", "purrr", "forcats", "forecast", "targets", "tarchetypes",
  "ggpubr", "collapse", "vegan", "ggspatial", "tigris", "permute", "viridis", "FactoMineR",
  "ggfortify", "bookdown", "Rcpp", "car", "corrplot", "Kendall", "terra", "exactextractr",
  "unmarked", "GGally", "AICcmodavg", "giscoR"
)

# Install missing packages
missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# Load only needed packages for most scripts
to_load <- c("dplyr", "ggplot2", "sf")
suppressPackageStartupMessages(
  invisible(lapply(to_load, function(pkg) library(pkg, character.only = TRUE)))
)

message("Project packages installed, and loaded")
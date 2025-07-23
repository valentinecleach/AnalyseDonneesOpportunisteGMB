# init.R

# Install missing packages
packages <- c("dplyr", "stringr", "ggplot2", "readr", 
              "sf", "stringi", "janitor", "rsample",
              "patchwork", "scales", "lubridate",
              "purrr", "forcats", "forecast", "targets", "tarchetypes",
              "ggpubr", "collapse", "vegan", "ggspatial",
              "tigris", "permute", "viridis", "FactoMineR",
              "ggfortify", "bookdown", "Rcpp")

missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing)) install.packages(missing)

# Load only needed packages
to_load <- c("dplyr", "ggplot2", "sf")
suppressPackageStartupMessages(lapply(to_load, function(pkg) library(pkg, character.only = TRUE)))

# Source functions safely
safe_source <- function(file) {
  tryCatch(source(file), error = function(e) message("Error in ", file, ": ", e$message))
}
if (dir.exists("src/functions")) {
  function_files <- list.files("src/functions", full.names = TRUE, pattern = "\\.R$")
  invisible(lapply(function_files, safe_source))
}

message("Project environment initialized.")

source("renv/activate.R")

# Auto-load common packages
packages <- c("dplyr", "stringr", "ggplot2", "readr", 
              "sf", "stringi", "janitor", "rsample",
              "patchwork", "scales", "lubridate",
              "purrr", "forcasts","forecast", "targets", "tarchetypes",
              "ggpubr", "collapse", "vegan", "tidyverse", "ggspatial",
              "tigris", "permute")  # Add your own here

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' is not installed.", pkg))
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Optional: message to confirm
message("Packages auto-loaded: ", paste(packages, collapse = ", "))


source("src/functions/divers.R")
source("src/functions/analysedonnees.R")
source("src/functions/cartographie.R")
source("src/functions/GLM.R")
source("src/functions/stats_desc.R")

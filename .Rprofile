source("renv/activate.R")


packages <- c("dplyr", "stringr", "ggplot2", "readr", 
              "sf", "stringi", "janitor", "rsample",
              "patchwork", "scales", "lubridate",
              "purrr", "forcats","forecast", "targets", "tarchetypes",
              "ggpubr", "collapse", "vegan", "tidyverse", "ggspatial",
              "tigris", "permute", "viridis", "FactoMineR",
              "scales", )  

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' is not installed.", pkg))
  } else {
    library(pkg, character.only = TRUE)
  }
}


message("Packages auto-loaded: ", paste(packages, collapse = ", "))


source("src/functions/divers.R")
source("src/functions/analysedonnees.R")
source("src/functions/cartographie.R")
source("src/functions/GLM.R")
source("src/functions/stats_desc.R")

VN <- "VisioNature"
GN <- "GeoNature"

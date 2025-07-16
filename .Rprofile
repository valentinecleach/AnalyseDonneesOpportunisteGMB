source("renv/activate.R")
source("src/functions/divers.R")


# Auto-load common packages
packages <- c("dplyr", "stringr", "ggplot2", "readr", 
              "sf", "stringi", "janitor")  # Add your own here

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' is not installed.", pkg))
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Optional: message to confirm
message("Packages auto-loaded: ", paste(packages, collapse = ", "))

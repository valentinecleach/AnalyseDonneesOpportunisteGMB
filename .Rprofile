# Minimal .Rprofile: activates renv, no heavy memory usage

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
  message("renv activated.")
}

# Only do this in interactive sessions (avoid doing anything for Rscript or batch runs)
if (interactive()) {
  message("Interactive session detected. Consider running source('init.R') to load packages and functions.")
}

message(".Rprofile loaded.")


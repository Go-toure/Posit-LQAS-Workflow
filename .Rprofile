# Project-specific R settings
options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  stringsAsFactors = FALSE,
  max.print = 1000
)

# Set library paths
.libPaths(c("~/R/library", .libPaths()))

# Load common packages on startup (optional)
if (interactive()) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(data.table)
    library(qs)
    library(logger)
    library(here)
  })
  message("🚀 Posit LQAS Workflow Loaded")
  message("   Working Directory: ", getwd())
}

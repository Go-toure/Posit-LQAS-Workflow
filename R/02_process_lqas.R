#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Processing - Optimized for Large Datasets
# Uses data.table for speed, qs for storage
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(qs)
  library(logger)
  library(here)
  library(future)
  library(furrr)
  library(lubridate)
  library(stringr)
  library(janitor)
})

log_appender(appender_file(here("logs/process.log")))
log_info("🚀 Starting LQAS Processing Pipeline")

# ============================================================
# Helper Functions (Optimized)
# ============================================================

# Efficient column renaming
rename_repetitive_columns <- function(dt) {
  pattern <- "^Count_HH\\[\\d+\\]/Count_HH/"
  new_names <- names(dt) %>%
    str_replace(pattern, "")
  setnames(dt, new_names)
  return(dt)
}

# Fast binary conversion with data.table
fast_binary_convert <- function(x) {
  fcase(
    tolower(x) %in% c("yes", "y", "1"), 1,
    tolower(x) %in% c("no", "n", "0"), 0,
    default = NA_real_
  )
}

# ============================================================
# Process Single File with qs
# ============================================================

process_single_file <- function(file_path, output_dir) {
  file_name <- basename(file_path)
  log_info("Processing: {file_name}")
  
  # Read with qs (much faster)
  tryCatch({
    dt <- qread(file_path)
    if (!is.data.table(dt)) dt <- as.data.table(dt)
  }, error = function(e) {
    log_warn("qread failed, trying readRDS: {e$message}")
    dt <- readRDS(file_path)
    if (!is.data.table(dt)) dt <- as.data.table(dt)
  })
  
  if (nrow(dt) == 0) {
    log_warn("Empty dataset: {file_name}")
    return(NULL)
  }
  
  log_info("  Initial: {nrow(dt)} rows, {ncol(dt)} columns")
  
  # Process steps (using data.table for speed)
  dt <- rename_repetitive_columns(dt)
  
  # Apply custom rules
  form_id <- as.numeric(str_extract(file_name, "\\d+"))
  if (form_id == 3583) dt[, Country := "GHA"]
  if (form_id == 8834) dt[, Region := District]
  
  # Select relevant columns efficiently
  cols_to_keep <- c(
    "Response", "roundNumber", "Country", "Region", "District", "Date_of_LQAS",
    "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_altitude",
    grep("^Count_HH\\[\\d+\\]/(Sex_Child|FM_Child|FM_ChildR|FM_ChildL|Reason_Not_FM|Reason_NC_NFM|Reason_ABS_NFM|Care_Giver_Informed_SIA)$",
         names(dt), value = TRUE),
    "Count_HH_count", "Cluster"
  )
  cols_to_keep <- intersect(cols_to_keep, names(dt))
  dt <- dt[, ..cols_to_keep]
  
  # Clean and standardize
  dt[, `:=`(
    Country = str_squish(toupper(Country)),
    Region = str_squish(toupper(Region)),
    District = str_squish(toupper(District))
  )]
  
  # Standardize binary variables
  fm_cols <- grep("^Count_HH\\[\\d+\\]/FM_Child", names(dt), value = TRUE)
  cgs_cols <- grep("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA", names(dt), value = TRUE)
  
  for (col in fm_cols) set(dt, j = col, value = fast_binary_convert(dt[[col]]))
  for (col in cgs_cols) set(dt, j = col, value = fast_binary_convert(dt[[col]]))
  
  # Calculate metrics efficiently
  sex_cols <- grep("^Count_HH\\[\\d+\\]/Sex_Child", names(dt), value = TRUE)
  
  dt[, female_sampled := rowSums(.SD, na.rm = TRUE), .SDcols = sex_cols]
  dt[, male_sampled := Count_HH_count - female_sampled]
  
  dt[, total_vaccinated := rowSums(.SD, na.rm = TRUE), .SDcols = fm_cols]
  dt[, missed_child := Count_HH_count - total_vaccinated]
  
  # Save processed data
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(file_name), ".qs"))
  qsave(dt, output_file, preset = "high")
  
  log_info("  ✅ Saved: {nrow(dt)} rows to {output_file}")
  return(dt)
}

# ============================================================
# Parallel Processing
# ============================================================

process_all_files <- function(input_dir, output_dir, chunk_size = 10) {
  # Get all qs files
  files <- dir_ls(input_dir, glob = "*.qs")
  
  if (length(files) == 0) {
    log_warn("No qs files found in {input_dir}")
    return(NULL)
  }
  
  log_info("Found {length(files)} files to process")
  
  # Process in parallel chunks
  plan(multisession, workers = min(availableCores() - 1, 8))
  
  results <- future_map(
    files,
    function(f) process_single_file(f, output_dir),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  
  # Summary
  successful <- sum(!sapply(results, is.null))
  log_info("✅ Processing complete: {successful}/{length(files)} successful")
  
  return(results)
}

# ============================================================
# Main Execution
# ============================================================

input_dir <- here("data/raw")
output_dir <- here("data/processed")

dir_create(output_dir)

results <- process_all_files(input_dir, output_dir)

log_info("🎉 Processing pipeline complete!")
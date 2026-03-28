#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Fetcher - Optimized with qs
# Handles large datasets efficiently
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
  library(qs)
  library(data.table)
  library(logger)
  library(here)
  library(future)
  library(furrr)
  library(config)
})

# Configure logging
log_appender(appender_file(here("logs/fetch.log")))
log_info("🚀 Starting LQAS Data Fetch")

# Load configuration
config <- config::get(file = here("config/config.yml"))

# API setup
ona_api_token <- Sys.getenv("ONA_API_TOKEN")
if (ona_api_token == "") {
  log_error("ONA_API_TOKEN not set")
  quit(status = 1)
}

# ============================================================
# Optimized Fetch Function with Parallel Processing
# ============================================================

fetch_form_parallel <- function(form_id) {
  log_info("Fetching form {form_id}...")
  
  all_data <- list()
  page <- 1
  page_size <- 10000
  
  while (TRUE) {
    url <- paste0(config$ona$base_url, "/", form_id, ".json")
    response <- GET(
      url,
      query = list(page = page, page_size = page_size),
      add_headers(Authorization = paste("Token", ona_api_token)),
      timeout(120)
    )
    
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed")
      if (length(data) == 0) break
      
      log_info("  Page {page}: {length(data)} records")
      all_data <- c(all_data, data)
      page <- page + 1
    } else if (status_code(response) == 404) {
      log_warn("Form {form_id} not found")
      break
    } else {
      log_error("Error {status_code(response)} for form {form_id}")
      break
    }
  }
  
  log_info("✅ Form {form_id}: {length(all_data)} total records")
  
  if (length(all_data) > 0) {
    # Convert to data.table for efficiency
    dt <- rbindlist(all_data, fill = TRUE)
    
    # Save with qs (compressed, fast)
    output_path <- here("data/raw", paste0(form_id, ".qs"))
    qsave(dt, output_path, preset = "high")
    log_info("  Saved to {output_path}")
    
    return(list(form_id = form_id, records = nrow(dt), success = TRUE))
  } else {
    return(list(form_id = form_id, records = 0, success = FALSE))
  }
}

# ============================================================
# Parallel Fetching
# ============================================================

form_ids <- config$ona$form_ids

# Use future_map for parallel processing
log_info("Fetching {length(form_ids)} forms in parallel...")
results <- future_map(form_ids, fetch_form_parallel, .progress = TRUE)

# Summary
success_count <- sum(sapply(results, function(x) x$success))
total_records <- sum(sapply(results, function(x) x$records))

log_info("✅ Fetch complete: {success_count}/{length(form_ids)} forms, {total_records} records")

# Save summary
summary <- tibble(
  timestamp = Sys.time(),
  forms_processed = length(form_ids),
  successful = success_count,
  total_records = total_records
)

qsave(summary, here("data/raw/fetch_summary.qs"))

log_info("🎉 Data fetch complete!")
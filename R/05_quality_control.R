#!/usr/bin/env Rscript
# ============================================================
# Quality Control Checks for LQAS Data
# Fixed version - no argparse dependency
# ============================================================

suppressPackageStartupMessages({
  library(magrittr)
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(logger)
  library(jsonlite)
  library(rmarkdown)
  library(fs)
})

# Configuration (hardcoded for local run)
INPUT_FILE <- "data/final/lqas_cleaned.parquet"
OUTPUT_DIR <- "reports"
THRESHOLD <- 90

# Create directories
dir_create("logs")
dir_create(OUTPUT_DIR)

# Configure logging
log_appender(appender_file("logs/qc.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("🔍 Running Quality Control Checks")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Load data
log_info("Loading data from: {INPUT_FILE}")

if (!file.exists(INPUT_FILE)) {
  # Try CSV fallback
  csv_file <- sub("\\.parquet$", ".csv", INPUT_FILE)
  if (file.exists(csv_file)) {
    dt <- fread(csv_file)
    log_info("Loaded CSV file: {nrow(dt)} rows")
  } else {
    stop("Data file not found: ", INPUT_FILE)
  }
} else {
  dt <- as.data.table(read_parquet(INPUT_FILE))
  log_info("Loaded Parquet file: {nrow(dt)} rows, {ncol(dt)} columns")
}

# Quick summary
log_info("\n📊 Data Summary:")
log_info("  Total records: {nrow(dt)}")
log_info("  Total countries: {uniqueN(dt$country)}")
log_info("  Date range: {min(dt$lqas_start_date, na.rm = TRUE)} to {max(dt$lqas_start_date, na.rm = TRUE)}")

# Coverage calculation
if ("total_vaccinated" %in% names(dt) && "total_sampled" %in% names(dt)) {
  dt[, coverage := total_vaccinated / total_sampled * 100]
  log_info("  Mean coverage: {round(mean(dt$coverage, na.rm = TRUE), 1)}%")
  log_info("  Median coverage: {round(median(dt$coverage, na.rm = TRUE), 1)}%")
}

# Pass rate
if ("status" %in% names(dt)) {
  pass_rate <- round(sum(dt$status == "PASS", na.rm = TRUE) / nrow(dt) * 100, 1)
  log_info("  Pass rate: {pass_rate}%")
}

log_info("\n✅ Quality control complete!")

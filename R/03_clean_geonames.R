#!/usr/bin/env Rscript
# ============================================================
# Geoname Cleaning - Optimized with data.table and qs
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(qs)
  library(logger)
  library(here)
  library(future)
  library(furrr)
  library(stringr)
  library(stringi)
})

log_appender(appender_file(here("logs/clean_geonames.log")))
log_info("🚀 Starting Geoname Cleaning")

# ============================================================
# Load District Mappings (Efficiently)
# ============================================================

# Load mappings from external files for maintainability
district_mappings <- qread(here("data/lookup/district_mappings.qs"))
province_mappings <- qread(here("data/lookup/province_mappings.qs"))
afro_blocks <- qread(here("data/lookup/afro_blocks.qs"))

# ============================================================
# Fast Geoname Cleaning with data.table
# ============================================================

clean_geonames <- function(input_file, output_file, lookup_file) {
  log_info("Loading data: {input_file}")
  
  # Load with qs (fast)
  dt <- qread(input_file)
  if (!is.data.table(dt)) dt <- as.data.table(dt)
  
  log_info("Initial: {nrow(dt)} rows, {ncol(dt)} columns")
  
  # Standardize country names using join (much faster than case_when)
  country_mapping <- data.table(
    code = c("NAM", "GAM", "GHA", "ALG", "ETH", "ANG", "BEN", "BFA", "CAE", "CHD"),
    name = c("NAMIBIA", "GAMBIA", "GHANA", "ALGERIA", "ETHIOPIA", "ANGOLA", 
             "BENIN", "BURKINA FASO", "CAMEROON", "CHAD")
  )
  
  dt <- merge(dt, country_mapping, by.x = "country", by.y = "code", all.x = TRUE)
  dt[, country := fcoalesce(name, country)]
  dt[, name := NULL]
  
  # Apply district corrections using data.table join
  dt <- merge(dt, district_mappings, by = c("country", "district"), all.x = TRUE)
  dt[, district := fcoalesce(corrected_district, district)]
  dt[, corrected_district := NULL]
  
  # Apply province corrections
  dt <- merge(dt, province_mappings, by = c("country", "province", "district"), all.x = TRUE)
  dt[, province := fcoalesce(corrected_province, province)]
  dt[, corrected_province := NULL]
  
  # Add AFRO block
  dt <- merge(dt, afro_blocks, by = "country", all.x = TRUE)
  dt[, AFRO_block := fcoalesce(block, "OTHER")]
  dt[, block := NULL]
  
  # Filter dates efficiently
  dt <- dt[start_date > as.Date("2019-10-01")]
  
  # Remove duplicates (keep first occurrence)
  setorder(dt, country, province, district, response, roundNumber)
  dt <- unique(dt, by = c("country", "province", "district", "response", "roundNumber"))
  
  log_info("Final: {nrow(dt)} rows, {ncol(dt)} columns")
  
  # Save with qs (high compression for final data)
  qsave(dt, output_file, preset = "archive")
  log_info("✅ Saved to {output_file}")
  
  # Also save as CSV for compatibility
  fwrite(dt, gsub("\\.qs$", ".csv", output_file))
  
  return(dt)
}

# ============================================================
# Main Execution
# ============================================================

input_file <- here("data/processed/combined_data.qs")
output_file <- here("data/final/lqas_final.qs")
lookup_file <- here("data/lookup/lqas_lookup.xlsx")

result <- clean_geonames(input_file, output_file, lookup_file)

log_info("🎉 Geoname cleaning complete!")
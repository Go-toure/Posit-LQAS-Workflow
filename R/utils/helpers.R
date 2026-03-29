# ============================================================
# Helper Functions for LQAS Pipeline
# Optimized for Parquet workflow and special cases
# ============================================================

#' Fast binary conversion (Yes/No to 0/1)
#' @param x Character or numeric vector
#' @return Numeric vector (0, 1, or NA)
fast_binary_convert <- function(x) {
  data.table::fcase(
    tolower(x) %in% c("yes", "y", "1", 1), 1,
    tolower(x) %in% c("no", "n", "0", 0), 0,
    default = NA_real_
  )
}

#' Clean and standardize text
#' @param x Character vector
#' @return Cleaned uppercase character vector
clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- toupper(x)
  # Remove any remaining non-ASCII characters
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  return(x)
}

#' Calculate performance category based on missed children
#' @param total_missed Numeric vector of missed children counts
#' @return Character vector of performance levels
performance_category <- function(total_missed) {
  data.table::fcase(
    total_missed < 4, "high",
    total_missed < 9, "moderate",
    total_missed < 20, "poor",
    default = "very poor"
  )
}

#' Calculate pass/fail status
#' @param total_missed Numeric vector of missed children counts
#' @return Character vector ("PASS" or "FAIL")
pass_fail_status <- function(total_missed) {
  data.table::fcase(
    total_missed <= 3, "PASS",
    default = "FAIL"
  )
}

#' Safely read multiple file formats (Parquet, CSV, RDS, QS)
#' @param file_path Path to file
#' @return data.table
read_any_format <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  
  result <- switch(ext,
    "parquet" = arrow::read_parquet(file_path),
    "csv" = data.table::fread(file_path),
    "rds" = tryCatch(qs::qread(file_path), error = function(e) readRDS(file_path)),
    "qs" = qs::qread(file_path),
    stop("Unsupported file format: ", ext)
  )
  
  return(as.data.table(result))
}

#' Calculate vaccination coverage percentage
#' @param vaccinated Numeric vector of vaccinated counts
#' @param sampled Numeric vector of sampled counts
#' @return Numeric vector of coverage percentages
calculate_coverage <- function(vaccinated, sampled) {
  ifelse(sampled > 0, round(vaccinated / sampled * 100, 2), NA_real_)
}

#' Standardize country names to consistent format
#' @param country Character vector of country names/codes
#' @return Standardized country names
standardize_country <- function(country) {
  country <- toupper(trimws(country))
  
  data.table::fcase(
    country %in% c("NAM", "NAMIBIA"), "NAMIBIA",
    country %in% c("GAM", "GAMBIA"), "GAMBIA", 
    country %in% c("GHA", "GHANA"), "GHANA",
    country %in% c("ALG", "ALGERIA"), "ALGERIA",
    country %in% c("ETH", "ETHIOPIA"), "ETHIOPIA",
    country %in% c("ANG", "ANGOLA"), "ANGOLA",
    country %in% c("BEN", "BENIN"), "BENIN",
    country %in% c("BFA", "BURKINA FASO"), "BURKINA FASO",
    country %in% c("CAE", "CAMEROON"), "CAMEROON",
    country %in% c("CIV", "COTE D IVOIRE"), "COTE D IVOIRE",
    country %in% c("GUI", "GUINEA"), "GUINEA",
    country %in% c("KEN", "KENYA"), "KENYA",
    country %in% c("MAL", "MALI"), "MALI",
    country %in% c("MAU", "MAURITANIA"), "MAURITANIA",
    country %in% c("MOZ", "MOZAMBIQUE"), "MOZAMBIQUE",
    country %in% c("NIE", "NIGERIA"), "NIGERIA",
    country %in% c("NIG", "NIGER"), "NIGER",
    country %in% c("RCA", "CENTRAL AFRICAN REPUBLIC"), "CENTRAL AFRICAN REPUBLIC",
    country %in% c("RDC", "DRC", "DEMOCRATIC REPUBLIC OF THE CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO",
    country %in% c("SEN", "SENEGAL"), "SENEGAL",
    country %in% c("SIL", "SIERRA LEONE"), "SIERRA LEONE",
    country %in% c("TOG", "TOGO"), "TOGO",
    country %in% c("UGA", "UGANDA"), "UGANDA",
    country %in% c("ZMB", "ZAMBIA"), "ZAMBIA",
    country %in% c("ZIM", "ZIMBABWE"), "ZIMBABWE",
    default = country
  )
}

#' Validate GPS coordinates
#' @param lat Latitude values
#' @param lon Longitude values
#' @return Logical vector indicating if coordinates are valid
is_valid_gps <- function(lat, lon) {
  lat_valid <- !is.na(lat) & between(as.numeric(lat), -90, 90)
  lon_valid <- !is.na(lon) & between(as.numeric(lon), -180, 180)
  return(lat_valid & lon_valid)
}

#' Create a unique identifier for each district-round combination
#' @param data Data frame with country, province, district, response, roundNumber columns
#' @return Character vector of unique IDs
create_district_round_id <- function(data) {
  paste(
    clean_text(data$country),
    clean_text(data$province),
    clean_text(data$district),
    clean_text(data$response),
    clean_text(data$roundNumber),
    sep = "_"
  )
}

#' Log processing time for a step
#' @param step_name Name of the processing step
#' @param start_time Start time from Sys.time()
#' @return Invisible, logs to console/logger
log_step_time <- function(step_name, start_time) {
  elapsed <- round(as.numeric(Sys.time() - start_time, units = "secs"), 2)
  logger::log_info("  ⚙️ {step_name}: {elapsed}s")
  return(invisible(elapsed))
}

#' Safe file writing with directory creation
#' @param data Data to write
#' @param file_path Output file path
#' @param format Format ("csv", "parquet", or "rds")
#' @return Logical indicating success
safe_write <- function(data, file_path, format = "csv") {
  # Create directory if it doesn't exist
  dir_create(dirname(file_path))
  
  success <- tryCatch({
    switch(format,
      "csv" = data.table::fwrite(data, file_path),
      "parquet" = arrow::write_parquet(data, file_path),
      "rds" = saveRDS(data, file_path),
      stop("Unsupported format: ", format)
    )
    TRUE
  }, error = function(e) {
    logger::log_error("Failed to write {file_path}: {e$message}")
    FALSE
  })
  
  if (success) {
    logger::log_info("✅ Saved to {file_path}")
  }
  
  return(success)
}
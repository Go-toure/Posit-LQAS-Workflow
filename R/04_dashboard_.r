---
title: "LQAS Monitoring Dashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
    theme: flatly
    source_code: embed
    self_contained: true
    navbar:
      - { title: "Overview", icon: "fa-dashboard" }
      - { title: "District Analysis", icon: "fa-map-marker" }
      - { title: "Performance", icon: "fa-chart-line" }
      - { title: "Data Explorer", icon: "fa-table" }
params:
  data_file: "data/final/lqas_dashboard_input.parquet"
  run_date: !r Sys.Date()
---

```{r setup, include=FALSE}
library(flexdashboard)
library(arrow)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(scales)

load_data <- function(file_path) {
  if (file.exists(file_path)) {
    dt <- tryCatch({
      as.data.table(read_parquet(file_path))
    }, error = function(e) {
      # Try CSV if parquet fails
      csv_file <- sub("\\.parquet$", ".csv", file_path)
      if (file.exists(csv_file)) {
        fread(csv_file)
      } else {
        NULL
      }
    })
    return(dt)
  }
  return(NULL)
}

# Load data
dt <- load_data(params$data_file)
data_available <- !is.null(dt) && nrow(dt) > 0

if (data_available) {
  # Fix column name mappings based on actual output from processing script
  # The processing script outputs: country, province, district, response, vaccine.type,
  # roundNumber, numbercluster, start_date, end_date, total_sampled, total_vaccinated,
  # total_missed, status, performance, etc.
  
  # Ensure date columns are proper dates
  if ("start_date" %in% names(dt)) {
    dt[, lqas_start_date := as.Date(start_date)]
  } else if ("lqas_start_date" %in% names(dt)) {
    dt[, lqas_start_date := as.Date(lqas_start_date)]
  }
  
  # Calculate coverage percentage
  dt[, coverage_pct := round(total_vaccinated / total_sampled * 100, 1)]
  
  # Use status field from processing (already has "Pass"/"Fail")
  if (!"pass_status" %in% names(dt) && "status" %in% names(dt)) {
    dt[, pass_status := status]
  } else if (!"pass_status" %in% names(dt)) {
    dt[, pass_status := ifelse(total_missed <= 3, "PASS", "FAIL")]
  }
  
  # Get unique values for filters
  countries <- unique(dt$country) %>% sort()
  vaccine_types <- unique(dt$vaccine.type) %>% na.omit() %>% sort()
  
  # Try to get AFRO blocks if available, otherwise create from country
  if ("afro_block" %in% names(dt)) {
    afro_blocks <- unique(dt$afro_block) %>% na.omit() %>% sort()
  } else {
    # Create simple region mapping based on country
    dt[, afro_block := case_when(
      country %in% c("BEN", "BFA", "CIV", "GHA", "GUI", "MLI", "NER", "NGA", "SEN", "TGO") ~ "West Africa",
      country %in% c("CMR", "CAF", "CHA", "COD", "COG", "GAB", "EQG") ~ "Central Africa",
      country %in% c("BDI", "DJI", "ERI", "ETH", "KEN", "RWA", "SOM", "SSD", "SDN", "TAN", "UGA") ~ "East Africa",
      country %in% c("ANG", "BOT", "LES", "MAD", "MAW", "MOZ", "NAM", "RSA", "SWZ", "ZAM", "ZIM") ~ "Southern Africa",
      TRUE ~ "Other"
    )]
    afro_blocks <- unique(dt$afro_block) %>% na.omit() %>% sort()
  }
  
  # Calculate global metrics
  global_metrics <- dt[, .(
    total_districts = uniqueN(district),
    total_clusters = sum(numbercluster, na.rm = TRUE),
    total_sampled = sum(total_sampled, na.rm = TRUE),
    total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
    pass_rate = round(sum(pass_status == "Pass", na.rm = TRUE) / .N * 100, 1),
    mean_coverage = round(mean(coverage_pct, na.rm = TRUE), 1)
  )]
  
  latest_date <- max(dt$lqas_start_date, na.rm = TRUE)
  
  # Debug info
  cat("Data loaded successfully!\n")
  cat("Columns available:", paste(names(dt), collapse=", "), "\n")
  cat("Total rows:", nrow(dt), "\n")
  cat("Date range:", min(dt$lqas_start_date, na.rm=TRUE), "to", latest_date, "\n")
}
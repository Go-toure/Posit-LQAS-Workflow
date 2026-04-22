# Set working directory
setwd("C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow")

# Dashboard content
dashboard_content <- '---
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
  data_file: "../data/final/lqas_dashboard_input.parquet"
  run_date: !r Sys.Date()
---

`r ''````{r setup, include=FALSE}
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
      csv_file <- sub("\\\\.parquet$", ".csv", file_path)
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

dt <- load_data(params$data_file)
data_available <- !is.null(dt) && nrow(dt) > 0

if (data_available) {
  if ("lqas_start_date" %in% names(dt)) {
    dt[, lqas_start_date := as.Date(lqas_start_date)]
  } else if ("start_date" %in% names(dt)) {
    dt[, lqas_start_date := as.Date(start_date)]
  }
  
  dt[, coverage_pct := round(total_vaccinated / total_sampled * 100, 1)]
  dt[, pass_status := ifelse(total_missed <= 3, "PASS", "FAIL")]
  
  if (!"afro_block" %in% names(dt)) {
    dt[, afro_block := fcase(
      country %in% c("BEN", "BFA", "CIV", "GHA", "GUI", "MLI", "NER", "NGA", "SEN", "TGO"), "West Africa",
      country %in% c("CMR", "CAF", "TCD", "COD", "COG", "GAB"), "Central Africa",
      country %in% c("BDI", "ETH", "KEN", "RWA", "SOM", "SSD", "SDN", "TZA", "UGA"), "East Africa",
      country %in% c("AGO", "MOZ", "ZAF", "ZMB", "ZWE"), "Southern Africa",
      default = "Other"
    )]
  }
  
  countries <- unique(dt$country) %>% sort()
  vaccine_types <- unique(dt$vaccine.type) %>% na.omit() %>% sort()
  afro_blocks <- unique(dt$afro_block) %>% na.omit() %>% sort()
  
  global_metrics <- dt[, .(
    total_districts = uniqueN(district),
    total_clusters = sum(numbercluster, na.rm = TRUE),
    total_sampled = sum(total_sampled, na.rm = TRUE),
    total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
    pass_rate = round(sum(pass_status == "PASS", na.rm = TRUE) / .N * 100, 1),
    mean_coverage = round(mean(coverage_pct, na.rm = TRUE), 1)
  )]
  
  latest_date <- max(dt$lqas_start_date, na.rm = TRUE)
}

#!/usr/bin/env Rscript
# ============================================================
# Quality Control Checks
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(qs)
  library(ggplot2)
  library(logger)
  library(here)
})

log_appender(appender_file(here("logs/qc.log")))
log_info("🔍 Running Quality Control Checks")

# ============================================================
# QC Functions
# ============================================================

check_data_completeness <- function(dt) {
  log_info("Checking data completeness...")
  
  completeness <- dt[, .(
    total_rows = .N,
    complete_country = sum(!is.na(country)),
    complete_province = sum(!is.na(province)),
    complete_district = sum(!is.na(district)),
    complete_response = sum(!is.na(response)),
    complete_vaccinated = sum(!is.na(total_vaccinated)),
    complete_sampled = sum(!is.na(total_sampled))
  )]
  
  completeness[, `:=`(
    pct_country = complete_country / total_rows * 100,
    pct_province = complete_province / total_rows * 100,
    pct_district = complete_district / total_rows * 100,
    pct_response = complete_response / total_rows * 100,
    pct_vaccinated = complete_vaccinated / total_rows * 100,
    pct_sampled = complete_sampled / total_rows * 100
  )]
  
  return(completeness)
}

check_coverage_quality <- function(dt) {
  log_info("Checking coverage quality...")
  
  coverage_stats <- dt[, .(
    mean_coverage = mean(total_vaccinated / total_sampled * 100, na.rm = TRUE),
    median_coverage = median(total_vaccinated / total_sampled * 100, na.rm = TRUE),
    sd_coverage = sd(total_vaccinated / total_sampled * 100, na.rm = TRUE),
    min_coverage = min(total_vaccinated / total_sampled * 100, na.rm = TRUE),
    max_coverage = max(total_vaccinated / total_sampled * 100, na.rm = TRUE),
    below_90 = sum(total_vaccinated / total_sampled < 0.9, na.rm = TRUE),
    below_80 = sum(total_vaccinated / total_sampled < 0.8, na.rm = TRUE),
    below_50 = sum(total_vaccinated / total_sampled < 0.5, na.rm = TRUE)
  )]
  
  return(coverage_stats)
}

check_sample_sizes <- function(dt) {
  log_info("Checking sample sizes...")
  
  sample_stats <- dt[, .(
    total_clusters = .N,
    clusters_below_60 = sum(total_sampled < 60, na.rm = TRUE),
    clusters_60_70 = sum(total_sampled >= 60 & total_sampled <= 70, na.rm = TRUE),
    clusters_70_80 = sum(total_sampled > 70 & total_sampled <= 80, na.rm = TRUE),
    clusters_above_80 = sum(total_sampled > 80, na.rm = TRUE),
    mean_cluster_size = mean(total_sampled, na.rm = TRUE),
    median_cluster_size = median(total_sampled, na.rm = TRUE)
  )]
  
  return(sample_stats)
}

detect_anomalies <- function(dt) {
  log_info("Detecting anomalies...")
  
  # Detect impossible values
  anomalies <- dt[
    total_vaccinated > total_sampled |
    total_missed < 0 |
    total_sampled == 0 |
    total_vaccinated < 0,
    .(country, province, district, response, roundNumber, 
      total_sampled, total_vaccinated, total_missed)
  ]
  
  if (nrow(anomalies) > 0) {
    log_warn("Found {nrow(anomalies)} anomalies")
    qsave(anomalies, here("reports/anomalies.qs"))
  }
  
  return(anomalies)
}

# ============================================================
# Run All QC Checks
# ============================================================

run_qc <- function(data_file) {
  log_info("Loading data for QC: {data_file}")
  dt <- qread(data_file)
  
  results <- list(
    completeness = check_data_completeness(dt),
    coverage = check_coverage_quality(dt),
    sample_sizes = check_sample_sizes(dt),
    anomalies = detect_anomalies(dt)
  )
  
  # Save QC report
  qsave(results, here("reports/qc_results.qs"))
  
  # Generate HTML report
  rmarkdown::render(
    here("R/04_dashboard.Rmd"),
    params = list(qc_results = results),
    output_file = here("reports/dashboards/qc_report.html")
  )
  
  return(results)
}

# ============================================================
# Execute QC
# ============================================================

data_file <- here("data/final/lqas_final.qs")
qc_results <- run_qc(data_file)

log_info("🎉 Quality Control Complete!")
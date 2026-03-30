#!/usr/bin/env Rscript
# Health Check Monitor for LQAS System

library(data.table)
library(logger)

log_appender(appender_file("logs/health_check.log"))

# Check data freshness
dt <- fread("data/final/lqas_cleaned.csv")
latest_date <- max(as.Date(dt$lqas_start_date), na.rm = TRUE)
days_since_update <- as.numeric(Sys.Date() - latest_date)

log_info("Health Check - {Sys.time()}")
log_info("Latest data: {latest_date} ({days_since_update} days ago)")

# Check file sizes
files <- c(
  "data/final/lqas_cleaned.csv",
  "data/final/lqas_cleaned.parquet",
  "04_dashboard.html"
)

for (f in files) {
  if (file.exists(f)) {
    size_mb <- file.size(f) / (1024 * 1024)
    log_info("{f}: {round(size_mb, 2)} MB")
  } else {
    log_warn("{f}: MISSING")
  }
}

# Check data quality
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
coverage <- mean(dt$total_vaccinated / dt$total_sampled * 100, na.rm = TRUE)

log_info("Records: {total_records}")
log_info("Countries: {total_countries}")
log_info("Avg Coverage: {round(coverage, 1)}%")

# Alert if issues detected
if (days_since_update > 7) {
  log_warn("Data is older than 7 days - fetch new data!")
}
if (coverage < 50) {
  log_warn("Average coverage below 50% - investigate!")
}

log_info("Health check complete")

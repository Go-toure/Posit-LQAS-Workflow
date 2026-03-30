#!/usr/bin/env Rscript
# Production LQAS Pipeline - Optimized for Monday Weekly Runs

library(logger)
library(jsonlite)

# Setup logging
log_dir <- "logs"
dir_create(log_dir)
log_appender(appender_file(file.path(log_dir, "production.log")))

log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("PRODUCTION LQAS PIPELINE STARTING (WEEKLY MONDAY RUN)")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Record start time
start_time <- Sys.time()
log_info("Start time: {start_time}")
log_info("Day of week: {format(start_time, '%A')}")

# Check if it's Monday (for validation)
current_day <- format(start_time, "%A")
if (current_day != "Monday") {
  log_warn("Running on {current_day} instead of Monday - manual execution detected")
}

# Function to run command with logging
run_command <- function(cmd, name) {
  log_info("Running: {name}")
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  log_info("Completed: {name}")
  return(result)
}

# Step 1: Fetch new data
log_info("Step 1: Fetching fresh data from ONA...")
fetch_result <- run_command("python fetch_ona_data.py --force-full", "Data Fetch (Full Refresh)")

# Step 2: Process data
log_info("Step 2: Processing LQAS data...")
process_result <- run_command("Rscript R/02_process_lqas.R", "Data Processing")

# Step 3: Clean geonames
log_info("Step 3: Cleaning geonames...")
clean_result <- run_command("Rscript R/03_clean_geonames.R", "Geoname Cleaning")

# Step 4: Generate executive dashboard
log_info("Step 4: Generating executive dashboard...")
dashboard_result <- run_command("Rscript executive_dashboard.R", "Dashboard Generation")

# Calculate duration
end_time <- Sys.time()
duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 1)

# Load final data for summary
dt <- fread("data/final/lqas_cleaned.csv")
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
coverage <- round(total_vaccinated / total_sampled * 100, 1)

# Create summary report
summary <- list(
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  day_of_week = current_day,
  duration_minutes = duration,
  status = "SUCCESS",
  data_summary = list(
    total_records = total_records,
    total_countries = total_countries,
    total_districts = total_districts,
    total_sampled = total_sampled,
    total_vaccinated = total_vaccinated,
    overall_coverage = coverage
  ),
  steps = list(
    fetch = "COMPLETED",
    process = "COMPLETED",
    clean = "COMPLETED",
    dashboard = "COMPLETED"
  )
)

# Save summary
summary_file <- file.path("reports", paste0("pipeline_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
write_json(summary, summary_file, pretty = TRUE)

log_info("Pipeline completed in {duration} minutes")
log_info("Data summary: {total_records} records, {total_countries} countries, {coverage}% coverage")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Open dashboard
system("start 04_dashboard.html", wait = FALSE)

quit(status = 0)

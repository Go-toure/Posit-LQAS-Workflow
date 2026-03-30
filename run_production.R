#!/usr/bin/env Rscript
# Production LQAS Pipeline with Notifications and Logging

library(logger)
library(jsonlite)

# Setup logging
log_dir <- "logs"
dir_create(log_dir)
log_appender(appender_file(file.path(log_dir, "production.log")))

log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("PRODUCTION LQAS PIPELINE STARTING")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Record start time
start_time <- Sys.time()
log_info("Start time: {start_time}")

# Function to run command with logging
run_command <- function(cmd, name) {
  log_info("Running: {name}")
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  log_info("Completed: {name}")
  return(result)
}

# Step 1: Fetch new data
log_info("Step 1: Fetching data from ONA...")
fetch_result <- run_command("python fetch_ona_data.py", "Data Fetch")

# Step 2: Process data
log_info("Step 2: Processing LQAS data...")
process_result <- run_command("Rscript R/02_process_lqas.R", "Data Processing")

# Step 3: Clean geonames
log_info("Step 3: Cleaning geonames...")
clean_result <- run_command("Rscript R/03_clean_geonames.R", "Geoname Cleaning")

# Step 4: Generate dashboard
log_info("Step 4: Generating dashboard...")
dashboard_result <- run_command("Rscript executive_dashboard.R", "Dashboard Generation")

# Calculate duration
end_time <- Sys.time()
duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 1)

# Create summary report
summary <- list(
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  duration_minutes = duration,
  status = "SUCCESS",
  steps = list(
    fetch = "COMPLETED",
    process = "COMPLETED",
    clean = "COMPLETED",
    dashboard = "COMPLETED"
  )
)

write_json(summary, file.path("reports", paste0("pipeline_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")), pretty = TRUE)

log_info("Pipeline completed in {duration} minutes")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Open dashboard
system("start 04_dashboard.html", wait = FALSE)

quit(status = 0)

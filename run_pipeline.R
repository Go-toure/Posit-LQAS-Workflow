#!/usr/bin/env Rscript
# ============================================================
# LQAS Pipeline Orchestrator - Fully Automated
# Runs: fetch → process → clean → dashboard → open
# ============================================================

# Configuration
FORCE_FULL <- "--force-full" %in% commandArgs(trailingOnly = TRUE)
SKIP_FETCH <- "--skip-fetch" %in% commandArgs(trailingOnly = TRUE)
AUTO_OPEN <- !"--no-open" %in% commandArgs(trailingOnly = TRUE)

# Function to run command and check status
run_step <- function(step_name, command) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP:", step_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  result <- system(command, intern = FALSE)
  
  if (result != 0) {
    cat("❌ ERROR:", step_name, "failed!\n")
    return(FALSE)
  }
  cat("✅", step_name, "completed successfully!\n")
  return(TRUE)
}

# Main pipeline
cat("\n")
cat("🚀 LQAS PIPELINE EXECUTION\n")
cat("============================================================\n")
cat("Started at:", Sys.time(), "\n")
cat("Force full run:", FORCE_FULL, "\n")
cat("Skip fetch:", SKIP_FETCH, "\n")
cat("Auto-open dashboard:", AUTO_OPEN, "\n")
cat("============================================================\n")

# Step 1: Fetch data from ONA
if (!SKIP_FETCH) {
  fetch_cmd <- paste("python fetch_ona_data.py", if(FORCE_FULL) "--force-full")
  if (!run_step("Data Fetch", fetch_cmd)) quit(status = 1)
} else {
  cat("\n⏭️ Skipping data fetch (--skip-fetch)\n")
}

# Step 2: Process LQAS data
if (!run_step("Data Processing", "Rscript R/02_process_lqas.R")) quit(status = 1)

# Step 3: Clean geonames
if (!run_step("Geoname Cleaning", "Rscript R/03_clean_geonames.R")) quit(status = 1)

# Step 4: Generate dashboard
if (!run_step("Dashboard Generation", "Rscript R/dashboard_simple.R")) quit(status = 1)

# Step 5: Open dashboard (if auto-open enabled)
if (AUTO_OPEN && file.exists("04_dashboard.html")) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📊 OPENING DASHBOARD\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Try different methods to open the dashboard
  if (Sys.info()["sysname"] == "Windows") {
    system("start 04_dashboard.html", wait = FALSE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system("open 04_dashboard.html", wait = FALSE)
  } else {
    system("xdg-open 04_dashboard.html", wait = FALSE)
  }
  cat("✅ Dashboard opened in your browser\n")
}

# Summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 PIPELINE COMPLETED SUCCESSFULLY!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Completed at:", Sys.time(), "\n")
cat("\n")
cat("📊 Dashboard location: 04_dashboard.html\n")
cat("📁 Data files: data/final/\n")
cat("📝 Logs: logs/\n")
cat("\n")

quit(status = 0)

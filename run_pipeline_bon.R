#!/usr/bin/env Rscript
# ============================================================
# LQAS Pipeline Orchestrator - Enhanced Version
# Runs: fetch → process → clean → dashboard → qc → open
# Skip flags available:
#   --skip-fetch, --skip-process, --skip-clean, --skip-dashboard, --skip-qc
# Dashboard ALWAYS opens after generation (if it exists)
# ============================================================

# Configuration
FORCE_FULL <- "--force-full" %in% commandArgs(trailingOnly = TRUE)
SKIP_FETCH <- "--skip-fetch" %in% commandArgs(trailingOnly = TRUE)
SKIP_PROCESS <- "--skip-process" %in% commandArgs(trailingOnly = TRUE)
SKIP_CLEAN <- "--skip-clean" %in% commandArgs(trailingOnly = TRUE)
SKIP_DASHBOARD <- "--skip-dashboard" %in% commandArgs(trailingOnly = TRUE)
SKIP_QC <- "--skip-qc" %in% commandArgs(trailingOnly = TRUE)

# Function to run R script and wait for completion
run_r_script <- function(script_path, script_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP:", script_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Run the script and capture output
  result <- system2("Rscript", args = c(script_path), stdout = TRUE, stderr = TRUE)
  
  # Print output
  cat(paste(result, collapse = "\n"), "\n")
  
  # Check if it succeeded (look for error messages)
  if (any(grepl("Error", result, ignore.case = TRUE))) {
    cat("❌ ERROR:", script_name, "failed!\n")
    return(FALSE)
  }
  
  cat("✅", script_name, "completed successfully!\n")
  return(TRUE)
}

# Function to run Python script
run_python_script <- function(script_path, script_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP:", script_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cmd <- paste("python", script_path, if(FORCE_FULL) "--force-full")
  result <- system(cmd, intern = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
  
  cat("✅", script_name, "completed successfully!\n")
  return(TRUE)
}

# Function to open dashboard
open_dashboard <- function() {
  if (file.exists("04_dashboard.html")) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("📊 OPENING DASHBOARD\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    
    if (Sys.info()["sysname"] == "Windows") {
      system("cmd.exe /c start 04_dashboard.html", wait = FALSE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      system("open 04_dashboard.html", wait = FALSE)
    } else {
      system("xdg-open 04_dashboard.html", wait = FALSE)
    }
    cat("✅ Dashboard opened in your browser\n")
    return(TRUE)
  } else {
    cat("\n⚠️ Dashboard file not found: 04_dashboard.html\n")
    return(FALSE)
  }
}

# Main pipeline
cat("\n")
cat("🚀 LQAS PIPELINE EXECUTION\n")
cat("============================================================\n")
cat("Started at:", Sys.time(), "\n")
cat("Force full run:", FORCE_FULL, "\n")
cat("Skip fetch:", SKIP_FETCH, "\n")
cat("Skip process:", SKIP_PROCESS, "\n")
cat("Skip clean:", SKIP_CLEAN, "\n")
cat("Skip dashboard:", SKIP_DASHBOARD, "\n")
cat("Skip QC:", SKIP_QC, "\n")
cat("============================================================\n")

# Step 1: Fetch data from ONA
if (!SKIP_FETCH) {
  if (!run_python_script("fetch_ona_data.py", "Data Fetch")) quit(status = 1)
} else {
  cat("\n⏭️ Skipping data fetch (--skip-fetch)\n")
}

# Step 2: Process LQAS data
if (!SKIP_PROCESS) {
  if (!run_r_script("R/02_process_lqas.R", "Data Processing")) quit(status = 1)
} else {
  cat("\n⏭️ Skipping data processing (--skip-process)\n")
}

# Step 3: Clean geonames
if (!SKIP_CLEAN) {
  if (!run_r_script("R/03_clean_geonames.R", "Geoname Cleaning")) {
    cat("⚠️ Geoname cleaning had issues, but continuing...\n")
  }
} else {
  cat("\n⏭️ Skipping geoname cleaning (--skip-clean)\n")
}

# Step 4: Generate dashboard
dashboard_exists_before <- file.exists("04_dashboard.html")

if (!SKIP_DASHBOARD) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP: Dashboard Generation\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  dashboard_result <- system2("Rscript",
                              args = c("-e", "\"rmarkdown::render('R/04_dashboard.Rmd', output_file = '04_dashboard.html')\""),
                              stdout = TRUE, stderr = TRUE)
  cat(paste(dashboard_result, collapse = "\n"), "\n")
  
  if (file.exists("04_dashboard.html")) {
    cat("✅ Dashboard Generation completed successfully!\n")
  } else {
    cat("❌ Dashboard Generation failed!\n")
  }
} else {
  cat("\n⏭️ Skipping dashboard generation (--skip-dashboard)\n")
}

# Step 5: Run quality control
if (!SKIP_QC) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP: Quality Control\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  qc_result <- system2("Rscript", args = c("R/05_quality_control.R"),
                       stdout = TRUE, stderr = TRUE)
  cat(paste(qc_result, collapse = "\n"), "\n")
  cat("✅ Quality Control completed!\n")
} else {
  cat("\n⏭️ Skipping quality control (--skip-qc)\n")
}

# Step 6: ALWAYS open dashboard (if it exists)
open_dashboard()

# Summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 PIPELINE COMPLETED SUCCESSFULLY!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Completed at:", Sys.time(), "\n")
cat("\n")
if (file.exists("04_dashboard.html")) {
  cat("📊 Dashboard location: 04_dashboard.html\n")
}
if (!SKIP_CLEAN) {
  cat("📁 Cleaned data: data/final/lqas_cleaned.parquet\n")
}
cat("📝 Logs: logs/\n")
cat("\n")

quit(status = 0)
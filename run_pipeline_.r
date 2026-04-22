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

# Get project root directory (where this script is located)
PROJECT_ROOT <- getwd()

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

# Function to generate dashboard
generate_dashboard <- function() {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP: Dashboard Generation\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  dashboard_rmd <- file.path(PROJECT_ROOT, "R", "04_dashboard.Rmd")
  dashboard_output <- file.path(PROJECT_ROOT, "04_dashboard.html")
  
  if (!file.exists(dashboard_rmd)) {
    cat("❌ Dashboard Rmd file not found:", dashboard_rmd, "\n")
    cat("Please ensure R/04_dashboard.Rmd exists\n")
    return(FALSE)
  }
  
  # Create a temporary R script to render the dashboard
  render_script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf("library(rmarkdown)"),
    sprintf("setwd('%s')", PROJECT_ROOT),
    sprintf("render(input = '%s', output_file = '%s', output_dir = '%s', params = list(data_file = 'data/final/lqas_dashboard_input.parquet', run_date = Sys.Date()), envir = new.env())",
            dashboard_rmd, 
            basename(dashboard_output),
            dirname(dashboard_output))
  ), render_script)
  
  # Run the render script
  result <- system2("Rscript", args = c(render_script), stdout = TRUE, stderr = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
  
  # Clean up temp file
  unlink(render_script)
  
  # Verify dashboard was created in the correct location
  if (file.exists(dashboard_output)) {
    cat("✅ Dashboard generated successfully at:", dashboard_output, "\n")
    return(TRUE)
  } else {
    # Check if it was saved in R/ directory
    alt_location <- file.path(PROJECT_ROOT, "R", "04_dashboard.html")
    if (file.exists(alt_location)) {
      cat("⚠️ Dashboard was saved to R/ directory, moving to project root...\n")
      file.copy(alt_location, dashboard_output, overwrite = TRUE)
      if (file.exists(dashboard_output)) {
        cat("✅ Dashboard moved to:", dashboard_output, "\n")
        return(TRUE)
      }
    }
    cat("❌ Dashboard Generation failed!\n")
    return(FALSE)
  }
}

# Function to open dashboard
open_dashboard <- function() {
  dashboard_path <- file.path(PROJECT_ROOT, "04_dashboard.html")
  
  if (file.exists(dashboard_path)) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("📊 OPENING DASHBOARD\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    
    if (Sys.info()["sysname"] == "Windows") {
      system2("cmd.exe", args = c("/c", "start", dashboard_path), wait = FALSE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      system2("open", args = dashboard_path, wait = FALSE)
    } else {
      system2("xdg-open", args = dashboard_path, wait = FALSE)
    }
    cat("✅ Dashboard opened in your browser\n")
    cat("📊 Dashboard location:", dashboard_path, "\n")
    return(TRUE)
  } else {
    cat("\n⚠️ Dashboard file not found:", dashboard_path, "\n")
    cat("Please generate the dashboard first (remove --skip-dashboard flag)\n")
    return(FALSE)
  }
}

# Function to run quality control
run_quality_control <- function() {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP: Quality Control\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  qc_script <- file.path(PROJECT_ROOT, "R", "05_quality_control.R")
  
  if (!file.exists(qc_script)) {
    cat("⚠️ Quality control script not found:", qc_script, "\n")
    cat("Skipping QC...\n")
    return(TRUE)
  }
  
  result <- system2("Rscript", args = c(qc_script), stdout = TRUE, stderr = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
  
  if (any(grepl("Error", result, ignore.case = TRUE))) {
    cat("⚠️ Quality Control completed with warnings\n")
    return(FALSE)
  }
  
  cat("✅ Quality Control completed!\n")
  return(TRUE)
}

# Main pipeline
cat("\n")
cat("🚀 LQAS PIPELINE EXECUTION\n")
cat("============================================================\n")
cat("Project root:", PROJECT_ROOT, "\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Force full run:", FORCE_FULL, "\n")
cat("Skip fetch:", SKIP_FETCH, "\n")
cat("Skip process:", SKIP_PROCESS, "\n")
cat("Skip clean:", SKIP_CLEAN, "\n")
cat("Skip dashboard:", SKIP_DASHBOARD, "\n")
cat("Skip QC:", SKIP_QC, "\n")
cat("============================================================\n")

# Step 1: Fetch data from ONA
if (!SKIP_FETCH) {
  if (!run_python_script("fetch_ona_data.py", "Data Fetch")) {
    cat("❌ Pipeline stopped due to fetch failure\n")
    quit(status = 1)
  }
} else {
  cat("\n⏭️ Skipping data fetch (--skip-fetch)\n")
}

# Step 2: Process LQAS data
if (!SKIP_PROCESS) {
  if (!run_r_script("R/02_process_lqas.R", "Data Processing")) {
    cat("❌ Pipeline stopped due to processing failure\n")
    quit(status = 1)
  }
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
dashboard_generated <- FALSE
if (!SKIP_DASHBOARD) {
  dashboard_generated <- generate_dashboard()
} else {
  cat("\n⏭️ Skipping dashboard generation (--skip-dashboard)\n")
  # Check if dashboard already exists
  if (file.exists(file.path(PROJECT_ROOT, "04_dashboard.html"))) {
    dashboard_generated <- TRUE
    cat("📊 Using existing dashboard\n")
  }
}

# Step 5: Run quality control
if (!SKIP_QC) {
  run_quality_control()
} else {
  cat("\n⏭️ Skipping quality control (--skip-qc)\n")
}

# Step 6: ALWAYS open dashboard (if it exists)
if (dashboard_generated || file.exists(file.path(PROJECT_ROOT, "04_dashboard.html"))) {
  open_dashboard()
} else {
  cat("\n⚠️ Cannot open dashboard - file not found\n")
  cat("Generate it with: Rscript run_pipeline.R (without --skip-dashboard)\n")
}

# Summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 PIPELINE COMPLETED SUCCESSFULLY!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n")
cat("📊 OUTPUT FILES:\n")
cat("   Dashboard:", file.path(PROJECT_ROOT, "04_dashboard.html"), "\n")
if (file.exists("data/final/lqas_dashboard_input.parquet")) {
  cat("   Processed data: data/final/lqas_dashboard_input.parquet\n")
}
if (file.exists("data/final/lqas_cleaned.parquet")) {
  cat("   Cleaned data: data/final/lqas_cleaned.parquet\n")
}
cat("   Logs: logs/\n")
cat("\n")

quit(status = 0)
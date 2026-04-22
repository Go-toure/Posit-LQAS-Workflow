#!/usr/bin/env Rscript
# ============================================================
# LQAS Pipeline Orchestrator - Enhanced Version with Progress Bars
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

# Function to create progress bar
create_progress_bar <- function(total, width = 50, char = "=") {
  list(
    current = 0,
    total = total,
    width = width,
    char = char,
    start_time = Sys.time()
  )
}

# Function to update progress bar
update_progress <- function(pb, step_name = "") {
  pb$current <- pb$current + 1
  percent <- pb$current / pb$total
  filled <- round(pb$width * percent)
  empty <- pb$width - filled
  bar <- paste0("[", paste(rep(pb$char, filled), collapse = ""),
                paste(rep(" ", empty), collapse = ""), "]")
  
  # Calculate elapsed time
  elapsed <- as.numeric(difftime(Sys.time(), pb$start_time, units = "secs"))
  eta <- if (pb$current > 0) (elapsed / pb$current) * (pb$total - pb$current) else 0
  
  cat(sprintf("\r  %s %3d%% (%d/%d) - %s - Elapsed: %.1fs, ETA: %.1fs     ",
              bar, round(percent * 100), pb$current, pb$total,
              step_name, elapsed, eta))
  flush.console()
  
  if (pb$current == pb$total) {
    cat("\n")
  }
}

# Function to run R script with progress tracking
run_r_script <- function(script_path, script_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP:", script_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Create progress bar for script execution
  pb <- create_progress_bar(total = 3)
  update_progress(pb, "Starting script execution")
  
  # Run the script and capture output
  update_progress(pb, "Running script...")
  result <- system2("Rscript", args = c(script_path), stdout = TRUE, stderr = TRUE)
  
  update_progress(pb, "Processing output")
  
  # Print output (but filter out excessive lines)
  cat("\n")
  if (length(result) > 50) {
    cat(paste(head(result, 25), collapse = "\n"), "\n")
    cat("... (", length(result) - 50, " lines omitted) ...\n")
    cat(paste(tail(result, 25), collapse = "\n"), "\n")
  } else {
    cat(paste(result, collapse = "\n"), "\n")
  }
  
  update_progress(pb, "Finalizing")
  
  # Check if it succeeded (look for error messages)
  if (any(grepl("Error", result, ignore.case = TRUE) & 
          !grepl("Loading required package", result, ignore.case = TRUE))) {
    cat("\n❌ ERROR:", script_name, "failed!\n")
    return(FALSE)
  }
  
  cat("\n✅", script_name, "completed successfully!\n")
  return(TRUE)
}

# Function to run Python script with progress tracking
run_python_script <- function(script_path, script_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📌 STEP:", script_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Create progress bar
  pb <- create_progress_bar(total = 2)
  update_progress(pb, "Initializing Python")
  
  cmd <- paste("python", script_path, if(FORCE_FULL) "--force-full")
  update_progress(pb, "Executing Python script")
  result <- system(cmd, intern = TRUE)
  
  # Print output
  cat("\n")
  if (length(result) > 50) {
    cat(paste(head(result, 25), collapse = "\n"), "\n")
    cat("... (", length(result) - 50, " lines omitted) ...\n")
    cat(paste(tail(result, 25), collapse = "\n"), "\n")
  } else {
    cat(paste(result, collapse = "\n"), "\n")
  }
  
  cat("\n✅", script_name, "completed successfully!\n")
  return(TRUE)
}

# Function to generate dashboard with progress tracking
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
  
  # Progress tracking
  pb <- create_progress_bar(total = 5)
  update_progress(pb, "Checking prerequisites")
  
  # Check if data file exists
  data_file <- file.path(PROJECT_ROOT, "data/final/lqas_dashboard_input.parquet")
  if (!file.exists(data_file)) {
    cat("\n⚠️ Warning: Data file not found:", data_file, "\n")
    cat("Dashboard may not display correctly\n")
  }
  update_progress(pb, "Data file verified")
  
  # Create a temporary R script to render the dashboard
  update_progress(pb, "Creating render script")
  render_script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf("library(rmarkdown)"),
    sprintf("setwd('%s')", PROJECT_ROOT),
    sprintf("cat('Rendering dashboard...\\n')"),
    sprintf("render(input = '%s', output_file = '%s', output_dir = '%s', params = list(data_file = 'data/final/lqas_dashboard_input.parquet', run_date = Sys.Date()), envir = new.env(), quiet = FALSE)",
            dashboard_rmd, 
            basename(dashboard_output),
            dirname(dashboard_output))
  ), render_script)
  
  update_progress(pb, "Executing render")
  # Run the render script
  result <- system2("Rscript", args = c(render_script), stdout = TRUE, stderr = TRUE)
  cat("\n")
  if (length(result) > 0) {
    cat(paste(result[!grepl("Loading required package", result)], collapse = "\n"), "\n")
  }
  
  update_progress(pb, "Verifying output")
  # Clean up temp file
  unlink(render_script)
  
  # Verify dashboard was created in the correct location
  if (file.exists(dashboard_output)) {
    cat("\n✅ Dashboard generated successfully at:", dashboard_output, "\n")
    return(TRUE)
  } else {
    # Check if it was saved in R/ directory
    alt_location <- file.path(PROJECT_ROOT, "R", "04_dashboard.html")
    if (file.exists(alt_location)) {
      cat("\n⚠️ Dashboard was saved to R/ directory, moving to project root...\n")
      file.copy(alt_location, dashboard_output, overwrite = TRUE)
      if (file.exists(dashboard_output)) {
        cat("✅ Dashboard moved to:", dashboard_output, "\n")
        return(TRUE)
      }
    }
    cat("\n❌ Dashboard Generation failed!\n")
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
    
    # Create progress for opening
    pb <- create_progress_bar(total = 2)
    update_progress(pb, "Launching browser")
    
    if (Sys.info()["sysname"] == "Windows") {
      system2("cmd.exe", args = c("/c", "start", dashboard_path), wait = FALSE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      system2("open", args = dashboard_path, wait = FALSE)
    } else {
      system2("xdg-open", args = dashboard_path, wait = FALSE)
    }
    
    update_progress(pb, "Dashboard opened")
    cat("\n✅ Dashboard opened in your browser\n")
    cat("📊 Dashboard location:", dashboard_path, "\n")
    return(TRUE)
  } else {
    cat("\n⚠️ Dashboard file not found:", dashboard_path, "\n")
    cat("Please generate the dashboard first (remove --skip-dashboard flag)\n")
    return(FALSE)
  }
}

# Function to run quality control with progress
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
  
  # Progress tracking
  pb <- create_progress_bar(total = 3)
  update_progress(pb, "Starting QC")
  
  result <- system2("Rscript", args = c(qc_script), stdout = TRUE, stderr = TRUE)
  
  update_progress(pb, "Analyzing results")
  
  cat("\n")
  if (length(result) > 50) {
    cat(paste(head(result, 25), collapse = "\n"), "\n")
    cat("... (", length(result) - 50, " lines omitted) ...\n")
    cat(paste(tail(result, 25), collapse = "\n"), "\n")
  } else {
    cat(paste(result, collapse = "\n"), "\n")
  }
  
  update_progress(pb, "Finalizing QC")
  
  if (any(grepl("Error", result, ignore.case = TRUE))) {
    cat("\n⚠️ Quality Control completed with warnings\n")
    return(FALSE)
  }
  
  cat("\n✅ Quality Control completed!\n")
  return(TRUE)
}

# Main pipeline with overall progress
run_pipeline <- function() {
  # Define pipeline steps
  steps <- list()
  step_num <- 1
  
  if (!SKIP_FETCH) {
    steps[[step_num]] <- list(name = "Data Fetch", type = "python", script = "fetch_ona_data.py")
    step_num <- step_num + 1
  }
  
  if (!SKIP_PROCESS) {
    steps[[step_num]] <- list(name = "Data Processing", type = "r", script = "R/02_process_lqas.R")
    step_num <- step_num + 1
  }
  
  if (!SKIP_CLEAN) {
    steps[[step_num]] <- list(name = "Geoname Cleaning", type = "r", script = "R/03_clean_geonames.R")
    step_num <- step_num + 1
  }
  
  if (!SKIP_DASHBOARD) {
    steps[[step_num]] <- list(name = "Dashboard Generation", type = "dashboard", script = NA)
    step_num <- step_num + 1
  }
  
  if (!SKIP_QC) {
    steps[[step_num]] <- list(name = "Quality Control", type = "qc", script = NA)
    step_num <- step_num + 1
  }
  
  # Overall pipeline progress
  cat("\n")
  cat("🚀 LQAS PIPELINE EXECUTION\n")
  cat("============================================================\n")
  cat("Project root:", PROJECT_ROOT, "\n")
  cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total steps:", length(steps), "\n")
  cat("Force full run:", FORCE_FULL, "\n")
  cat("Skip fetch:", SKIP_FETCH, "\n")
  cat("Skip process:", SKIP_PROCESS, "\n")
  cat("Skip clean:", SKIP_CLEAN, "\n")
  cat("Skip dashboard:", SKIP_DASHBOARD, "\n")
  cat("Skip QC:", SKIP_QC, "\n")
  cat("============================================================\n")
  
  # Create overall progress bar
  overall_pb <- create_progress_bar(total = length(steps), width = 60, char = "█")
  
  # Execute each step
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    update_progress(overall_pb, sprintf("Step %d/%d: %s", i, length(steps), step$name))
    
    result <- FALSE
    
    if (step$type == "python") {
      result <- run_python_script(step$script, step$name)
    } else if (step$type == "r") {
      result <- run_r_script(step$script, step$name)
    } else if (step$type == "dashboard") {
      result <- generate_dashboard()
    } else if (step$type == "qc") {
      result <- run_quality_control()
    }
    
    if (!result && step$type != "qc" && step$type != "dashboard") {
      cat("\n❌ Pipeline stopped at step:", step$name, "\n")
      return(FALSE)
    }
    
    # Small delay to show progress
    Sys.sleep(0.5)
  }
  
  # Final update
  update_progress(overall_pb, "Pipeline complete!")
  
  return(TRUE)
}

# Execute pipeline
pipeline_success <- run_pipeline()

# Open dashboard if pipeline succeeded or dashboard exists
if (pipeline_success || file.exists(file.path(PROJECT_ROOT, "04_dashboard.html"))) {
  open_dashboard()
} else {
  cat("\n⚠️ Cannot open dashboard - pipeline failed and no existing dashboard found\n")
}

# Final summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
if (pipeline_success) {
  cat("🎉 PIPELINE COMPLETED SUCCESSFULLY!\n")
} else {
  cat("⚠️ PIPELINE COMPLETED WITH ISSUES\n")
}
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n")
cat("📊 OUTPUT FILES:\n")
if (file.exists(file.path(PROJECT_ROOT, "04_dashboard.html"))) {
  cat("   ✅ Dashboard:", file.path(PROJECT_ROOT, "04_dashboard.html"), "\n")
} else {
  cat("   ❌ Dashboard: Not generated\n")
}
if (file.exists("data/final/lqas_dashboard_input.parquet")) {
  file_size <- round(file.size("data/final/lqas_dashboard_input.parquet") / (1024 * 1024), 2)
  cat("   ✅ Processed data: data/final/lqas_dashboard_input.parquet (", file_size, " MB)\n")
}
if (file.exists("data/final/lqas_cleaned.parquet")) {
  file_size <- round(file.size("data/final/lqas_cleaned.parquet") / (1024 * 1024), 2)
  cat("   ✅ Cleaned data: data/final/lqas_cleaned.parquet (", file_size, " MB)\n")
}
cat("   📝 Logs: logs/\n")
cat("\n")

# Return exit code
quit(status = ifelse(pipeline_success, 0, 1))
#!/usr/bin/env Rscript

# ============================================================
# DIAGNOSTIC SCRIPT: Inspect CHAD (4431) and GUINEA (5299) raw files
# ============================================================

library(arrow)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

# Path to raw data directory
raw_dir <- "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/data/raw"

# Focus on specific files
chad_file <- file.path(raw_dir, "4431.parquet")
guinea_file <- file.path(raw_dir, "5299.parquet")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("DIAGNOSTIC: Inspecting CHAD (4431) and GUINEA (5299) files\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Function to inspect a file in detail
inspect_file_detail <- function(file_path, file_id) {
  cat("\n", paste(rep("#", 80), collapse = ""), "\n")
  cat("FILE:", basename(file_path), "(", file_id, ")\n")
  cat(paste(rep("#", 80), collapse = ""), "\n")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("ERROR: File not found!\n")
    cat("Path:", file_path, "\n")
    return(NULL)
  }
  
  # Read the file
  data <- tryCatch({
    as.data.table(read_parquet(file_path))
  }, error = function(e) {
    cat("ERROR reading file:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    cat("No data or empty file\n")
    return(NULL)
  }
  
  cat("\n--- BASIC INFO ---\n")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", ncol(data), "\n")
  
  # Show first 20 column names to understand structure
  cat("\n--- FIRST 20 COLUMN NAMES ---\n")
  cat(paste(head(names(data), 20), collapse = "\n"), "\n")
  if (ncol(data) > 20) cat("... and", ncol(data) - 20, "more columns\n")
  
  # Look for vaccination-related columns (case insensitive)
  cat("\n--- VACCINATION COLUMNS (FM_Child patterns) ---\n")
  fm_patterns <- c("FM_Child", "FM_CHILD", "fm_child", "Vaccinated", "VACC", "U5_Vac", "Child_Vacc")
  fm_cols <- c()
  for (pattern in fm_patterns) {
    cols <- names(data)[grepl(pattern, names(data), ignore.case = TRUE)]
    fm_cols <- c(fm_cols, cols)
  }
  fm_cols <- unique(fm_cols)
  
  if (length(fm_cols) > 0) {
    for (col in fm_cols[1:min(10, length(fm_cols))]) {
      cat("\n  Column:", col, "\n")
      # Get unique non-NA values
      unique_vals <- unique(data[[col]][!is.na(data[[col]])])
      cat("    Type:", class(data[[col]]), "\n")
      cat("    Unique values:", paste(head(unique_vals, 15), collapse = ", "), "\n")
      if (length(unique_vals) > 15) cat("    ... and", length(unique_vals) - 15, "more\n")
      
      # Count distribution
      if (length(unique_vals) <= 10) {
        cat("    Value counts:\n")
        val_counts <- table(data[[col]], useNA = "ifany")
        print(val_counts)
      }
    }
  } else {
    cat("  No FM_Child related columns found!\n")
    cat("  Searching for any column with 'vacc' or 'child' in name...\n")
    alt_cols <- names(data)[grepl("vacc|child|immun", names(data), ignore.case = TRUE)]
    if (length(alt_cols) > 0) {
      cat("  Found these alternative columns:\n")
      for (col in alt_cols[1:min(10, length(alt_cols))]) {
        cat("    -", col, "\n")
      }
    }
  }
  
  # Look for sex columns
  cat("\n--- SEX COLUMNS ---\n")
  sex_cols <- names(data)[grepl("Sex|sex|SEX|Gender|gender", names(data))]
  if (length(sex_cols) > 0) {
    for (col in sex_cols[1:min(5, length(sex_cols))]) {
      cat("\n  Column:", col, "\n")
      unique_vals <- unique(data[[col]][!is.na(data[[col]])])
      cat("    Unique values:", paste(head(unique_vals, 10), collapse = ", "), "\n")
    }
  } else {
    cat("  No sex columns found\n")
  }
  
  # Check for JSON or nested structure in Count_HH
  cat("\n--- CHECKING FOR NESTED STRUCTURE ---\n")
  if ("Count_HH" %in% names(data)) {
    cat("Count_HH column exists\n")
    sample_val <- as.character(data$Count_HH[1])
    cat("Sample value (first row):", substr(sample_val, 1, 200), "\n")
    is_json <- grepl("\\[|\\{|'", sample_val)
    cat("Looks like JSON?", is_json, "\n")
  } else {
    cat("Count_HH column NOT found\n")
    # Look for any column that might contain nested data
    list_cols <- sapply(data, function(x) is.list(x) || grepl("\\[|\\{", as.character(x[1])))
    if (any(list_cols)) {
      cat("Found potential list/nested columns:", paste(names(data)[list_cols], collapse = ", "), "\n")
    }
  }
  
  # Check for HH bracket pattern
  cat("\n--- CHECKING FOR HH BRACKET PATTERN ---\n")
  hh_pattern <- grepl("^HH\\[", names(data))
  if (any(hh_pattern)) {
    cat("Found HH bracket columns:", paste(head(names(data)[hh_pattern], 5), collapse = ", "), "\n")
  } else {
    cat("No HH bracket pattern found\n")
  }
  
  # Sample actual data rows (first 3 rows of key columns)
  cat("\n--- SAMPLE DATA (First 3 rows, key columns) ---\n")
  key_cols <- c("Country", "Region", "District", "Response", "roundNumber", "Date_of_LQAS")
  key_cols <- key_cols[key_cols %in% names(data)]
  
  if (length(key_cols) > 0) {
    sample_data <- head(data[, ..key_cols], 3)
    print(sample_data)
  }
  
  # If we found FM_Child columns, show a cross-tab with Response
  if (length(fm_cols) > 0 && "Response" %in% names(data)) {
    cat("\n--- VACCINATION STATUS BY RESPONSE (first 10 responses) ---\n")
    # Take first FM_Child column
    first_fm <- fm_cols[1]
    summary_data <- data %>%
      group_by(Response) %>%
      summarise(
        n = n(),
        vaccinated_pct = mean(as.numeric(as.character(get(first_fm))) %in% c(1, "1", "Yes", "YES", "yes"), na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      head(10)
    print(summary_data)
  }
  
  return(invisible(TRUE))
}

# Inspect CHAD file (4431)
if (file.exists(chad_file)) {
  inspect_file_detail(chad_file, "CHAD")
} else {
  cat("\nCHAD file not found at:", chad_file, "\n")
  # Try to find any file with 4431 in name
  raw_files <- list.files(raw_dir, pattern = "4431", full.names = TRUE)
  if (length(raw_files) > 0) {
    cat("Found alternative:", raw_files[1], "\n")
    inspect_file_detail(raw_files[1], "CHAD")
  } else {
    cat("No file containing '4431' found in directory\n")
  }
}

# Inspect GUINEA file (5299)
if (file.exists(guinea_file)) {
  inspect_file_detail(guinea_file, "GUINEA")
} else {
  cat("\nGUINEA file not found at:", guinea_file, "\n")
  # Try to find any file with 5299 in name
  raw_files <- list.files(raw_dir, pattern = "5299", full.names = TRUE)
  if (length(raw_files) > 0) {
    cat("Found alternative:", raw_files[1], "\n")
    inspect_file_detail(raw_files[1], "GUINEA")
  } else {
    cat("No file containing '5299' found in directory\n")
  }
}

# Also list all files in the directory for reference
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALL FILES IN DATA/RAW DIRECTORY:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
all_files <- list.files(raw_dir, pattern = "\\.parquet$|\\.csv$", full.names = FALSE)
for (f in all_files) {
  file_size <- file.info(file.path(raw_dir, f))$size / 1024  # KB
  cat(sprintf("  %-30s (%.1f KB)\n", f, file_size))
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("DIAGNOSTIC COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
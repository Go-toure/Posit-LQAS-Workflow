# Diagnostic script for CHAD and GUINEA files
library(arrow)
library(data.table)

# Function to inspect file structure
inspect_lqas_file <- function(file_path, country_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("INSPECTING:", country_name, "\n")
  cat("File:", basename(file_path), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Read the file
  data <- tryCatch({
    read_parquet(file_path)
  }, error = function(e) {
    cat("ERROR reading file:", e$message, "\n")
    return(NULL)
  })

  if (is.null(data)) return(NULL)

  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

  # Show all column names
  cat("ALL COLUMN NAMES:\n")
  all_cols <- names(data)
  for (i in seq_along(all_cols)) {
    cat(sprintf("%3d: %s\n", i, all_cols[i]))
  }

  # Look for vaccination-related columns
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("VACCINATION-RELATED COLUMNS:\n")

  vac_patterns <- c("FM_Child", "Vaccinated", "Immunized", "Received", "Dose",
                    "Vaccine", "OPV", "nOPV", "bOPV", "mOPV")

  for (pattern in vac_patterns) {
    matches <- grep(pattern, all_cols, value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) {
      cat(sprintf("  %s: %d columns\n", pattern, length(matches)))
      if (length(matches) <= 5) {
        for (m in matches) cat("    -", m, "\n")
      } else {
        cat("    First 5:", paste(head(matches, 5), collapse = ", "), "\n")
      }
    }
  }

  # Check Count_HH column if it exists
  if ("Count_HH" %in% all_cols) {
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("Count_HH COLUMN ANALYSIS:\n")

    # Sample values
    sample_vals <- head(data$Count_HH, 3)
    for (i in seq_along(sample_vals)) {
      val <- as.character(sample_vals[i])
      cat(sprintf("  Row %d (first 500 chars):\n    %s\n", i, substr(val, 1, 500)))

      # Check if it's JSON
      if (grepl("^\\[|\\{", val)) {
        cat("    ✓ Appears to be JSON format\n")

        # Try to parse
        tryCatch({
          parsed <- jsonlite::fromJSON(val, simplifyVector = FALSE)
          if (is.list(parsed) && length(parsed) > 0) {
            cat("    ✓ Successfully parsed JSON\n")
            cat("    Number of child records:", length(parsed), "\n")
            if (length(parsed) > 0 && is.list(parsed[[1]])) {
              cat("    Fields in first child:", paste(names(parsed[[1]]), collapse = ", "), "\n")
            }
          }
        }, error = function(e) {
          cat("    ✗ Failed to parse JSON:", e$message, "\n")
        })
      }
    }
  }

  # Check for HH bracket format
  hh_cols <- grep("^HH\\[", all_cols, value = TRUE)
  if (length(hh_cols) > 0) {
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("HH BRACKET COLUMNS FOUND:", length(hh_cols), "\n")
    cat("First 10:", paste(head(hh_cols, 10), collapse = ", "), "\n")
  }

  # Check for direct child columns (Count_HH[1]/... format)
  count_hh_cols <- grep("^Count_HH\\[\\d+\\]/", all_cols, value = TRUE)
  if (length(count_hh_cols) > 0) {
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("COUNT_HH BRACKET COLUMNS:", length(count_hh_cols), "\n")
    cat("Sample (first 20):\n")
    for (col in head(count_hh_cols, 20)) {
      cat("  ", col, "\n")
    }
  }

  # Sample data from first row
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("SAMPLE DATA (Row 1):\n")
  if (nrow(data) > 0) {
    row1 <- as.list(data[1, ])
    # Show key columns
    key_cols <- intersect(c("Country", "Region", "District", "Response", "roundNumber",
                            "Date_of_LQAS", "Cluster", "Count_HH_count"), names(data))
    for (col in key_cols) {
      cat(sprintf("  %s: %s\n", col, row1[[col]]))
    }
  }

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
}

# Run diagnostics
chad_file <- "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/data/raw/4431.parquet"
guinea_file <- "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/data/raw/5299.parquet"

if (file.exists(chad_file)) {
  inspect_lqas_file(chad_file, "CHAD")
} else {
  cat("CHAD file not found at:", chad_file, "\n")
}

if (file.exists(guinea_file)) {
  inspect_lqas_file(guinea_file, "GUINEA")
} else {
  cat("GUINEA file not found at:", guinea_file, "\n")
}

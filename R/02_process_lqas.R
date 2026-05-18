#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Processing Script
# EXACT mirror of convert_padacord_LQAS_to_csv logic
# Processes each file individually, handles special cases
# Reads Parquet files (converted from original RDS/QS)
# Saves individual processed files + combined final output
# ============================================================

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(logger)
  library(fs)
  library(stringr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readxl)
  library(janitor)
  library(rlang)
  library(tidyverse)
  library(tools)
  library(jsonlite)
})

# Create directories
dir_create("logs")
dir_create("data/processed")
dir_create("data/final")

# Configure logging
log_appender(appender_file("logs/process.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting LQAS Data Processing (Mirroring Original)")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# ============================================================
# Helper Functions (EXACT from original)
# ============================================================

rename_repetitive_columns <- function(data) {
  pattern <- "^Count_HH\\[\\d+\\]/Count_HH/"
  new_columns <- sapply(colnames(data), function(col) {
    if (grepl(pattern, col)) gsub("Count_HH/", "", col) else col
  })
  colnames(data) <- new_columns
  return(data)
}

apply_custom_rules <- function(data, file_name) {
  if (startsWith(file_name, "3583")) {
    data <- data %>% mutate(Country = "GHA")
  }
  if (startsWith(file_name, "8834")) {
    data <- data %>% mutate(Region = District, District = District)
  }
  if (startsWith(file_name, "4351")) {
    data <- data %>% mutate(District = district)
  }
  return(data)
}

normalize_reason_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")

  x[x %in% c(
    "", "na", "n_a", "n/a", "null", "none", "missing", "unknown", ".", "-", "--"
  )] <- NA

  x
}

map_abs_reason <- function(x) {
  x <- normalize_reason_text(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("farm") ~ "Farm",
    x %in% c("market") ~ "Market",
    x %in% c("school") ~ "School",
    x %in% c("in_playground", "playground") ~ "In_playground",
    x %in% c("travelled", "travel", "travelling", "traveling") ~ "Travelled",
    TRUE ~ "Other"
  )
}

map_nc_reason <- function(x) {
  x <- normalize_reason_text(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("religious_cultural", "religious", "cultural", "religious_and_cultural") ~ "Religious_Cultural",
    x %in% c("vaccines_safety", "vaccine_safety", "safety", "vaccine_safety_concern") ~ "Vaccines_Safety",
    x %in% c("no_felt_need", "no_need", "no_need_felt", "no_perceived_need") ~ "No_felt_Need",
    x %in% c("too_many_rnd", "too_many_round", "too_many_rounds") ~ "Too_Many_Rnd",
    x %in% c("no_care_giver_consent", "no_caregiver_consent", "caregiver_refusal", "no_parental_consent") ~ "No_Care_giver_Consent",
    x %in% c("child_sick", "child_is_sick", "sick_child", "childsick") ~ "Child-Sick",
    x %in% c("covid_19", "covid19", "covid") ~ "Covid_19",
    x %in% c("poliofree", "polio_free", "poliofree_area") ~ "PolioFree",
    x %in% c("nopvconcern", "nopv_concern", "n_opv_concern") ~ "nOPVConcern",
    TRUE ~ "Others"
  )
}

make_reason_slug <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    stringr::str_to_lower()
}

build_reason_wide <- function(data, reason_cols, names_prefix) {
  if (length(reason_cols) == 0) {
    return(
      data %>%
        dplyr::distinct(Country, Region, District, Response, roundNumber)
    )
  }

  data %>%
    dplyr::select(Country, Region, District, Response, roundNumber, dplyr::all_of(reason_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(reason_cols),
      names_to = "reason_source_col",
      values_to = "reason"
    ) %>%
    dplyr::filter(!is.na(reason)) %>%
    dplyr::count(Country, Region, District, Response, roundNumber, reason, name = "n") %>%
    dplyr::mutate(reason = make_reason_slug(reason)) %>%
    tidyr::pivot_wider(
      names_from = reason,
      values_from = n,
      values_fill = 0,
      names_prefix = names_prefix
    )
}

# ============================================================
# GENERIC HANDLER: Process JSON Nested Structure
# Detects and expands any file with JSON nested data in Count_HH column
# ============================================================

process_json_nested_format <- function(file_path, file_name) {
  log_info("  🔸 JSON NESTED FORMAT: Processing {file_name} (detected JSON structure)")

  # Read the parquet file
  data <- tryCatch({
    as.data.table(read_parquet(file_path))
  }, error = function(e) {
    log_error("    Failed to read file: {e$message}")
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  log_info("    Read {nrow(data)} rows with {ncol(data)} columns")

  # Check if this is the JSON nested structure
  if (!"Count_HH" %in% names(data)) {
    log_warn("    No Count_HH column found - cannot process as JSON nested format")
    return(NULL)
  }

  # Function to safely parse JSON
  parse_json_column <- function(json_str) {
    if (is.na(json_str) || json_str == "" || json_str == "[]") return(list())

    # Clean the JSON string
    json_str <- as.character(json_str)
    json_str <- gsub("'", '"', json_str)  # Replace single quotes with double quotes
    json_str <- gsub("None", 'null', json_str)
    json_str <- gsub("True", 'true', json_str)
    json_str <- gsub("False", 'false', json_str)

    # Parse
    tryCatch({
      result <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      if (is.null(result)) return(list())
      if (!is.list(result)) return(list())
      return(result)
    }, error = function(e) {
      return(list())
    })
  }

  # Detect what fields are in the JSON structure by examining first non-empty entry
  detected_fields <- c()
  for (i in 1:min(10, nrow(data))) {
    json_data <- parse_json_column(data$Count_HH[i])
    if (length(json_data) > 0 && length(json_data[[1]]) > 0) {
      detected_fields <- names(json_data[[1]])
      if (length(detected_fields) > 0) {
        log_info("    Detected JSON fields: {paste(detected_fields, collapse=', ')}")
        break
      }
    }
  }

  # Expand each row's Count_HH JSON into multiple columns
  expanded_rows <- list()
  rows_with_children <- 0
  total_children <- 0

  for (i in 1:nrow(data)) {
    json_data <- parse_json_column(data$Count_HH[i])

    if (length(json_data) == 0) {
      # No child data - create a single row with NA values for child fields
      new_row <- as.list(data[i, ])
      new_row[["Count_HH_count"]] <- 0
      # Add placeholder for at least one child to maintain structure
      for (field in detected_fields) {
        field_name <- gsub("Count_HH/", "", field)
        new_row[[paste0("Count_HH[1]/", field_name)]] <- NA
      }
      expanded_rows[[length(expanded_rows) + 1]] <- new_row
      next
    }

    rows_with_children <- rows_with_children + 1
    total_children <- total_children + length(json_data)

    # Each element in json_data is a child
    for (child_idx in seq_along(json_data)) {
      child <- json_data[[child_idx]]

      # Create a new row combining metadata + child data
      new_row <- as.list(data[i, ])

      # Add child-specific fields with proper naming
      new_row[["Count_HH_count"]] <- length(json_data)

      # Map each field to the expected naming convention
      for (field_name in names(child)) {
        # Remove "Count_HH/" prefix if present
        clean_field <- gsub("^Count_HH/", "", field_name)
        col_name <- paste0("Count_HH[", child_idx, "]/", clean_field)
        new_row[[col_name]] <- child[[field_name]]
      }

      # Also check for any fields that might be in the JSON but not detected
      for (field_name in detected_fields) {
        clean_field <- gsub("^Count_HH/", "", field_name)
        col_name <- paste0("Count_HH[", child_idx, "]/", clean_field)
        if (!col_name %in% names(new_row)) {
          new_row[[col_name]] <- child[[field_name]] %||% NA
        }
      }

      expanded_rows[[length(expanded_rows) + 1]] <- new_row
    }
  }

  if (length(expanded_rows) == 0) {
    log_warn("    No valid records found after expansion")
    return(NULL)
  }

  # Combine expanded rows
  expanded_data <- rbindlist(lapply(expanded_rows, as.data.table), fill = TRUE)

  # Remove the original Count_HH column
  expanded_data[, Count_HH := NULL]

  log_info("    Expanded: {nrow(data)} rows -> {nrow(expanded_data)} child records")
  log_info("    Rows with children: {rows_with_children}, Total children: {total_children}")

  # Save temp file and process regularly
  temp_file <- tempfile(fileext = ".parquet")
  write_parquet(expanded_data, temp_file)
  result <- process_regular_file(temp_file, file_name)
  unlink(temp_file)

  return(result)
}

# ============================================================
# CONVERTER: HH bracket format to standard (like form 4987)
# ============================================================

convert_hh_format <- function(file_path, file_name) {
  log_info("  🔸 Converting HH bracket format to standard")

  data <- tryCatch({
    as.data.table(read_parquet(file_path))
  }, error = function(e) {
    log_error("    Failed to read: {e$message}")
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  log_info("    Read {nrow(data)} rows with {ncol(data)} columns")

  # Rename columns from HH[1]/HH/... to Count_HH[1]/...
  old_names <- names(data)
  new_names <- old_names

  # Pattern: HH[1]/HH/count -> Count_HH[1]/count
  new_names <- gsub("^HH\\[(\\d+)\\]/HH/", "Count_HH[\\1]/", new_names)
  # Handle nested groups: HH[1]/HH/group1/Tot_child_NC_HH -> Count_HH[1]/group1/Tot_child_NC_HH
  new_names <- gsub("^HH\\[(\\d+)\\]/HH/", "Count_HH[\\1]/", new_names)

  setnames(data, old_names, new_names)

  # Map field names to expected ones
  # U5_Vac_FM_HH -> FM_Child
  fm_cols <- grep("U5_Vac_FM_HH", names(data), value = TRUE)
  for (col in fm_cols) {
    new_col <- gsub("U5_Vac_FM_HH", "FM_Child", col)
    setnames(data, col, new_col)
    log_info("    Mapped column: {col} -> {new_col}")
  }

  log_info("    Converted to standard format with {ncol(data)} columns")

  # Process as regular file
  temp_file <- tempfile(fileext = ".parquet")
  write_parquet(data, temp_file)
  result <- process_regular_file(temp_file, file_name)
  unlink(temp_file)

  return(result)
}

# ============================================================
# FORMAT DETECTOR: Auto-detect file format and route to appropriate handler
# ============================================================

detect_and_process_file <- function(file_path, file_name) {
  log_info("  Detecting format for: {file_name}")

  # Read just the first row to check structure
  first_row <- tryCatch({
    read_parquet(file_path, n_rows = 1)
  }, error = function(e) {
    log_error("    Cannot read file: {e$message}")
    return(NULL)
  })

  if (is.null(first_row)) {
    return(NULL)
  }

  # Check for JSON nested structure in Count_HH
  if ("Count_HH" %in% names(first_row)) {
    sample_val <- as.character(first_row$Count_HH[1])
    is_json <- grepl("\\[.*\\{.*\\}.*\\]", sample_val) ||
      grepl("\\{.*\\}", sample_val) ||
      grepl("'Count_HH/", sample_val)

    if (is_json) {
      log_info("    ✅ Detected JSON nested structure - using JSON expander")
      return(process_json_nested_format(file_path, file_name))
    }
  }

  # Check for HH bracket notation (new format like 4987)
  if (any(grepl("^HH\\[\\d+\\]/", names(first_row)))) {
    log_info("    ✅ Detected HH bracket format - converting to standard")
    return(convert_hh_format(file_path, file_name))
  }

  # Check for standard Count_HH bracket notation
  if (any(grepl("^Count_HH\\[\\d+\\]/", names(first_row)))) {
    log_info("    ✅ Detected standard Count_HH bracket format")
    return(process_regular_file(file_path, file_name))
  }

  # Unknown format - log details
  log_warn("    ⚠️ Unknown format for {file_name}")
  log_info("    First 10 column names: {paste(head(names(first_row), 10), collapse=', ')}")
  return(NULL)
}

# ============================================================
# SPECIAL CASE: Process 272 (EXACT mirror of original NIE script)
# ============================================================

process_special_272 <- function(file_path, file_name) {
  log_info("  🔸 SPECIAL CASE: Processing 272 (Nigeria LQAS) - Mirroring original")

  # Set locale for French month names
  Sys.setlocale("LC_TIME", "French_France.1252")

  # Read file (supports parquet or csv)
  if (grepl("\\.parquet$", file_path)) {
    df <- as.data.table(read_parquet(file_path))
  } else {
    df <- fread(file_path)
  }

  log_info("    Read {nrow(df)} rows")

  # EXACT original transformations
  df <- df |>
    mutate(country = "NIE") |>
    mutate(Cluster = ifelse(!is.na(Cluster), 1, NA)) |>
    mutate(across(starts_with("Children_Seen_"), as.numeric)) |>
    select(
      country, Region = states, District = lgas, Cluster, today,
      matches("Children_seen_h[1-9]|Children_Seen_h10"),
      matches("Sex_Child[1-9]|Sex_Child10"),
      matches("FM_Child[1-9]|FM_Child10"),
      matches("Reason_Not_FM[1-9]|Reason_Not_FM10"),
      matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")
    )

  # Parse and extract date info
  df <- df |>
    mutate(
      today = parse_date_time(today, orders = c("ymd", "dmy", "mdy"), quiet = TRUE),
      year = year(today),
      month = format(today, "%b")
    )

  # Compute households visited
  df <- df |>
    mutate(across(matches("Children_seen_h[1-9]|Children_Seen_h10"),
                  ~ ifelse(!is.na(.), 1, 0),
                  .names = "h_{.col}")) |>
    mutate(tot_hh_visited = rowSums(across(starts_with("h_Children_Seen_")), na.rm = TRUE))

  # Clean binary variables
  clean_binary_var <- function(x) {
    case_when(
      tolower(x) %in% c("yes", "y", "1") ~ 1,
      tolower(x) %in% c("no", "n", "0") ~ 0,
      TRUE ~ NA_real_
    )
  }

  clean_sex_var <- function(x) {
    case_when(
      toupper(x) == "F" ~ 1,
      toupper(x) == "M" ~ 0,
      TRUE ~ NA_real_
    )
  }

  df <- df |>
    mutate(across(matches("Sex_Child[1-9]|Sex_Child10"), clean_sex_var),
           across(matches("FM_Child[1-9]|FM_Child10"), clean_binary_var),
           across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10"), clean_binary_var))

  # Process reasons
  reason_cols <- c("childnotborn", "childabsent", "noncompliance", "housenotvisited", "security")
  for (r in reason_cols) {
    for (i in 1:10) {
      col_in <- paste0("Reason_Not_FM", i)
      col_out <- paste0("R_", r, i)
      if (col_in %in% names(df)) {
        df[[col_out]] <- ifelse(tolower(df[[col_in]]) == r, 1, 0)
      }
    }
  }

  # Summary stats
  df <- df |>
    mutate(
      female_sampled = rowSums(across(matches("Sex_Child[1-9]|Sex_Child10")), na.rm = TRUE),
      male_sampled = tot_hh_visited - female_sampled,
      total_vaccinated = rowSums(across(matches("FM_Child[1-9]|FM_Child10")), na.rm = TRUE),
      missed_child = tot_hh_visited - total_vaccinated
    ) |>
    rowwise() |>
    mutate(
      female_vaccinated = sum(
        unlist(across(matches("Sex_Child[1-9]|Sex_Child10"))) == 1 &
          unlist(across(matches("FM_Child[1-9]|FM_Child10"))) == 1,
        na.rm = TRUE
      ),
      male_vaccinated = total_vaccinated - female_vaccinated
    ) |>
    ungroup()

  # Aggregate reasons
  df <- df |>
    mutate(
      r_house_not_visited = rowSums(across(matches("R_housenotvisited[1-9]|R_housenotvisited10")), na.rm = TRUE),
      r_childabsent = rowSums(across(matches("r_childabsent[1-9]|r_childabsent10")), na.rm = TRUE),
      r_non_compliance = rowSums(across(matches("R_noncompliance[1-9]|R_noncompliance10")), na.rm = TRUE),
      r_childnotborn = rowSums(across(matches("r_childnotborn[1-9]|r_childnotborn10")), na.rm = TRUE),
      r_security = rowSums(across(matches("r_security[1-9]|r_security10")), na.rm = TRUE),
      care_giver_informed_sia = rowSums(across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")), na.rm = TRUE)
    )

  # Round and response
  df <- df |>
    mutate(
      today = as_date(today),
      month = format(today, "%b"),
      roundNumber = case_when(
        str_detect(month, "janv.") ~ "Rnd1",
        str_detect(month, "févr.") ~ "Rnd2",
        str_detect(month, "mars") ~ "Rnd3",
        str_detect(month, "avr.")  ~ "Rnd4",
        str_detect(month, "mai")  ~ "Rnd5",
        str_detect(month, "juin") ~ "Rnd6",
        str_detect(month, "juil.") ~ "Rnd7",
        str_detect(month, "août") ~ "Rnd8",
        str_detect(month, "sept.") ~ "Rnd9",
        str_detect(month, "oct.")  ~ "Rnd10",
        str_detect(month, "nov.")  ~ "Rnd11",
        str_detect(month, "déc.")  ~ "Rnd12",
        TRUE ~ NA_character_
      )
    )

  df <- df |>
    mutate(
      roundNumber = case_when(
        year == 2025 & month %in% c("juin", "juil.") ~ "Rnd2",
        year == 2025 & month %in% c("janv.", "févr.", "mars", "avr.", "mai") ~ "Rnd1",
        year == 2024 & month %in% c("févr.", "mars") ~ "Rnd1",
        year == 2024 & month %in% c("avr.", "mai", "juin","août") ~ "Rnd2",
        year == 2024 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2024 & month == "nov." ~ "Rnd4",
        year == 2024 & month == "déc." ~ "Rnd5",
        year == 2023 & month %in% c("janv.", "mai") ~ "Rnd1",
        year == 2023 & month %in% c("juin", "juil.", "août") ~ "Rnd2",
        year == 2023 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2023 & month == "nov." ~ "Rnd4",
        year == 2023 & month == "déc." ~ "Rnd5",
        TRUE ~ roundNumber
      ),
      total_sampled = tot_hh_visited,
      vaccine.type = case_when(
        year == 2025 & month %in% c("juin", "juil.") ~ "nOPV2",
        year == 2025 & month %in% c("janv.", "avr.") ~ "nOPV2",
        year == 2024 ~ "nOPV2",
        year == 2023 & month == "mai" ~ "fIPV+nOPV2",
        year == 2023 & month == "juil." ~ "fIPV+nOPV2",
        year == 2023 & month %in% c("août", "oct.", "nov.", "déc.") ~ "nOPV2",
        year == 2023 & month == "sept." ~ "fIPV+nOPV2",
        year %in% 2020:2022 ~ "bOPV",
        TRUE ~ "nOPV2"
      ),
      response = case_when(
        year == 2025 & month %in% c("juin", "juil.") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2025 & month %in% c("avr.", "mai") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2024 ~ "NIE-2024-nOPV2",
        year == 2023 & month %in% c("mai", "juin") ~ "NIE-2023-04-02_nOPV",
        year == 2023 & month %in% c("juil.", "août", "sept.", "oct.", "nov.", "déc.") ~ "NIE-2023-07-03_nOPV",
        year == 2020 ~ "NGA-20DS-01-2020",
        year == 2021 & month %in% c("mars", "avr.") ~ "NGA-2021-013-1",
        year == 2021 & month %in% c("avr.", "mai") ~ "NGA-2021-011-1",
        year == 2021 & month == "juin" ~ "NGA-2021-016-1",
        year == 2021 & month %in% c("juil.", "août") ~ "NGA-2021-014-1",
        year == 2021 & month == "sept." ~ "NGA-2021-020-2",
        TRUE ~ "OBR_name"
      )
    )

  # Aggregate to cluster level
  df <- df |>
    filter(year > 2019) |>
    group_by(country, Region, District, response, vaccine.type, roundNumber) |>
    summarise(
      start_date = min(today),
      end_date = max(today),
      year = year(start_date),
      numbercluster = sum(Cluster, na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_non_compliance = sum(r_non_compliance, na.rm = TRUE),
      r_house_not_visited = sum(r_house_not_visited, na.rm = TRUE),
      r_childabsent = sum(r_childabsent, na.rm = TRUE),
      r_security = sum(r_security, na.rm = TRUE),
      r_childnotborn = sum(r_childnotborn, na.rm = TRUE),
      care_giver_informed_sia = sum(care_giver_informed_sia, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      percent_care_giver_informed_sia = ifelse(total_sampled > 0, round(care_giver_informed_sia / total_sampled * 100, 2), 0),
      total_missed = ifelse(total_sampled < 60, 60 - total_sampled + missed_child, missed_child),
      status = ifelse(total_missed <= 3, "Pass", "Fail"),
      performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      ),
      tot_r = r_non_compliance + r_house_not_visited + r_childabsent + r_security + r_childnotborn,
      other_r = pmax(total_missed - tot_r, 0),
      prct_r_non_compliance = ifelse(total_missed > 0, round(r_non_compliance / total_missed * 100, 2), 0),
      prct_r_house_not_visited = ifelse(total_missed > 0, round(r_house_not_visited / total_missed * 100, 2), 0),
      prct_r_childabsent = ifelse(total_missed > 0, round(r_childabsent / total_missed * 100, 2), 0),
      prct_r_childnotborn = ifelse(total_missed > 0, round(r_childnotborn / total_missed * 100, 2), 0),
      prct_r_security = ifelse(total_missed > 0, round(r_security / total_missed * 100, 2), 0),
      prct_other_r = ifelse(total_missed > 0, round(other_r / total_missed * 100, 2), 0)
    ) |>
    mutate(
      start_date = case_when(
        response == "OBR_name" &
          year(start_date) == 2025 &
          month(start_date) == 2 &
          roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "OBR_name" &
          year(start_date) == 2025 &
          month(start_date) == 3 &
          roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "NIE-2025-04-01_nOPV_NIDs" &
          year(start_date) == 2025 &
          month(start_date) == 5 &
          roundNumber == "Rnd1" ~ as_date("2025-04-26"),
        response == "NIE-2025-04-01_nOPV_NIDs" &
          year(start_date) == 2025 &
          month(start_date) == 7 &
          roundNumber == "Rnd2" ~ as_date("2025-06-14"),
        TRUE ~ start_date
      )
    )

  # Load preparedness data and join
  prep_data_file <- "data/lookup/lqas_lookup.xlsx"
  if (file.exists(prep_data_file)) {
    prep_data <- read_excel(prep_data_file) |>
      filter(Country == "NIGERIA") |>
      mutate(
        `Round Number` = case_when(
          `Round Number` == "Round 0" ~ "Rnd0",
          `Round Number` == "Round 1" ~ "Rnd1",
          `Round Number` == "Round 2" ~ "Rnd2",
          `Round Number` == "Round 3" ~ "Rnd3",
          `Round Number` == "Round 4" ~ "Rnd4",
          `Round Number` == "Round 5" ~ "Rnd5",
          `Round Number` == "Round 6" ~ "Rnd6",
          TRUE ~ `Round Number`
        )
      )

    prep_data <- prep_data |>
      rename(
        response = `OBR Name`,
        vaccine.type = Vaccines,
        roundNumber = `Round Number`
      ) |>
      mutate(
        round_start_date = as_date(`Round Start Date`),
        start_date = round_start_date + 4,
        end_date = as_date(start_date) + 1
      )

    prep_data <- prep_data |>
      select(response, vaccine.type, roundNumber, round_start_date, start_date, end_date)

    lookup_table <- as_tibble(prep_data) |>
      mutate(
        start_date = as_date(start_date),
        end_date = as_date(end_date),
        round_start_date = as_date(round_start_date)
      )

    # Join the lookup table
    df <- df |>
      left_join(lookup_table, by = c("response", "vaccine.type", "roundNumber")) |>
      mutate(
        start_date = coalesce(start_date.y, as_date(start_date.x)),
        end_date = as_date(start_date) + 1,
        round_start_date = coalesce(round_start_date, start_date - days(4))
      ) |>
      select(-start_date.x, -start_date.y, -end_date.x, -end_date.y) |>
      filter(!is.na(District))
  }

  # Special districts vaccine.type rule
  districts_special <- c("YUSUFARI", "GURI", "BIRINIWA", "KIRI KASAMA", "NGURU", "MACHINA", "KARASUWA", "BARDE")

  result <- df |>
    mutate(vaccine.type = case_when(
      District %in% districts_special & response == "NIE-2025-04-01_nOPV_NIDs" ~ "nOPV2 & bOPV",
      TRUE ~ vaccine.type
    )) |>
    select(
      country, province = Region, district = District, response, vaccine.type,
      roundNumber, numbercluster, round_start_date, start_date, end_date, year,
      male_sampled, female_sampled, total_sampled,
      male_vaccinated, female_vaccinated, total_vaccinated, missed_child,
      r_non_compliance, r_house_not_visited, r_childabsent, r_security, r_childnotborn,
      care_giver_informed_sia, percent_care_giver_informed_sia, total_missed, status, performance,
      tot_r, other_r, prct_r_non_compliance, prct_r_house_not_visited, prct_r_childabsent,
      prct_r_childnotborn, prct_r_security, prct_other_r
    )

  log_info("    Processed 272: {nrow(result)} rows")

  return(result)
}

# ============================================================
# SPECIAL CASE: Process Nigeria CSV (EXACT mirror)
# ============================================================

process_special_nigeria <- function(file_path, file_name) {
  log_info("  🔸 SPECIAL CASE: Processing Nigeria CSV - Mirroring original")

  # Set locale for French month names
  Sys.setlocale("LC_TIME", "French_France.1252")

  # Read CSV
  raw <- fread(file_path)
  log_info("    Read {nrow(raw)} rows")

  # Fix invalid UTF-8
  raw <- raw %>%
    mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")))

  # STANDARDIZE REQUIRED COLUMNS
  if (!("states" %in% names(raw))) {
    if ("state" %in% names(raw)) raw$states <- raw$state
    if ("State" %in% names(raw)) raw$states <- raw$State
  }
  if (!("lgas" %in% names(raw))) {
    if ("lga" %in% names(raw)) raw$lgas <- raw$lga
    if ("LGA" %in% names(raw)) raw$lgas <- raw$LGA
  }
  if (!("today" %in% names(raw))) {
    if ("date" %in% names(raw)) raw$today <- raw$date
    if ("Date" %in% names(raw)) raw$today <- raw$Date
  }
  if (!("Cluster" %in% names(raw))) raw$Cluster <- NA

  # ORIGINAL SCRIPT STARTS HERE
  df <- raw |>
    mutate(country = "NIE") |>
    mutate(Cluster = ifelse(!is.na(Cluster), 1, NA_real_)) |>
    mutate(across(starts_with("Children_Seen_"), ~ suppressWarnings(as.numeric(.x)))) |>
    select(
      country, Region = states, District = lgas, Cluster, today,
      matches("Children_seen_h[1-9]|Children_Seen_h10"),
      matches("Sex_Child[1-9]|Sex_Child10"),
      matches("FM_Child[1-9]|FM_Child10"),
      matches("Reason_Not_FM[1-9]|Reason_Not_FM10"),
      matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")
    )

  # Parse and extract date info
  df <- df |>
    mutate(
      today = parse_date_time(today, orders = c("ymd", "dmy", "mdy"), quiet = TRUE),
      year = year(today),
      month = format(today, "%b")
    )

  # Compute households visited
  df <- df |>
    mutate(across(matches("Children_seen_h[1-9]|Children_Seen_h10"),
                  ~ ifelse(!is.na(.), 1, 0),
                  .names = "h_{.col}")) |>
    mutate(tot_hh_visited = rowSums(across(starts_with("h_Children_Seen_")), na.rm = TRUE))

  # Clean binary variables
  clean_binary_var <- function(x) {
    case_when(
      tolower(x) %in% c("yes", "y", "1") ~ 1,
      tolower(x) %in% c("no", "n", "0") ~ 0,
      TRUE ~ NA_real_
    )
  }

  clean_sex_var <- function(x) {
    case_when(
      toupper(x) == "F" ~ 1,
      toupper(x) == "M" ~ 0,
      TRUE ~ NA_real_
    )
  }

  df <- df |>
    mutate(
      across(matches("Sex_Child[1-9]|Sex_Child10"), clean_sex_var),
      across(matches("FM_Child[1-9]|FM_Child10"), clean_binary_var),
      across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10"), clean_binary_var)
    )

  # Process reasons
  reason_cols <- c("childnotborn", "childabsent", "noncompliance", "housenotvisited", "security")
  for (r in reason_cols) {
    for (i in 1:10) {
      col_in <- paste0("Reason_Not_FM", i)
      col_out <- paste0("R_", r, i)
      if (col_in %in% names(df)) {
        df[[col_out]] <- ifelse(tolower(as.character(df[[col_in]])) == r, 1, 0)
      }
    }
  }

  # Summary stats
  df <- df |>
    mutate(
      female_sampled = rowSums(across(matches("Sex_Child[1-9]|Sex_Child10")), na.rm = TRUE),
      male_sampled = tot_hh_visited - female_sampled,
      total_vaccinated = rowSums(across(matches("FM_Child[1-9]|FM_Child10")), na.rm = TRUE),
      missed_child = tot_hh_visited - total_vaccinated
    ) |>
    rowwise() |>
    mutate(
      female_vaccinated = sum(
        unlist(across(matches("Sex_Child[1-9]|Sex_Child10"))) == 1 &
          unlist(across(matches("FM_Child[1-9]|FM_Child10"))) == 1,
        na.rm = TRUE
      ),
      male_vaccinated = total_vaccinated - female_vaccinated
    ) |>
    ungroup()

  # Aggregate reasons
  df <- df |>
    mutate(
      r_house_not_visited = rowSums(across(matches("R_housenotvisited[1-9]|R_housenotvisited10")), na.rm = TRUE),
      r_childabsent = rowSums(across(matches("r_childabsent[1-9]|r_childabsent10")), na.rm = TRUE),
      r_non_compliance = rowSums(across(matches("R_noncompliance[1-9]|R_noncompliance10")), na.rm = TRUE),
      r_childnotborn = rowSums(across(matches("r_childnotborn[1-9]|r_childnotborn10")), na.rm = TRUE),
      r_security = rowSums(across(matches("r_security[1-9]|r_security10")), na.rm = TRUE),
      care_giver_informed_sia = rowSums(across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")), na.rm = TRUE)
    )

  # Round and response
  df <- df |>
    mutate(
      today = as_date(today),
      month = format(today, "%b"),
      roundNumber = case_when(
        str_detect(month, "janv.") ~ "Rnd1",
        str_detect(month, "févr.") ~ "Rnd2",
        str_detect(month, "mars") ~ "Rnd3",
        str_detect(month, "avr.") ~ "Rnd4",
        str_detect(month, "mai") ~ "Rnd5",
        str_detect(month, "juin") ~ "Rnd6",
        str_detect(month, "juil.") ~ "Rnd7",
        str_detect(month, "août") ~ "Rnd8",
        str_detect(month, "sept.") ~ "Rnd9",
        str_detect(month, "oct.") ~ "Rnd10",
        str_detect(month, "nov.") ~ "Rnd11",
        str_detect(month, "déc.") ~ "Rnd12",
        TRUE ~ NA_character_
      )
    )

  df <- df |>
    mutate(
      roundNumber = case_when(
        year == 2025 & month %in% c("juin", "juil.") ~ "Rnd2",
        year == 2025 & month %in% c("janv.", "févr.", "mars", "avr.", "mai") ~ "Rnd1",
        year == 2024 & month %in% c("févr.", "mars") ~ "Rnd1",
        year == 2024 & month %in% c("avr.", "mai", "juin", "août") ~ "Rnd2",
        year == 2024 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2024 & month == "nov." ~ "Rnd4",
        year == 2024 & month == "déc." ~ "Rnd5",
        year == 2023 & month %in% c("janv.", "mai") ~ "Rnd1",
        year == 2023 & month %in% c("juin", "juil.", "août") ~ "Rnd2",
        year == 2023 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2023 & month == "nov." ~ "Rnd4",
        year == 2023 & month == "déc." ~ "Rnd5",
        TRUE ~ roundNumber
      ),
      total_sampled = tot_hh_visited,
      vaccine.type = case_when(
        year == 2025 & month %in% c("oct.", "nov.") ~ "nOPV2",
        year == 2025 & month %in% c("juin", "juil.") ~ "nOPV2",
        year == 2025 & month %in% c("janv.", "avr.") ~ "nOPV2",
        year == 2024 ~ "nOPV2",
        year == 2023 & month == "mai" ~ "fIPV+nOPV2",
        year == 2023 & month == "juil." ~ "fIPV+nOPV2",
        year == 2023 & month %in% c("août", "oct.", "nov.", "déc.") ~ "nOPV2",
        year == 2023 & month == "sept." ~ "fIPV+nOPV2",
        year %in% 2020:2022 ~ "bOPV",
        TRUE ~ "nOPV2"
      ),
      response = case_when(
        year == 2025 & month %in% c("oct.", "nov.") ~ "NIE-2025-10-01_nOPV_sNID",
        year == 2025 & month %in% c("juin", "juil.") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2025 & month %in% c("avr.", "mai") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2024 ~ "NIE-2024-nOPV2",
        year == 2023 & month %in% c("mai", "juin") ~ "NIE-2023-04-02_nOPV",
        year == 2023 & month %in% c("juil.", "août", "sept.", "oct.", "nov.", "déc.") ~ "NIE-2023-07-03_nOPV",
        year == 2020 ~ "NGA-20DS-01-2020",
        year == 2021 & month %in% c("mars", "avr.") ~ "NGA-2021-013-1",
        year == 2021 & month %in% c("avr.", "mai") ~ "NGA-2021-011-1",
        year == 2021 & month == "juin" ~ "NGA-2021-016-1",
        year == 2021 & month %in% c("juil.", "août") ~ "NGA-2021-014-1",
        year == 2021 & month == "sept." ~ "NGA-2021-020-2",
        TRUE ~ "OBR_name"
      )
    )

  df <- df |>
    mutate(
      roundNumber = case_when(
        response == "NIE-2025-10-01_nOPV_sNID" & year == 2025 & month %in% c("oct.", "nov.") ~ "Rnd1",
        TRUE ~ roundNumber
      ),
      today = case_when(
        response == "NIE-2025-10-01_nOPV_sNID" & roundNumber == "Rnd1" ~ as.Date("2025-10-10"),
        TRUE ~ today
      )
    )

  # Aggregate to cluster level
  df <- df |>
    filter(year > 2019) |>
    group_by(country, Region, District, response, vaccine.type, roundNumber) |>
    summarise(
      start_date = min(today),
      end_date = max(today),
      year = year(start_date),
      numbercluster = sum(Cluster, na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_non_compliance = sum(r_non_compliance, na.rm = TRUE),
      r_house_not_visited = sum(r_house_not_visited, na.rm = TRUE),
      r_childabsent = sum(r_childabsent, na.rm = TRUE),
      r_security = sum(r_security, na.rm = TRUE),
      r_childnotborn = sum(r_childnotborn, na.rm = TRUE),
      care_giver_informed_sia = sum(care_giver_informed_sia, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(numbercluster >= 2) |>
    mutate(
      percent_care_giver_informed_sia = ifelse(total_sampled > 0, round(care_giver_informed_sia / total_sampled * 100, 2), 0),
      total_missed = ifelse(total_sampled < 60, 60 - total_sampled + missed_child, missed_child),
      status = ifelse(total_missed <= 3, "Pass", "Fail"),
      performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      ),
      tot_r = r_non_compliance + r_house_not_visited + r_childabsent + r_security + r_childnotborn,
      other_r = pmax(total_missed - tot_r, 0),
      prct_r_non_compliance = ifelse(total_missed > 0, round(r_non_compliance / total_missed * 100, 2), 0),
      prct_r_house_not_visited = ifelse(total_missed > 0, round(r_house_not_visited / total_missed * 100, 2), 0),
      prct_r_childabsent = ifelse(total_missed > 0, round(r_childabsent / total_missed * 100, 2), 0),
      prct_r_childnotborn = ifelse(total_missed > 0, round(r_childnotborn / total_missed * 100, 2), 0),
      prct_r_security = ifelse(total_missed > 0, round(r_security / total_missed * 100, 2), 0),
      prct_other_r = ifelse(total_missed > 0, round(other_r / total_missed * 100, 2), 0)
    ) |>
    mutate(
      start_date = case_when(
        response == "OBR_name" & year(start_date) == 2025 & month(start_date) == 2 & roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "OBR_name" & year(start_date) == 2025 & month(start_date) == 3 & roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "NIE-2025-04-01_nOPV_NIDs" & year(start_date) == 2025 & month(start_date) == 5 & roundNumber == "Rnd1" ~ as_date("2025-04-26"),
        response == "NIE-2025-04-01_nOPV_NIDs" & year(start_date) == 2025 & month(start_date) == 7 & roundNumber == "Rnd2" ~ as_date("2025-06-14"),
        TRUE ~ start_date
      )
    )

  # Lookup table join
  prep_data_file <- "data/lookup/lqas_lookup.xlsx"
  if (file.exists(prep_data_file)) {
    prep_data <- read_excel(prep_data_file) |>
      filter(Country == "NIGERIA") |>
      mutate(
        `Round Number` = case_when(
          `Round Number` == "Round 0" ~ "Rnd0",
          `Round Number` == "Round 1" ~ "Rnd1",
          `Round Number` == "Round 2" ~ "Rnd2",
          `Round Number` == "Round 3" ~ "Rnd3",
          `Round Number` == "Round 4" ~ "Rnd4",
          `Round Number` == "Round 5" ~ "Rnd5",
          `Round Number` == "Round 6" ~ "Rnd6",
          TRUE ~ `Round Number`
        )
      )

    prep_data <- prep_data |>
      rename(
        response = `OBR Name`,
        vaccine.type = Vaccines,
        roundNumber = `Round Number`
      ) |>
      mutate(
        round_start_date = as_date(`Round Start Date`),
        start_date = round_start_date + 4,
        end_date = as_date(start_date) + 1
      ) |>
      select(response, vaccine.type, roundNumber, round_start_date, start_date, end_date)

    lookup_table <- as_tibble(prep_data) |>
      mutate(
        start_date = as_date(start_date),
        end_date = as_date(end_date),
        round_start_date = as_date(round_start_date)
      )

    # Clean join keys
    df <- df %>%
      mutate(
        response = trimws(as.character(response)),
        vaccine.type = trimws(as.character(vaccine.type)),
        roundNumber = trimws(as.character(roundNumber))
      )
    lookup_table <- lookup_table %>%
      mutate(
        response = trimws(as.character(response)),
        vaccine.type = trimws(as.character(vaccine.type)),
        roundNumber = trimws(as.character(roundNumber))
      )

    df <- df |>
      left_join(lookup_table, by = c("response", "vaccine.type", "roundNumber")) |>
      mutate(
        start_date = coalesce(start_date.y, as_date(start_date.x)),
        end_date = as_date(start_date) + 1,
        round_start_date = coalesce(round_start_date, start_date - days(4))
      ) |>
      select(-start_date.x, -start_date.y, -end_date.x, -end_date.y) |>
      filter(!is.na(District))
  }

  # Special districts vaccine.type rule
  districts_special <- c("YUSUFARI", "GURI", "BIRINIWA", "KIRI KASAMA", "NGURU", "MACHINA", "KARASUWA", "BARDE")
  province_special <- c("Adamawa", "Bauchi", "Borno", "Jigawa", "Kano", "Yobe")

  result <- df |>
    mutate(vaccine.type = case_when(
      District %in% districts_special & response == "NIE-2025-04-01_nOPV_NIDs" ~ "nOPV2 & bOPV",
      Region %in% province_special & response == "NIE-2025-10-01_nOPV_sNID" ~ "nOPV2 & bOPV",
      TRUE ~ vaccine.type
    )) |>
    select(
      country, province = Region, district = District, response, vaccine.type,
      roundNumber, numbercluster, round_start_date, start_date, end_date, year,
      male_sampled, female_sampled, total_sampled,
      male_vaccinated, female_vaccinated, total_vaccinated, missed_child,
      r_non_compliance, r_house_not_visited, r_childabsent, r_security, r_childnotborn,
      care_giver_informed_sia, percent_care_giver_informed_sia, total_missed, status, performance,
      tot_r, other_r, prct_r_non_compliance, prct_r_house_not_visited, prct_r_childabsent,
      prct_r_childnotborn, prct_r_security, prct_other_r
    )

  log_info("    Processed Nigeria CSV: {nrow(result)} rows")

  return(result)
}

# ============================================================
# Smart FM_Child Harmonizer (Handles all form types dynamically)
# ============================================================
update_fm_child_dynamic <- function(AC) {
  # Find all child indices from any FM_Child related columns
  idx_from_any <- names(AC) |>
    str_match("^Count_HH\\[(\\d+)\\]/FM_Child(R|L)?$") |>
    (\(m) m[, 2])() |>
    na.omit() |>
    unique() |>
    as.integer() |>
    sort()

  if (length(idx_from_any) == 0) return(AC)

  # Detect which column types have actual data (non-NA, non-empty)
  detect_column_with_data <- function(pattern) {
    cols <- names(AC)[grepl(pattern, names(AC))]
    if (length(cols) == 0) return(FALSE)
    # Check if any column has at least one non-NA, non-empty value
    any(sapply(cols, function(col) {
      vals <- AC[[col]]
      if (is.character(vals)) {
        any(!is.na(vals) & vals != "" & vals != " ")
      } else {
        any(!is.na(vals))
      }
    }))
  }

  has_R <- detect_column_with_data("^Count_HH\\[\\d+\\]/FM_ChildR$")
  has_L <- detect_column_with_data("^Count_HH\\[\\d+\\]/FM_ChildL$")
  has_FM <- detect_column_with_data("^Count_HH\\[\\d+\\]/FM_Child$")

  # Also check for the nested pattern (Count_HH/Count_HH/FM_Child)
  has_nested_FM <- detect_column_with_data("^Count_HH\\[\\d+\\]/Count_HH/FM_Child$")
  has_nested_R <- detect_column_with_data("^Count_HH\\[\\d+\\]/Count_HH/FM_ChildR$")
  has_nested_L <- detect_column_with_data("^Count_HH\\[\\d+\\]/Count_HH/FM_ChildL$")

  # Use nested if they have data and standard don't
  if (!has_FM && has_nested_FM) has_FM <- TRUE
  if (!has_R && has_nested_R) has_R <- TRUE
  if (!has_L && has_nested_L) has_L <- TRUE

  # Also update column names if using nested pattern
  if (has_nested_FM && !has_FM) {
    nested_cols <- names(AC)[grepl("^Count_HH\\[\\d+\\]/Count_HH/FM_Child$", names(AC))]
    for (old in nested_cols) {
      new <- gsub("/Count_HH/", "/", old)
      setnames(AC, old, new)
    }
  }
  if (has_nested_R && !has_R) {
    nested_cols <- names(AC)[grepl("^Count_HH\\[\\d+\\]/Count_HH/FM_ChildR$", names(AC))]
    for (old in nested_cols) {
      new <- gsub("/Count_HH/", "/", old)
      setnames(AC, old, new)
    }
    has_R <- TRUE
  }
  if (has_nested_L && !has_L) {
    nested_cols <- names(AC)[grepl("^Count_HH\\[\\d+\\]/Count_HH/FM_ChildL$", names(AC))]
    for (old in nested_cols) {
      new <- gsub("/Count_HH/", "/", old)
      setnames(AC, old, new)
    }
    has_L <- TRUE
  }

  log_info("    Data detection: has_FM={has_FM}, has_R={has_R}, has_L={has_L}")

  # Function to safely convert any value to binary (0/1)
  to_binary <- function(x) {
    if (is.numeric(x)) {
      return(ifelse(x == 1, 1L, ifelse(x == 0, 0L, NA_integer_)))
    }
    x_char <- as.character(x)
    x_char <- stringr::str_trim(x_char)
    x_char <- stringr::str_to_lower(x_char)

    # Handle empty/NA
    if (is.na(x_char) || x_char == "" || x_char == " ") return(NA_integer_)

    # True values
    if (x_char %in% c("1", "yes", "y", "true", "t", "oui", "o")) return(1L)
    # False values
    if (x_char %in% c("0", "no", "n", "false", "f", "non")) return(0L)

    return(NA_integer_)
  }

  mutate_list <- list()

  for (ii in idx_from_any) {
    col_FM <- sprintf("Count_HH[%d]/FM_Child", ii)
    col_R  <- sprintf("Count_HH[%d]/FM_ChildR", ii)
    col_L  <- sprintf("Count_HH[%d]/FM_ChildL", ii)

    # Ensure columns exist in AC
    col_FM_exists <- col_FM %in% names(AC)
    col_R_exists <- col_R %in% names(AC)
    col_L_exists <- col_L %in% names(AC)

    # Case 1: We have L column with data (nOPV2 - priority)
    if (has_L && col_L_exists) {
      # Convert L to binary
      if (!is.numeric(AC[[col_L]])) {
        AC[[col_L]] <- sapply(AC[[col_L]], to_binary)
      }
      AC[[col_L]] <- ifelse(is.na(AC[[col_L]]), 0L, AC[[col_L]])

      unified_value <- expr(!!sym(col_L))

      if (col_FM_exists) {
        mutate_list[[col_FM]] <- unified_value
      } else {
        mutate_list[[col_FM]] <- unified_value
      }
    }
    # Case 2: We have R column with data (bOPV only) and no L data
    else if (has_R && col_R_exists && !has_L) {
      if (!is.numeric(AC[[col_R]])) {
        AC[[col_R]] <- sapply(AC[[col_R]], to_binary)
      }
      AC[[col_R]] <- ifelse(is.na(AC[[col_R]]), 0L, AC[[col_R]])

      unified_value <- expr(!!sym(col_R))

      if (col_FM_exists) {
        mutate_list[[col_FM]] <- unified_value
      } else {
        mutate_list[[col_FM]] <- unified_value
      }
    }
    # Case 3: We have both R and L with data - priority to L (nOPV2)
    else if (has_R && has_L && col_R_exists && col_L_exists) {
      if (!is.numeric(AC[[col_R]])) {
        AC[[col_R]] <- sapply(AC[[col_R]], to_binary)
      }
      if (!is.numeric(AC[[col_L]])) {
        AC[[col_L]] <- sapply(AC[[col_L]], to_binary)
      }
      AC[[col_R]] <- ifelse(is.na(AC[[col_R]]), 0L, AC[[col_R]])
      AC[[col_L]] <- ifelse(is.na(AC[[col_L]]), 0L, AC[[col_L]])

      # Priority: L (nOPV2) over R (bOPV)
      unified_value <- expr(
        case_when(
          !!sym(col_L) == 1 ~ 1L,
          !!sym(col_R) == 1 ~ 1L,
          TRUE ~ 0L
        )
      )

      if (col_FM_exists) {
        mutate_list[[col_FM]] <- unified_value
      } else {
        mutate_list[[col_FM]] <- unified_value
      }
    }
    # Case 4: Only old FM_Child exists with data
    else if (has_FM && col_FM_exists) {
      if (!is.numeric(AC[[col_FM]])) {
        AC[[col_FM]] <- sapply(AC[[col_FM]], to_binary)
      }
      AC[[col_FM]] <- ifelse(is.na(AC[[col_FM]]), 0L, AC[[col_FM]])
      # No mutation needed, column already exists with correct values
      next
    }
  }

  if (length(mutate_list) == 0) {
    return(AC)
  }

  result <- AC %>% mutate(!!!mutate_list)

  # Log the results for debugging
  if (length(mutate_list) > 0) {
    sample_col <- names(mutate_list)[1]
    if (nrow(result) > 0 && sample_col %in% names(result)) {
      vaccinated_count <- sum(result[[sample_col]] == 1, na.rm = TRUE)
      not_vaccinated_count <- sum(result[[sample_col]] == 0, na.rm = TRUE)
      total_non_na <- vaccinated_count + not_vaccinated_count
      if (total_non_na > 0) {
        log_info("    Unified FM_Child from available data: vaccinated={vaccinated_count}, not_vaccinated={not_vaccinated_count}")
      } else {
        log_warn("    WARNING: No valid vaccination data found after unification!")
      }
    }
  }

  return(result)
}


# ============================================================
# Process Regular LQAS File (EXACT from original)
# ============================================================

process_regular_file <- function(file_path, file_name) {

  # Read parquet file
  data <- tryCatch({
    as.data.table(read_parquet(file_path))
  }, error = function(e) {
    log_warn("    Failed to read {file_name}: {e$message}")
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    log_warn("    Empty data in {file_name}")
    return(NULL)
  }

  log_info("    Initial data: {nrow(data)} rows, {ncol(data)} columns")

  # EXACT original processing steps
  data <- rename_repetitive_columns(data)
  data <- apply_custom_rules(data, file_name)

  # Select columns (EXACT from original)
  selected_columns <- c(
    "Response", "roundNumber", "Country", "Region", "District", "Date_of_LQAS",
    "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_altitude",
    grep("^Count_HH\\[\\d+\\]/(Sex_Child|FM_Child|FM_ChildR|FM_ChildL|Reason_Not_FM|Reason_NC_NFM|Reason_ABS_NFM|care_giver_informed_sia)$",
         names(data), value = TRUE),
    "Count_HH_count", "Cluster"
  )
  selected_columns <- intersect(selected_columns, names(data))
  data <- data %>% select(all_of(selected_columns))

  # Standardize data (EXACT from original)
  standardize_yes_no <- function(x) {
    case_when(
      x %in% c("Yes", "YES", "yes", "Y") ~ 1,
      x %in% c("No", "NO", "no", "N") ~ 0,
      TRUE ~ NA_real_
    )
  }

  standardize_informed_sia <- function(x) {
    case_when(
      x %in% c("Y", "1") ~ 1,
      x %in% c("N", "0") ~ 0,
      TRUE ~ NA_real_
    )
  }

  data <- data %>%
    mutate(
      Country = str_squish(toupper(Country)),
      Region = str_squish(toupper(Region)),
      District = str_squish(toupper(District))
    ) %>%
    mutate(
      across(matches("^Count_HH\\[\\d+\\]/Sex_Child$"),
             ~ ifelse(. == "F", 1, ifelse(. == "M", 0, .))),
      across(matches("^Count_HH\\[\\d+\\]/FM_Child$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/FM_ChildR$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/FM_ChildL$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/care_giver_informed_sia$"), standardize_informed_sia),
      across(matches("^Count_HH\\[\\d+\\]/Reason_ABS_NFM$"), map_abs_reason),
      across(matches("^Count_HH\\[\\d+\\]/Reason_NC_NFM$"), map_nc_reason)
    )

  # Apply FM Child harmonizer (handles all form types dynamically)
  data <- update_fm_child_dynamic(data)

  # Get actual child indices from the data (don't assume 1-10)
  child_idx <- names(data) |>
    str_match("^Count_HH\\[(\\d+)\\]/") |>
    (\(m) m[, 2])() |>
    na.omit() |>
    unique() |>
    as.integer() |>
    sort()

  log_info("    Detected child indices: {paste(child_idx, collapse=', ')}")

  # If no indices found, try alternative pattern (some forms use underscores)
  if (length(child_idx) == 0) {
    child_idx <- names(data) |>
      str_match("^Count_HH_(\\d+)_") |>
      (\(m) m[, 2])() |>
      na.omit() |>
      unique() |>
      as.integer() |>
      sort()
    log_info("    Alternative pattern detected child indices: {paste(child_idx, collapse=', ')}")
  }

  # If still no indices, log warning and return
  if (length(child_idx) == 0) {
    log_warn("    No child indices found in column names!")
    return(NULL)
  }

  # Build column lists using detected indices
  sex_cols <- intersect(sprintf("Count_HH[%d]/Sex_Child", child_idx), names(data))
  fm_cols <- intersect(sprintf("Count_HH[%d]/FM_Child", child_idx), names(data))
  fmr_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildR", child_idx), names(data))
  fml_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildL", child_idx), names(data))
  cgs_cols <- intersect(sprintf("Count_HH[%d]/care_giver_informed_sia", child_idx), names(data))
  abs_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_ABS_NFM", child_idx), names(data))
  nc_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_NC_NFM", child_idx), names(data))

  log_info("    Found {length(sex_cols)} sex columns, {length(fm_cols)} FM_Child columns")

  # If no FM_Child columns found, log warning
  if (length(fm_cols) == 0) {
    log_warn("    No FM_Child columns found! Vaccination data will be missing.")
  }

  # Reason_Not_FM logic
  data <- data %>%
    mutate(
      across(
        matches("^Count_HH\\[\\d+\\]/Reason_Not_FM$"),
        .fns = list(
          House_not_visited = ~ as.numeric(. == "House_not_visited"),
          childabsent = ~ as.numeric(. == "childabsent"),
          Vaccinated_but_not_FM = ~ as.numeric(. == "Vaccinated_but_not_FM"),
          Non_Compliance = ~ as.numeric(. == "Non_Compliance"),
          Child_was_asleep = ~ as.numeric(. == "Child_was_asleep"),
          Child_is_a_visitor = ~ as.numeric(. == "Child_is_a_visitor")
        ),
        .names = "R_{.fn}_{col}"
      )
    )

  # Convert numeric columns
  numeric_count_cols <- c(sex_cols, fm_cols, fmr_cols, fml_cols, cgs_cols, "Count_HH_count", "Cluster")
  numeric_count_cols <- intersect(numeric_count_cols, names(data))

  data <- data %>%
    mutate(across(
      all_of(numeric_count_cols),
      ~ as.numeric(replace(., . %in% c(".", "NA", ""), NA))
    ))

  # ALG response / round fixes
  data <- data %>%
    mutate(Date_of_LQAS = as.Date(Date_of_LQAS)) %>%
    mutate(
      Response = if_else(
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-11-30"), as.Date("2026-01-06")),
        "ALG-2025-09-01_nOPV_NID",
        Response
      ),
      roundNumber = case_when(
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-11-30"), as.Date("2025-12-13")) ~ "rnd1",
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-12-31"), as.Date("2026-01-06")) ~ "rnd2",
        TRUE ~ roundNumber
      )
    )

  # Build reason wide tables
  absent_reason_wide <- build_reason_wide(data, abs_reason_cols, "abs_reason_")
  noncomp_reason_wide <- build_reason_wide(data, nc_reason_cols, "nc_reason_")

  # Calculate metrics using detected columns
  if (length(sex_cols) > 0 && length(fm_cols) > 0) {
    AF <- data %>%
      relocate(all_of(sex_cols), .after = "_GPS_hh_altitude") %>%
      relocate(all_of(fm_cols), .after = last(sex_cols)) %>%
      mutate(
        female_sampled = rowSums(across(all_of(sex_cols)), na.rm = TRUE),
        male_sampled = Count_HH_count - female_sampled,
        total_vaccinated = rowSums(across(all_of(fm_cols)), na.rm = TRUE),
        missed_child = Count_HH_count - total_vaccinated
      )
  } else {
    # Handle case where columns are missing
    AF <- data %>%
      mutate(
        female_sampled = 0,
        male_sampled = Count_HH_count,
        total_vaccinated = 0,
        missed_child = Count_HH_count
      )
    log_warn("    Missing sex or FM_Child columns - using defaults")
  }

  # Calculate female vaccinated
  if (length(sex_cols) > 0 && length(fm_cols) > 0) {
    AG <- AF %>%
      mutate(
        across(
          .cols = all_of(sex_cols),
          .fns = ~ ifelse(
            . + get(str_replace(cur_column(), "Sex_Child", "FM_Child")) >= 2,
            1,
            0
          ),
          .names = "FV{gsub('[^0-9]', '', .col)}"
        ),
        female_vaccinated = rowSums(across(starts_with("FV")), na.rm = TRUE),
        male_vaccinated = total_vaccinated - female_vaccinated
      )
  } else {
    AG <- AF %>%
      mutate(
        female_vaccinated = 0,
        male_vaccinated = 0
      )
  }

  AG <- AG %>% mutate(across(starts_with("R_"), as.numeric))

  AS <- AG %>%
    mutate(
      r_house_not_visited = rowSums(across(matches("^r_house_not_visited_Count_HH"), ~ replace_na(., 0))),
      R_Vaccinated_but_not_FM = rowSums(across(matches("^R_Vaccinated_but_not_FM_Count_HH"), ~ replace_na(., 0))),
      r_non_compliance = rowSums(across(matches("^r_non_compliance_Count_HH"), ~ replace_na(., 0))),
      R_Child_was_asleep = rowSums(across(matches("^R_Child_was_asleep_Count_HH"), ~ replace_na(., 0))),
      R_Child_is_a_visitor = rowSums(across(matches("^R_Child_is_a_visitor_Count_HH"), ~ replace_na(., 0))),
      r_childabsent = rowSums(across(matches("^r_childabsent_Count_HH"), ~ replace_na(., 0))),
      care_giver_informed_sia = rowSums(across(all_of(cgs_cols), ~ replace_na(., 0)))
    )

  AQ <- AS %>%
    select(
      Country, Region, District, Response, roundNumber, Date_of_LQAS,
      male_sampled, female_sampled,
      total_sampled = Count_HH_count,
      male_vaccinated, female_vaccinated, total_vaccinated, missed_child,
      r_non_compliance, r_house_not_visited, r_childabsent, R_Child_was_asleep,
      R_Child_is_a_visitor, R_Vaccinated_but_not_FM, care_giver_informed_sia,
      Cluster
    ) %>%
    mutate(Cluster = as.numeric(Cluster))

  # Aggregate to district level
  F1 <- AQ %>%
    mutate(Date_of_LQAS = as_date(Date_of_LQAS)) %>%
    group_by(Country, Region, District, Response, roundNumber) %>%
    summarise(
      start_date = min(Date_of_LQAS, na.rm = TRUE),
      end_date = max(Date_of_LQAS, na.rm = TRUE),
      cluster = sum(Cluster, na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_non_compliance = sum(r_non_compliance, na.rm = TRUE),
      r_house_not_visited = sum(r_house_not_visited, na.rm = TRUE),
      r_childabsent = sum(r_childabsent, na.rm = TRUE),
      r_Child_was_asleep = sum(R_Child_was_asleep, na.rm = TRUE),
      r_Child_is_a_visitor = sum(R_Child_is_a_visitor, na.rm = TRUE),
      r_Vaccinated_but_not_FM = sum(R_Vaccinated_but_not_FM, na.rm = TRUE),
      care_giver_informed_sia = sum(care_giver_informed_sia, na.rm = TRUE),
      percent_care_giver_informed_sia = care_giver_informed_sia / total_sampled,
      .groups = "drop"
    ) %>%
    left_join(absent_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber")) %>%
    left_join(noncomp_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber"))

  # Fill NA reasons with 0
  reason_count_cols <- names(F1)[str_detect(names(F1), "^abs_reason_|^nc_reason_")]
  if (length(reason_count_cols) > 0) {
    F1 <- F1 %>%
      mutate(across(all_of(reason_count_cols), ~ replace_na(., 0)))
  }

  # Calculate final metrics
  F2 <- F1 %>%
    filter(start_date > as.Date("2019-10-01")) %>%
    mutate(
      percent_care_giver_informed_sia = round(percent_care_giver_informed_sia * 100, 2),
      total_missed = ifelse(total_sampled < 60, (60 - total_sampled) + missed_child, missed_child)
    ) %>%
    filter(cluster >= 3) %>%
    mutate(
      roundNumber = toupper(roundNumber),
      roundNumber = case_when(
        str_detect(roundNumber, "0") ~ "Rnd0",
        str_detect(roundNumber, "1") ~ "Rnd1",
        str_detect(roundNumber, "2") ~ "Rnd2",
        str_detect(roundNumber, "3") ~ "Rnd3",
        str_detect(roundNumber, "4") ~ "Rnd4",
        str_detect(roundNumber, "5") ~ "Rnd5",
        str_detect(roundNumber, "6") ~ "Rnd6",
        TRUE ~ roundNumber
      ),
      Status = case_when(
        total_missed <= 3 ~ "Pass",
        TRUE ~ "Fail"
      ),
      Performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      )
    )

  # Add vaccine type
  F3 <- F2 %>%
    mutate(
      Vaccine.type = case_when(
        str_detect(Response, "BITTOU|MENAKA-mOPV2|BAMAKO-mOPV2|KANKAN-mOPV|MLI-12DS-01-2021-mOPV2|CONAKRY-mOPV|Ouagadogou|Bangui 1|GOTHEY|YOPOUGON|Golfe|MDG-2023-03-01_bOPV|BEN-xxDS-02-2020|BEN-26DS-08-2020|Chavuma-mOPV|Luapula-mOPV") ~ "mOPV",
        str_detect(Response, "nOPV|VPOn|TSHUAPA|Tanganyika|Liberia|Mauritania|KOUIBLY|Sierra Leone|SEN|CEN|MAL|BEN-39DS-01-2021|BERTOUA|EBOLOWA|EXNORD|ExtNord2023|ADDIS ABABA|Mekelle|AMANSIE SOUTH|CAF-2020-002|CENBLOCK|CENTRALBLK|CHA-17DS-02-2020|DONOMANGA|GNBnOPV|GOLFE|GOTHEYE|KEN-13DS-02-2021|MopUp2022|SSD-79DS-09-2020|ALG-2023-09-01_nOPV|ALG-2024-01-01_nOPV|nOPV2022|BEN-2023-09-01_nOPV|BFA-2023-05-01_nOPV|BFA-2023-09-01_nOPV|BFA-2024-02-01_nOPV|BITTOU-mOPV2|Ouagadogou-mOPV2|BOT-2023-02-01_nOPV|CAM-2023-05-01_nOPV|CAM-2023-08-01_nOPV|CAM-2024-02-01_nOPV|nOPV2022|nOPV2023|nVPO|nVPO_Maradi|nVPO_Zinder|nVPO2|May2021|OPVb2021|OPVb2022|RSSmOPV10C2021|SEN_VPOn|UGAnOPV|VPOb|VPOb13ProV|n_OPV") ~ "nOPV2",
        str_detect(Response, "BOPV|bOPV|OPVb|WPV1") ~ "bOPV",
        TRUE ~ NA_character_
      )
    )

  # Load lookup table for campaign dates
  lookup_file <- "data/lookup/lqas_lookup.xlsx"
  if (file.exists(lookup_file)) {
    date_lookup <- read_excel(lookup_file) %>%
      mutate(
        `Round Number` = case_when(
          `Round Number` == "Round 0" ~ "Rnd0",
          `Round Number` == "Round 1" ~ "Rnd1",
          `Round Number` == "Round 2" ~ "Rnd2",
          `Round Number` == "Round 3" ~ "Rnd3",
          `Round Number` == "Round 4" ~ "Rnd4",
          `Round Number` == "Round 5" ~ "Rnd5",
          `Round Number` == "Round 6" ~ "Rnd6",
          TRUE ~ `Round Number`
        )
      ) %>%
      rename(
        Response = `OBR Name`,
        Vaccine.type_lookup = Vaccines,
        roundNumber = `Round Number`,
        round_start_date = `Round Start Date`
      ) %>%
      mutate(
        start_date = as.Date(round_start_date) + 4,
        end_date = start_date + 1
      ) %>%
      select(Response, Vaccine.type_lookup, roundNumber, round_start_date, start_date, end_date)

    F3 <- F3 %>%
      left_join(date_lookup, by = c("Response", "roundNumber")) %>%
      mutate(
        start_date = coalesce(start_date.y, start_date.x),
        end_date = coalesce(end_date.y, end_date.x),
        round_start_date = coalesce(round_start_date, start_date - 4)
      ) %>%
      select(-start_date.x, -start_date.y, -end_date.x, -end_date.y, -Vaccine.type_lookup)
  }

  # Final formatting
  F4 <- F3 %>%
    mutate(
      tot_r = r_non_compliance + r_house_not_visited + r_childabsent +
        r_Child_was_asleep + r_Child_is_a_visitor + r_Vaccinated_but_not_FM,
      other_r = ifelse((total_missed - tot_r) < 0, 0, (total_missed - tot_r)),
      Country = case_when(
        Country == "DRC" ~ "RDC",
        Country == "Camerooun" ~ "CAE",
        Country == "CAMEROON" ~ "CAE",
        Country == "BURKINA_FASO" ~ "BFA",
        Country == "Ethiopia" ~ "ETH",
        Country == "ZAMBIA" ~ "ZMB",
        Country == "BENIN" ~ "BEN",
        Country == "CHAD" ~ "CHD",
        TRUE ~ Country
      )
    ) %>%
    select(
      country = Country,
      province = Region,
      district = District,
      response = Response,
      vaccine.type = Vaccine.type,
      roundNumber,
      numbercluster = cluster,
      round_start_date,
      start_date,
      end_date,
      male_sampled,
      female_sampled,
      total_sampled,
      male_vaccinated,
      female_vaccinated,
      total_vaccinated,
      total_missed,
      status = Status,
      performance = Performance,
      r_non_compliance,
      r_house_not_visited,
      r_childabsent,
      r_Child_was_asleep,
      r_Child_is_a_visitor,
      r_Vaccinated_but_not_FM,
      other_r,
      percent_care_giver_informed_sia,
      starts_with("abs_reason_"),
      starts_with("nc_reason_")
    ) %>%
    mutate(
      percent_care_giver_informed_sia = round(percent_care_giver_informed_sia, 2),
      across(c(r_non_compliance, r_house_not_visited, r_childabsent,
               r_Child_was_asleep, r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r),
             ~ ifelse(total_missed == 0, 0, (.x / total_missed) * 100),
             .names = "prct_{.col}"),
      across(starts_with("prct_"), ~ round(.x, 2)),
      proportion_missed_child = round(total_missed / total_sampled, 2)
    )

  log_info("    Processed regular file: {nrow(F4)} aggregated rows")

  # Debug: Log sample of results
  if (nrow(F4) > 0) {
    log_info("    Sample output - first 3 districts:")
    sample_out <- F4[1:min(3, nrow(F4)), c("country", "province", "district", "total_sampled", "total_vaccinated", "total_missed")]
    for (j in 1:nrow(sample_out)) {
      log_info("      {sample_out$district[j]}: sampled={sample_out$total_sampled[j]}, vaccinated={sample_out$total_vaccinated[j]}, missed={sample_out$total_missed[j]}")
    }
  }

  return(F4)
}

# ============================================================
# Main Processing Function
# ============================================================

process_lqas_data <- function(force_full_run = FALSE) {

  log_info("=" %>% paste(rep("=", 60), collapse = ""))
  log_info("PROCESSING LQAS DATA (Mirroring Original)")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  # List all Parquet files
  parquet_files <- list.files("data/raw", pattern = "\\.parquet$", full.names = TRUE)

  # Also look for special case files
  nigeria_csv <- "data/raw/Nigeria_LQAS_int_oct_2025.csv"
  form_272 <- "data/raw/272.parquet"

  all_files <- parquet_files

  if (file.exists(nigeria_csv)) {
    all_files <- c(all_files, nigeria_csv)
    log_info("Found special case: Nigeria CSV")
  }

  if (file.exists(form_272)) {
    all_files <- c(all_files, form_272)
    log_info("Found special case: 272.parquet")
  }

  if (length(all_files) == 0) {
    log_error("No files found in data/raw/")
    log_info("Please run: python fetch_ona_data.py --force-full")
    return(NULL)
  }

  log_info("Found {length(all_files)} total files to process")

  # Process each file individually
  all_results <- list()
  processed_count <- 0
  failed_count <- 0
  special_count <- 0

  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)

    log_info("\n--- Processing file {i}/{length(all_files)}: {file_name} ---")

    # Detect explicit special cases first
    is_272 <- grepl("^272\\.", file_name) || grepl("272", file_name)
    is_nigeria <- grepl("Nigeria.*\\.csv$", file_name)

    result <- NULL

    if (is_272) {
      result <- tryCatch({
        special_count <- special_count + 1
        process_special_272(file_path, file_name)
      }, error = function(e) {
        log_error("    Special case 272 failed: {e$message}")
        return(NULL)
      })
    } else if (is_nigeria) {
      result <- tryCatch({
        special_count <- special_count + 1
        process_special_nigeria(file_path, file_name)
      }, error = function(e) {
        log_error("    Special case Nigeria failed: {e$message}")
        return(NULL)
      })
    } else {
      # For all other files, auto-detect the format
      result <- tryCatch({
        detect_and_process_file(file_path, file_name)
      }, error = function(e) {
        log_error("    Processing failed: {e$message}")
        traceback()
        return(NULL)
      })
    }

    if (!is.null(result) && nrow(result) > 0) {
      # Save individual file output (mirroring original)
      individual_output <- file.path("data/processed", paste0(tools::file_path_sans_ext(file_name), ".csv"))
      fwrite(result, individual_output)
      log_info("    ✅ Saved individual output to {individual_output}")

      all_results[[file_name]] <- result
      processed_count <- processed_count + 1
      log_info("    ✅ Successfully processed {file_name}")
    } else {
      failed_count <- failed_count + 1
      log_warn("    ❌ Failed to process {file_name}")
    }
  }

  # Combine all results
  log_info("\n" %>% paste(rep("=", 60), collapse = ""))
  log_info("COMBINING RESULTS")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  if (length(all_results) == 0) {
    log_error("No files were successfully processed")
    return(NULL)
  }

  combined <- bind_rows(all_results)
  log_info("Combined {nrow(combined)} rows from {length(all_results)} files")
  log_info("Regular files: {processed_count - special_count}, Special cases: {special_count}, Failed: {failed_count}")

  # Remove duplicates
  combined <- combined %>%
    distinct(country, province, district, response, roundNumber, .keep_all = TRUE)

  log_info("After deduplication: {nrow(combined)} rows")

  # ============================================================
  # Save Outputs with Forced Write
  # ============================================================
  log_info("\n" %>% paste(rep("=", 60), collapse = ""))
  log_info("SAVING OUTPUTS")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  # Force write function for CSV
  force_write_csv <- function(data, file_path) {
    temp_file <- paste0(file_path, ".tmp", format(Sys.time(), "%Y%m%d%H%M%S"))
    tryCatch({
      fwrite(data, temp_file)

      # Remove original if it exists
      if (file.exists(file_path)) {
        # Try to change permissions
        tryCatch({
          Sys.chmod(file_path, mode = "0777")
        }, error = function(e) {})

        # Force delete with retry
        for (i in 1:3) {
          unlink_result <- tryCatch({
            unlink(file_path, force = TRUE)
            TRUE
          }, error = function(e) {
            FALSE
          })

          if (unlink_result || !file.exists(file_path)) break
          Sys.sleep(0.5)
        }
      }

      # Rename temp file to target
      file.rename(temp_file, file_path)

      # Verify
      if (file.exists(file_path)) {
        return(TRUE)
      } else {
        return(FALSE)
      }

    }, error = function(e) {
      log_error("Error writing CSV: {e$message}")
      return(FALSE)
    })
  }

  # Force write function for Parquet
  force_write_parquet <- function(data, file_path) {
    temp_file <- paste0(file_path, ".tmp", format(Sys.time(), "%Y%m%d%H%M%S"))
    tryCatch({
      write_parquet(data, temp_file)

      # Remove original if it exists
      if (file.exists(file_path)) {
        # Try to change permissions
        tryCatch({
          Sys.chmod(file_path, mode = "0777")
        }, error = function(e) {})

        # Force delete with retry
        for (i in 1:3) {
          unlink_result <- tryCatch({
            unlink(file_path, force = TRUE)
            TRUE
          }, error = function(e) {
            FALSE
          })

          if (unlink_result || !file.exists(file_path)) break
          Sys.sleep(0.5)
        }
      }

      # Rename temp file to target
      file.rename(temp_file, file_path)

      # Verify
      if (file.exists(file_path)) {
        return(TRUE)
      } else {
        return(FALSE)
      }

    }, error = function(e) {
      log_error("Error writing Parquet: {e$message}")
      return(FALSE)
    })
  }

  # Save final CSV for dashboard
  final_csv <- "data/final/lqas_dashboard_input.csv"
  if (force_write_csv(combined, final_csv)) {
    log_info("✅ Saved final data to {final_csv}")
  } else {
    log_error("❌ Failed to save {final_csv}")
  }

  # Save as Parquet for faster loading
  final_parquet <- "data/final/lqas_dashboard_input.parquet"
  if (force_write_parquet(combined, final_parquet)) {
    log_info("✅ Saved parquet to {final_parquet}")
  } else {
    log_error("❌ Failed to save {final_parquet}")
  }

  # Save summary (simple save, less likely to be locked)
  summary_file <- "data/processed/processing_summary.rds"
  summary <- list(
    timestamp = Sys.time(),
    total_records = nrow(combined),
    total_columns = ncol(combined),
    regular_files_processed = processed_count - special_count,
    special_cases_processed = special_count,
    files_failed = failed_count,
    countries = unique(combined$country),
    file_size_mb = ifelse(file.exists(final_csv), file.size(final_csv) / (1024 * 1024), NA)
  )

  tryCatch({
    saveRDS(summary, summary_file)
    log_info("✅ Saved processing summary to {summary_file}")
  }, error = function(e) {
    # Try with temp file for RDS as well
    temp_rds <- paste0(summary_file, ".tmp")
    saveRDS(summary, temp_rds)
    if (file.exists(summary_file)) {
      unlink(summary_file, force = TRUE)
    }
    file.rename(temp_rds, summary_file)
    log_info("✅ Saved processing summary to {summary_file} (forced)")
  })

  log_info("\n" %>% paste(rep("=", 60), collapse = ""))
  log_info("PROCESSING COMPLETE!")
  log_info("Total records: {nrow(combined)}")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  return(combined)
}

# ============================================================
# Helper operator for null coalescing
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# Run Main Function
# ============================================================

if (interactive()) {
  process_lqas_data(force_full_run = FALSE)
} else {
  args <- commandArgs(trailingOnly = TRUE)
  force_full <- "--force-full" %in% args
  process_lqas_data(force_full_run = force_full)
}

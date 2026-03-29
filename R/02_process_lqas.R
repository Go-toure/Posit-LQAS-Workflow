#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Processing Script
# Complete integration of convert_padacord_LQAS_to_csv logic
# Reads Parquet files, processes, outputs CSV for dashboard
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
})

# Create directories
dir_create("logs")
dir_create("data/processed")
dir_create("data/final")

# Configure logging
log_appender(appender_file("logs/process.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting LQAS Data Processing (Full Integration)")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# ============================================================
# Helper Functions (from original script)
# ============================================================

rename_repetitive_columns <- function(dt) {
  pattern <- "^Count_HH\\[\\d+\\]/Count_HH/"
  new_names <- names(dt)
  new_names <- str_replace(new_names, pattern, "")
  new_names <- make.unique(new_names, sep = "_")
  setnames(dt, new_names, skip_absent = TRUE)
  return(dt)
}

apply_custom_rules <- function(dt, file_name) {
  form_id <- as.numeric(str_extract(file_name, "\\d+"))
  if (!is.na(form_id)) {
    if (form_id == 3583) {
      dt[, Country := "GHA"]
    }
    if (form_id == 8834) {
      dt[, Region := District]
    }
    if (form_id == 4351) {
      dt[, District := district]
    }
  }
  return(dt)
}

normalize_reason_text <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_squish(x)
  x <- str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x[x %in% c("", "na", "n_a", "n/a", "null", "none", "missing", "unknown", ".", "-", "--")] <- NA
  return(x)
}

map_abs_reason <- function(x) {
  x <- normalize_reason_text(x)
  case_when(
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
  case_when(
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
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
}

build_reason_wide <- function(data, reason_cols, names_prefix) {
  if (length(reason_cols) == 0) {
    return(NULL)
  }

  result <- data %>%
    select(Country, Region, District, Response, roundNumber, all_of(reason_cols)) %>%
    pivot_longer(
      cols = all_of(reason_cols),
      names_to = "reason_source_col",
      values_to = "reason"
    ) %>%
    filter(!is.na(reason)) %>%
    count(Country, Region, District, Response, roundNumber, reason, name = "n") %>%
    mutate(reason = make_reason_slug(reason)) %>%
    pivot_wider(
      names_from = reason,
      values_from = n,
      values_fill = 0,
      names_prefix = names_prefix
    )

  return(result)
}

# ============================================================
# Smart FM_Child Harmonizer (dynamic)
# ============================================================

update_fm_child_dynamic <- function(dt) {
  idx_from_any <- names(dt) %>%
    str_match("^Count_HH\\[(\\d+)\\]/FM_Child(R|L)?$") %>%
    .[, 2] %>%
    na.omit() %>%
    unique() %>%
    as.integer() %>%
    sort()

  if (length(idx_from_any) == 0) return(dt)

  has_R <- any(str_detect(names(dt), "^Count_HH\\[\\d+\\]/FM_ChildR$"))
  has_L <- any(str_detect(names(dt), "^Count_HH\\[\\d+\\]/FM_ChildL$"))
  has_RL <- has_R && has_L
  if (!has_RL) return(dt)

  for (ii in idx_from_any) {
    col_FM <- sprintf("Count_HH[%d]/FM_Child", ii)
    col_R <- sprintf("Count_HH[%d]/FM_ChildR", ii)
    col_L <- sprintf("Count_HH[%d]/FM_ChildL", ii)

    if (!(col_R %in% names(dt) && col_L %in% names(dt))) next

    if (col_FM %in% names(dt)) {
      dt[, (col_FM) := ifelse(get(col_R) + get(col_L) >= 1 | get(col_FM) == 1, 1, 0)]
    } else {
      dt[, (col_FM) := ifelse(get(col_R) + get(col_L) >= 1, 1, 0)]
    }
  }

  return(dt)
}

# ============================================================
# Main Processing Function
# ============================================================

process_lqas_data <- function(force_full_run = FALSE) {

  log_info("=" %>% paste(rep("=", 60), collapse = ""))
  log_info("PROCESSING LQAS DATA")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  # Read all Parquet files
  log_info("Step 1: Reading Parquet files...")
  parquet_files <- list.files("data/raw", pattern = "\\.parquet$", full.names = TRUE)

  if (length(parquet_files) == 0) {
    log_error("No Parquet files found in data/raw/")
    log_info("Please run: python fetch_ona_data.py --force-full")
    return(NULL)
  }

  log_info("Found {length(parquet_files)} Parquet files")

  # Read and combine all files
  all_data <- list()

  for (i in seq_along(parquet_files)) {
    file_path <- parquet_files[i]
    file_name <- basename(file_path)
    log_info("  [{i}/{length(parquet_files)}] Reading {file_name}...")

    dt <- tryCatch({
      as.data.table(read_parquet(file_path))
    }, error = function(e) {
      log_warn("    Failed to read {file_name}: {e$message}")
      return(NULL)
    })

    if (!is.null(dt) && nrow(dt) > 0) {
      dt <- rename_repetitive_columns(dt)
      dt <- apply_custom_rules(dt, file_name)
      all_data[[file_name]] <- dt
    }
  }

  AC <- rbindlist(all_data, fill = TRUE, use.names = TRUE)
  AC <- AC[, !duplicated(names(AC)), with = FALSE]

  log_info("Combined dataset: {nrow(AC)} rows, {ncol(AC)} columns")
  log_info("Column names: {paste(names(AC)[1:min(20, ncol(AC))], collapse=', ')}...")

  # ============================================================
  # Standardize Data
  # ============================================================
  log_info("Step 2: Standardizing data...")

  standardize_yes_no <- function(x) {
    case_when(
      x %in% c("Yes", "YES", "yes", "Y", "1", 1) ~ 1,
      x %in% c("No", "NO", "no", "N", "0", 0) ~ 0,
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

  # Safely convert columns if they exist
  if ("Country" %in% names(AC)) AC[, Country := str_squish(toupper(as.character(Country)))]
  if ("Region" %in% names(AC)) AC[, Region := str_squish(toupper(as.character(Region)))]
  if ("District" %in% names(AC)) AC[, District := str_squish(toupper(as.character(District)))]

  # Apply reason mappings
  abs_reason_cols_all <- grep("^Count_HH\\[\\d+\\]/Reason_ABS_NFM$", names(AC), value = TRUE)
  for (col in abs_reason_cols_all) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = map_abs_reason(AC[[col]]))
    }
  }

  nc_reason_cols_all <- grep("^Count_HH\\[\\d+\\]/Reason_NC_NFM$", names(AC), value = TRUE)
  for (col in nc_reason_cols_all) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = map_nc_reason(AC[[col]]))
    }
  }

  # Apply binary conversions
  for (col in grep("^Count_HH\\[\\d+\\]/FM_Child$", names(AC), value = TRUE)) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = standardize_yes_no(AC[[col]]))
    }
  }

  for (col in grep("^Count_HH\\[\\d+\\]/FM_ChildR$", names(AC), value = TRUE)) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = standardize_yes_no(AC[[col]]))
    }
  }

  for (col in grep("^Count_HH\\[\\d+\\]/FM_ChildL$", names(AC), value = TRUE)) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = standardize_yes_no(AC[[col]]))
    }
  }

  for (col in grep("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA$", names(AC), value = TRUE)) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = standardize_informed_sia(AC[[col]]))
    }
  }

  # Sex conversion
  for (col in grep("^Count_HH\\[\\d+\\]/Sex_Child$", names(AC), value = TRUE)) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = ifelse(AC[[col]] == "F", 1, ifelse(AC[[col]] == "M", 0, AC[[col]])))
    }
  }

  # Apply FM Child harmonizer
  AC <- update_fm_child_dynamic(AC)

  # Get child indices
  child_idx <- names(AC) %>%
    str_match("^Count_HH\\[(\\d+)\\]/") %>%
    .[, 2] %>%
    na.omit() %>%
    unique() %>%
    as.integer() %>%
    sort()

  sex_cols <- intersect(sprintf("Count_HH[%d]/Sex_Child", child_idx), names(AC))
  fm_cols <- intersect(sprintf("Count_HH[%d]/FM_Child", child_idx), names(AC))
  fmr_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildR", child_idx), names(AC))
  fml_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildL", child_idx), names(AC))
  cgs_cols <- intersect(sprintf("Count_HH[%d]/Care_Giver_Informed_SIA", child_idx), names(AC))
  abs_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_ABS_NFM", child_idx), names(AC))
  nc_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_NC_NFM", child_idx), names(AC))

  log_info("Found {length(sex_cols)} sex columns, {length(fm_cols)} FM columns")

  # Create Reason_Not_FM columns if they exist
  reason_not_fm_cols <- intersect(sprintf("Count_HH[%d]/Reason_Not_FM", child_idx), names(AC))
  for (col in reason_not_fm_cols) {
    if (col %in% names(AC)) {
      AC[, paste0("R_House_not_visited_", col) := as.numeric(get(col) == "House_not_visited")]
      AC[, paste0("R_childabsent_", col) := as.numeric(get(col) == "childabsent")]
      AC[, paste0("R_Vaccinated_but_not_FM_", col) := as.numeric(get(col) == "Vaccinated_but_not_FM")]
      AC[, paste0("R_Non_Compliance_", col) := as.numeric(get(col) == "Non_Compliance")]
      AC[, paste0("R_Child_was_asleep_", col) := as.numeric(get(col) == "Child_was_asleep")]
      AC[, paste0("R_Child_is_a_visitor_", col) := as.numeric(get(col) == "Child_is_a_visitor")]
    }
  }

  # Convert numeric columns
  numeric_count_cols <- c(sex_cols, fm_cols, fmr_cols, fml_cols, cgs_cols, "Count_HH_count", "Cluster")
  numeric_count_cols <- intersect(numeric_count_cols, names(AC))

  for (col in numeric_count_cols) {
    if (col %in% names(AC)) {
      set(AC, j = col, value = as.numeric(as.character(AC[[col]])))
    }
  }

  # ============================================================
  # ALG response / round fixes
  # ============================================================
  if ("Date_of_LQAS" %in% names(AC) && "Country" %in% names(AC)) {
    AC[, Date_of_LQAS := as.Date(Date_of_LQAS)]
    if ("Response" %in% names(AC)) {
      AC[, Response := ifelse(
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-11-30"), as.Date("2026-01-06")),
        "ALG-2025-09-01_nOPV_NID",
        Response
      )]
    }
    if ("roundNumber" %in% names(AC)) {
      AC[, roundNumber := case_when(
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-11-30"), as.Date("2025-12-13")) ~ "rnd1",
        Country == "ALG" & between(Date_of_LQAS, as.Date("2025-12-31"), as.Date("2026-01-06")) ~ "rnd2",
        TRUE ~ roundNumber
      )]
    }
  }

  # ============================================================
  # Metrics Calculation
  # ============================================================
  log_info("Step 3: Calculating metrics...")

  # Calculate sampled counts
  if (length(sex_cols) > 0 && "Count_HH_count" %in% names(AC)) {
    AC[, female_sampled := rowSums(.SD, na.rm = TRUE), .SDcols = sex_cols]
    AC[, male_sampled := Count_HH_count - female_sampled]
  } else {
    AC[, `:=`(female_sampled = NA_real_, male_sampled = NA_real_)]
  }

  if (length(fm_cols) > 0 && "Count_HH_count" %in% names(AC)) {
    AC[, total_vaccinated := rowSums(.SD, na.rm = TRUE), .SDcols = fm_cols]
    AC[, missed_child := Count_HH_count - total_vaccinated]
  } else {
    AC[, `:=`(total_vaccinated = NA_real_, missed_child = NA_real_)]
  }

  # Create female vaccinated columns
  for (i in seq_along(sex_cols)) {
    if (i <= length(fm_cols)) {
      sex_col <- sex_cols[i]
      fm_col <- fm_cols[i]
      if (!is.na(fm_col) && fm_col %in% names(AC)) {
        new_col <- paste0("FV", str_extract(sex_col, "\\d+"))
        AC[, (new_col) := ifelse(get(sex_col) + get(fm_col) >= 2, 1, 0)]
      }
    }
  }

  fv_cols <- names(AC)[str_detect(names(AC), "^FV\\d+")]
  if (length(fv_cols) > 0 && "total_vaccinated" %in% names(AC)) {
    AC[, female_vaccinated := rowSums(.SD, na.rm = TRUE), .SDcols = fv_cols]
    AC[, male_vaccinated := total_vaccinated - female_vaccinated]
  } else {
    AC[, `:=`(female_vaccinated = NA_real_, male_vaccinated = NA_real_)]
  }

  # Sum reason columns (only if they exist)
  r_house_cols <- names(AC)[str_detect(names(AC), "^R_House_not_visited_")]
  if (length(r_house_cols) > 0) AC[, R_House_not_visited := rowSums(.SD, na.rm = TRUE), .SDcols = r_house_cols]

  r_vacc_cols <- names(AC)[str_detect(names(AC), "^R_Vaccinated_but_not_FM_")]
  if (length(r_vacc_cols) > 0) AC[, R_Vaccinated_but_not_FM := rowSums(.SD, na.rm = TRUE), .SDcols = r_vacc_cols]

  r_noncomp_cols <- names(AC)[str_detect(names(AC), "^R_Non_Compliance_")]
  if (length(r_noncomp_cols) > 0) AC[, R_Non_Compliance := rowSums(.SD, na.rm = TRUE), .SDcols = r_noncomp_cols]

  r_asleep_cols <- names(AC)[str_detect(names(AC), "^R_Child_was_asleep_")]
  if (length(r_asleep_cols) > 0) AC[, R_Child_was_asleep := rowSums(.SD, na.rm = TRUE), .SDcols = r_asleep_cols]

  r_visitor_cols <- names(AC)[str_detect(names(AC), "^R_Child_is_a_visitor_")]
  if (length(r_visitor_cols) > 0) AC[, R_Child_is_a_visitor := rowSums(.SD, na.rm = TRUE), .SDcols = r_visitor_cols]

  r_absent_cols <- names(AC)[str_detect(names(AC), "^R_childabsent_")]
  if (length(r_absent_cols) > 0) AC[, R_childabsent := rowSums(.SD, na.rm = TRUE), .SDcols = r_absent_cols]

  if (length(cgs_cols) > 0) {
    AC[, Care_Giver_Informed_SIA := rowSums(.SD, na.rm = TRUE), .SDcols = cgs_cols]
  } else {
    AC[, Care_Giver_Informed_SIA := NA_real_]
  }

  # ============================================================
  # Build reason wide tables
  # ============================================================
  absent_reason_wide <- build_reason_wide(AC, abs_reason_cols, "abs_reason_")
  noncomp_reason_wide <- build_reason_wide(AC, nc_reason_cols, "nc_reason_")

  # ============================================================
  # Aggregate by District
  # ============================================================
  log_info("Step 4: Aggregating by district...")

  # Define columns to aggregate
  agg_cols <- c("Country", "Region", "District", "Response", "roundNumber")
  agg_cols_exist <- agg_cols[agg_cols %in% names(AC)]

  # Define summary columns that exist
  summary_cols <- c(
    "Date_of_LQAS", "Cluster", "male_sampled", "female_sampled", "Count_HH_count",
    "male_vaccinated", "female_vaccinated", "total_vaccinated", "missed_child",
    "R_Non_Compliance", "R_House_not_visited", "R_childabsent",
    "R_Child_was_asleep", "R_Child_is_a_visitor", "R_Vaccinated_but_not_FM",
    "Care_Giver_Informed_SIA"
  )
  summary_cols_exist <- summary_cols[summary_cols %in% names(AC)]

  if (length(agg_cols_exist) == 0) {
    log_error("No aggregation columns found!")
    return(NULL)
  }

  # Create summary expression dynamically
  F1 <- AC %>%
    mutate(Date_of_LQAS = as.Date(Date_of_LQAS)) %>%
    group_by(across(all_of(agg_cols_exist))) %>%
    summarise(
      start_date = min(Date_of_LQAS, na.rm = TRUE),
      end_date = max(Date_of_LQAS, na.rm = TRUE),
      cluster = sum(as.numeric(Cluster), na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(Count_HH_count, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_Non_Compliance = if ("R_Non_Compliance" %in% names(AC)) sum(R_Non_Compliance, na.rm = TRUE) else 0,
      r_House_not_visited = if ("R_House_not_visited" %in% names(AC)) sum(R_House_not_visited, na.rm = TRUE) else 0,
      r_childabsent = if ("R_childabsent" %in% names(AC)) sum(R_childabsent, na.rm = TRUE) else 0,
      r_Child_was_asleep = if ("R_Child_was_asleep" %in% names(AC)) sum(R_Child_was_asleep, na.rm = TRUE) else 0,
      r_Child_is_a_visitor = if ("R_Child_is_a_visitor" %in% names(AC)) sum(R_Child_is_a_visitor, na.rm = TRUE) else 0,
      r_Vaccinated_but_not_FM = if ("R_Vaccinated_but_not_FM" %in% names(AC)) sum(R_Vaccinated_but_not_FM, na.rm = TRUE) else 0,
      Care_Giver_Informed_SIA = if ("Care_Giver_Informed_SIA" %in% names(AC)) sum(Care_Giver_Informed_SIA, na.rm = TRUE) else 0,
      .groups = "drop"
    ) %>%
    mutate(
      percent_care_Giver_Informed_SIA = Care_Giver_Informed_SIA / total_sampled,
      total_missed = ifelse(total_sampled < 60, (60 - total_sampled) + missed_child, missed_child),
      Status = ifelse(total_missed <= 3, "Pass", "Fail"),
      Performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      ),
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
      )
    ) %>%
    filter(cluster >= 3, start_date > as.Date("2019-10-01"))

  # Join reason tables if they exist
  if (!is.null(absent_reason_wide) && nrow(absent_reason_wide) > 0) {
    F1 <- F1 %>%
      left_join(absent_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber"))
  }

  if (!is.null(noncomp_reason_wide) && nrow(noncomp_reason_wide) > 0) {
    F1 <- F1 %>%
      left_join(noncomp_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber"))
  }

  # Fill NA reasons with 0
  reason_count_cols <- names(F1)[str_detect(names(F1), "^abs_reason_|^nc_reason_")]
  for (col in reason_count_cols) {
    if (col %in% names(F1)) {
      F1[[col]] <- replace_na(F1[[col]], 0)
    }
  }

  # ============================================================
  # Add Vaccine Type
  # ============================================================
  log_info("Step 5: Adding vaccine types...")

  F2 <- F1 %>%
    mutate(
      Vaccine.type = case_when(
        str_detect(Response, "nOPV|VPOn|nOPV2") ~ "nOPV2",
        str_detect(Response, "bOPV|BOPV") ~ "bOPV",
        str_detect(Response, "mOPV") ~ "mOPV",
        TRUE ~ NA_character_
      )
    )

  # ============================================================
  # Final Formatting
  # ============================================================
  log_info("Step 6: Final formatting...")

  F3 <- F2 %>%
    mutate(
      tot_r = coalesce(r_Non_Compliance, 0) + coalesce(r_House_not_visited, 0) + coalesce(r_childabsent, 0) +
        coalesce(r_Child_was_asleep, 0) + coalesce(r_Child_is_a_visitor, 0) + coalesce(r_Vaccinated_but_not_FM, 0),
      other_r = ifelse((total_missed - tot_r) < 0, 0, (total_missed - tot_r)),
      Country = case_when(
        Country == "DRC" ~ "RDC",
        Country == "CAMEROON" ~ "CAE",
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
      r_Non_Compliance,
      r_House_not_visited,
      r_childabsent,
      r_Child_was_asleep,
      r_Child_is_a_visitor,
      r_Vaccinated_but_not_FM,
      other_r,
      percent_care_Giver_Informed_SIA,
      starts_with("abs_reason_"),
      starts_with("nc_reason_")
    ) %>%
    mutate(
      percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA * 100, 2),
      across(c(r_Non_Compliance, r_House_not_visited, r_childabsent,
               r_Child_was_asleep, r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r),
             ~ ifelse(total_missed == 0, 0, (.x / total_missed) * 100),
             .names = "prct_{.col}"),
      across(starts_with("prct_"), ~ round(.x, 2)),
      proportion_missed_child = round(total_missed / total_sampled, 2)
    )

  # Remove duplicates
  F3 <- F3 %>%
    distinct(country, province, district, response, roundNumber, .keep_all = TRUE)

  # ============================================================
  # Save Outputs
  # ============================================================
  log_info("Step 7: Saving outputs...")

  final_csv <- "data/final/lqas_dashboard_input.csv"
  fwrite(F3, final_csv)
  log_info("✅ Saved final data to {final_csv}")

  final_parquet <- "data/final/lqas_dashboard_input.parquet"
  write_parquet(F3, final_parquet)
  log_info("✅ Saved parquet to {final_parquet}")

  summary <- list(
    timestamp = Sys.time(),
    total_records = nrow(F3),
    total_columns = ncol(F3),
    countries = unique(F3$country),
    file_size_mb = file.size(final_csv) / (1024 * 1024)
  )

  saveRDS(summary, "data/processed/processing_summary.rds")
  log_info("✅ Saved processing summary")

  log_info("=" %>% paste(rep("=", 60), collapse = ""))
  log_info("PROCESSING COMPLETE! {nrow(F3)} records processed")
  log_info("=" %>% paste(rep("=", 60), collapse = ""))

  return(F3)
}

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

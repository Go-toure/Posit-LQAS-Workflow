#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Processing Script - Posit Workflow Parquet Version
# Adapted from convert_padacord_LQAS_to_csv for Parquet files
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(arrow)
  library(data.table)
  library(logger)
  library(fs)
  library(stringr)
  library(stringi)
  library(janitor)
  library(parallel)
})

# Configure logging
log_dir <- "logs"
dir_create(log_dir)
log_appender(appender_file(file.path(log_dir, "process_lqas.log")))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting LQAS Data Processing (Parquet Version)")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# ============================================================
# ARGUMENT PARSING
# ============================================================
library(argparse)
parser <- ArgumentParser()
parser$add_argument("--input-dir", default = "data/raw", 
                    help = "Input directory with Parquet files")
parser$add_argument("--output-dir", default = "data/processed", 
                    help = "Output directory for processed CSV files")
parser$add_argument("--chunk", type = "integer", default = 1,
                    help = "Chunk number for parallel processing")
parser$add_argument("--total-chunks", type = "integer", default = 1,
                    help = "Total number of chunks")
parser$add_argument("--force-full", action = "store_true",
                    help = "Force full reprocessing")
parser$add_argument("--lookup-file", default = "data/lookup/lqas_lookup.xlsx",
                    help = "Lookup file path for date harmonization")

args <- parser$parse_args()

log_info("Arguments:")
log_info("  input-dir: {args$input_dir}")
log_info("  output-dir: {args$output_dir}")
log_info("  chunk: {args$chunk}/{args$total_chunks}")
log_info("  force-full: {args$force_full}")
log_info("  lookup-file: {args$lookup_file}")

# ============================================================
# HELPER FUNCTIONS (from original script)
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
  
  x[x %in% c("", "na", "n_a", "n/a", "null", "none", "missing", "unknown", ".", "-", "--")] <- NA
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
# SMART FM_CHILD HARMONIZER (dynamic from original)
# ============================================================

update_fm_child_dynamic <- function(data) {
  idx_from_any <- names(data) |>
    str_match("^Count_HH\\[(\\d+)\\]/FM_Child(R|L)?$") |>
    (\(m) m[, 2])() |>
    na.omit() |>
    unique() |>
    as.integer() |>
    sort()
  
  if (length(idx_from_any) == 0) return(data)
  
  has_R <- any(str_detect(names(data), "^Count_HH\\[\\d+\\]/FM_ChildR$"))
  has_L <- any(str_detect(names(data), "^Count_HH\\[\\d+\\]/FM_ChildL$"))
  has_RL <- has_R && has_L
  if (!has_RL) return(data)
  
  mutate_list <- list()
  
  for (ii in idx_from_any) {
    col_FM <- sprintf("Count_HH[%d]/FM_Child", ii)
    col_R <- sprintf("Count_HH[%d]/FM_ChildR", ii)
    col_L <- sprintf("Count_HH[%d]/FM_ChildL", ii)
    
    if (!(col_R %in% names(data) && col_L %in% names(data))) next
    
    if (col_FM %in% names(data)) {
      mutate_list[[col_FM]] <- expr(
        ifelse((!!sym(col_R) + !!sym(col_L)) >= 1 | !!sym(col_FM) == 1, 1, 0)
      )
    } else {
      mutate_list[[col_FM]] <- expr(
        ifelse((!!sym(col_R) + !!sym(col_L)) >= 1, 1, 0)
      )
    }
  }
  
  if (length(mutate_list) == 0) return(data)
  data %>% mutate(!!!mutate_list)
}

# ============================================================
# READ PARQUET FILES
# ============================================================

log_info("Reading Parquet files from {args$input_dir}...")

parquet_files <- list.files(args$input_dir, pattern = "\\.parquet$", full.names = TRUE)

if (length(parquet_files) == 0) {
  log_error("No Parquet files found in {args$input_dir}")
  quit(status = 1)
}

log_info("Found {length(parquet_files)} Parquet files")

# Split files into chunks for parallel processing
if (args$total_chunks > 1) {
  chunk_size <- ceiling(length(parquet_files) / args$total_chunks)
  start_idx <- (args$chunk - 1) * chunk_size + 1
  end_idx <- min(args$chunk * chunk_size, length(parquet_files))
  parquet_files <- parquet_files[start_idx:end_idx]
  log_info("Processing chunk {args$chunk}/{args$total_chunks}: {length(parquet_files)} files")
}

# Process each file
processing_results <- list()

for (i in seq_along(parquet_files)) {
  file_path <- parquet_files[i]
  file_name <- basename(file_path)
  form_id <- gsub("\\.parquet$", "", file_name)
  
  log_info("\n📂 Processing file {i}/{length(parquet_files)}: {file_name}")
  
  # Read parquet file
  data <- tryCatch({
    start_time <- Sys.time()
    result <- as.data.table(read_parquet(file_path))
    read_time <- round(as.numeric(Sys.time() - start_time, units = "secs"), 2)
    log_info("   ⏱️ Read time: {read_time} seconds")
    log_info("   📊 Data: {nrow(result)} rows, {ncol(result)} columns")
    result
  }, error = function(e) {
    log_error("Failed to read {file_name}: {e$message}")
    return(NULL)
  })
  
  if (is.null(data)) next
  if (nrow(data) == 0) {
    log_warn("Empty dataset in {file_name}")
    next
  }
  
  # ============================================================
  # APPLY ORIGINAL PROCESSING STEPS
  # ============================================================
  
  # Step 1: Rename repetitive columns
  data <- rename_repetitive_columns(data)
  
  # Step 2: Apply custom rules
  data <- apply_custom_rules(data, form_id)
  
  # Step 3: Select relevant columns
  selected_columns <- c(
    "Response", "roundNumber", "Country", "Region", "District", "Date_of_LQAS",
    "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_altitude",
    grep("^Count_HH\\[\\d+\\]/(Sex_Child|FM_Child|FM_ChildR|FM_ChildL|Reason_Not_FM|Reason_NC_NFM|Reason_ABS_NFM|Care_Giver_Informed_SIA)$",
         names(data), value = TRUE),
    "Count_HH_count", "Cluster"
  )
  selected_columns <- intersect(selected_columns, names(data))
  data <- data %>% select(all_of(selected_columns))
  
  # Step 4: Standardize data
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
      across(matches("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA$"), standardize_informed_sia),
      across(matches("^Count_HH\\[\\d+\\]/Reason_ABS_NFM$"), map_abs_reason),
      across(matches("^Count_HH\\[\\d+\\]/Reason_NC_NFM$"), map_nc_reason)
    )
  
  # Step 5: Update FM_Child dynamically
  data <- update_fm_child_dynamic(data)
  
  # Step 6: Get child indices
  child_idx <- names(data) |>
    str_match("^Count_HH\\[(\\d+)\\]/") |>
    (\(m) m[, 2])() |>
    na.omit() |>
    unique() |>
    as.integer() |>
    sort()
  
  sex_cols <- intersect(sprintf("Count_HH[%d]/Sex_Child", child_idx), names(data))
  fm_cols <- intersect(sprintf("Count_HH[%d]/FM_Child", child_idx), names(data))
  fmr_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildR", child_idx), names(data))
  fml_cols <- intersect(sprintf("Count_HH[%d]/FM_ChildL", child_idx), names(data))
  cgs_cols <- intersect(sprintf("Count_HH[%d]/Care_Giver_Informed_SIA", child_idx), names(data))
  abs_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_ABS_NFM", child_idx), names(data))
  nc_reason_cols <- intersect(sprintf("Count_HH[%d]/Reason_NC_NFM", child_idx), names(data))
  
  # Step 7: Reason_Not_FM processing
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
  
  # Step 8: Convert numeric fields
  numeric_count_cols <- c(sex_cols, fm_cols, fmr_cols, fml_cols, cgs_cols, "Count_HH_count", "Cluster")
  numeric_count_cols <- intersect(numeric_count_cols, names(data))
  
  data <- data %>%
    mutate(across(all_of(numeric_count_cols), ~ as.numeric(replace(., . %in% c(".", "NA", ""), NA))))
  
  # Step 9: ALG response/round fixes
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
  
  # Step 10: Build reason wide tables
  absent_reason_wide <- build_reason_wide(data, abs_reason_cols, "abs_reason_")
  noncomp_reason_wide <- build_reason_wide(data, nc_reason_cols, "nc_reason_")
  
  # Step 11: Calculate metrics
  AF <- data %>%
    relocate(all_of(sex_cols), .after = "_GPS_hh_altitude") %>%
    relocate(all_of(fm_cols), .after = tail(sex_cols, 1)) %>%
    mutate(
      female_sampled = rowSums(across(all_of(sex_cols)), na.rm = TRUE),
      male_sampled = Count_HH_count - female_sampled,
      total_vaccinated = rowSums(across(all_of(fm_cols)), na.rm = TRUE),
      missed_child = Count_HH_count - total_vaccinated
    )
  
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
  
  AG <- AG %>% mutate(across(starts_with("R_"), as.numeric))
  
  AS <- AG %>%
    mutate(
      R_House_not_visited = rowSums(across(matches("^R_House_not_visited_Count_HH"), ~ replace_na(., 0))),
      R_Vaccinated_but_not_FM = rowSums(across(matches("^R_Vaccinated_but_not_FM_Count_HH"), ~ replace_na(., 0))),
      R_Non_Compliance = rowSums(across(matches("^R_Non_Compliance_Count_HH"), ~ replace_na(., 0))),
      R_Child_was_asleep = rowSums(across(matches("^R_Child_was_asleep_Count_HH"), ~ replace_na(., 0))),
      R_Child_is_a_visitor = rowSums(across(matches("^R_Child_is_a_visitor_Count_HH"), ~ replace_na(., 0))),
      R_childabsent = rowSums(across(matches("^R_childabsent_Count_HH"), ~ replace_na(., 0))),
      Care_Giver_Informed_SIA = rowSums(across(all_of(cgs_cols), ~ replace_na(., 0)))
    )
  
  AQ <- AS %>%
    select(
      Country, Region, District, Response, roundNumber, Date_of_LQAS,
      male_sampled, female_sampled,
      total_sampled = Count_HH_count,
      male_vaccinated, female_vaccinated, total_vaccinated, missed_child,
      R_Non_Compliance, R_House_not_visited, R_childabsent, R_Child_was_asleep,
      R_Child_is_a_visitor, R_Vaccinated_but_not_FM, Care_Giver_Informed_SIA,
      Cluster
    ) %>%
    mutate(Cluster = as.numeric(Cluster))
  
  # Step 12: Aggregate to cluster level
  F1 <- AQ %>%
    mutate(Date_of_LQAS = as_date(Date_of_LQAS)) %>%
    group_by(Country, Region, District, Response, roundNumber) %>%
    arrange(Date_of_LQAS, .by_group = TRUE) %>%
    mutate(
      date.diff = c(1, diff(Date_of_LQAS)),
      period = cumsum(date.diff != 1)
    ) %>%
    ungroup() %>%
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
      r_Non_Compliance = sum(R_Non_Compliance, na.rm = TRUE),
      r_House_not_visited = sum(R_House_not_visited, na.rm = TRUE),
      r_childabsent = sum(R_childabsent, na.rm = TRUE),
      r_Child_was_asleep = sum(R_Child_was_asleep, na.rm = TRUE),
      r_Child_is_a_visitor = sum(R_Child_is_a_visitor, na.rm = TRUE),
      r_Vaccinated_but_not_FM = sum(R_Vaccinated_but_not_FM, na.rm = TRUE),
      Care_Giver_Informed_SIA = sum(Care_Giver_Informed_SIA, na.rm = TRUE),
      percent_care_Giver_Informed_SIA = Care_Giver_Informed_SIA / total_sampled,
      .groups = "drop"
    ) %>%
    left_join(absent_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber")) %>%
    left_join(noncomp_reason_wide, by = c("Country", "Region", "District", "Response", "roundNumber"))
  
  reason_count_cols <- names(F1)[str_detect(names(F1), "^abs_reason_|^nc_reason_")]
  if (length(reason_count_cols) > 0) {
    F1 <- F1 %>% mutate(across(all_of(reason_count_cols), ~ replace_na(., 0)))
  }
  
  # Step 13: Filter and calculate metrics
  F2 <- F1 %>%
    filter(start_date > as.Date("2019-10-01")) %>%
    mutate(
      percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA * 100, 2),
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
  
  # Step 14: Vaccine type classification
  FI <- F2 %>%
    mutate(
      Vaccine.type = case_when(
        str_detect(Response, "BITTOU|MENAKA-mOPV2|BAMAKO-mOPV2|KANKAN-mOPV|MLI-12DS-01-2021-mOPV2|CONAKRY-mOPV|Ouagadogou|Bangui 1|GOTHEY|YOPOUGON|Golfe|MDG-2023-03-01_bOPV|BEN-xxDS-02-2020|BEN-26DS-08-2020|Chavuma-mOPV|Luapula-mOPV") ~ "mOPV",
        str_detect(Response, "nOPV|VPOn|TSHUAPA|Tanganyika|Liberia|Mauritania|KOUIBLY|Sierra Leone|SEN|CEN|MAL|BEN-39DS-01-2021|BERTOUA|EBOLOWA|EXNORD|ExtNord2023|ADDIS ABABA|Mekelle|AMANSIE SOUTH|CAF-2020-002|CENBLOCK|CENTRALBLK|CHA-17DS-02-2020|DONOMANGA|GNBnOPV|GOLFE|GOTHEYE|KEN-13DS-02-2021|MopUp2022|SSD-79DS-09-2020|ALG-2023-09-01_nOPV|ALG-2024-01-01_nOPV|nOPV2022|BEN-2023-09-01_nOPV|BFA-2023-05-01_nOPV|BFA-2023-09-01_nOPV|BFA-2024-02-01_nOPV|BITTOU-mOPV2|Ouagadogou-mOPV2|BOT-2023-02-01_nOPV|CAM-2023-05-01_nOPV|CAM-2023-08-01_nOPV|CAM-2024-02-01_nOPV|nOPV2022|nOPV2023|nVPO|nVPO_Maradi|nVPO_Zinder|nVPO2|May2021|OPVb2021|OPVb2022|RSSmOPV10C2021|SEN_VPOn|UGAnOPV|VPOb|VPOb13ProV|n_OPV") ~ "nOPV2",
        str_detect(Response, "BOPV|bOPV|OPVb|WPV1") ~ "bOPV",
        TRUE ~ NA_character_
      ),
      Response = case_when(
        Response == "nOPV2022" & Country == "GHA" ~ "nOPV2022",
        Response == "CENTRALBLK" ~ "DRC-7DS-02-2022",
        Response == "nOPV2022" & Country == "RDC" ~ "DRC-39DS-01-2021",
        Response %in% c("Tshuapa", "TSHUAPA") ~ "DRC-23DS-12-2020",
        Response == "VPOb13ProV" ~ "DRC-39DS-01-2021",
        TRUE ~ Response
      )
    ) %>%
    mutate(
      Vaccine.type = case_when(
        Response == "DRC-2025-02-01_nOPV_sNID" &
          roundNumber == "Rnd1" &
          Region %in% c("HAUT KATANGA", "HAUT LOMAMI", "TANGANIKA", "KINSHASA") ~ "nOPV2",
        Response == "DRC-2025-02-01_nOPV_sNID" &
          roundNumber == "Rnd1" &
          Region == "TSHOPO" &
          District %in% c("ALUNGULI", "FEREKENI", "KAILO", "LUBUTU", "OBOKOTE", "OPIENGE") ~ "bOPV",
        TRUE ~ Vaccine.type
      )
    )
  
  # Step 15: Join with lookup table for dates
  if (file.exists(args$lookup_file)) {
    date <- read_excel(args$lookup_file)
    
    date <- date %>%
      mutate(`Round Number` = case_when(
        `Round Number` == "Round 0" ~ "Rnd0",
        `Round Number` == "Round 1" ~ "Rnd1",
        `Round Number` == "Round 2" ~ "Rnd2",
        `Round Number` == "Round 3" ~ "Rnd3",
        `Round Number` == "Round 4" ~ "Rnd4",
        `Round Number` == "Round 5" ~ "Rnd5",
        `Round Number` == "Round 6" ~ "Rnd6",
        TRUE ~ `Round Number`
      ))
    
    data_lookup <- date %>%
      rename(
        Response = `OBR Name`,
        Vaccine.type = Vaccines,
        roundNumber = `Round Number`
      ) %>%
      mutate(round_start_date = as_date(`Round Start Date`)) %>%
      mutate(round_start_date = case_when(
        Country == "ALGERIA" & Response == "ALG-2024-01-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-02-18"),
        TRUE ~ round_start_date
      )) %>%
      mutate(
        start_date = round_start_date + 4,
        end_date = as_date(start_date) + 1
      )
    
    lookup_table <- data_lookup %>%
      select(Response, Vaccine.type, roundNumber, round_start_date, start_date, end_date) %>%
      as_tibble() %>%
      mutate(
        start_date = as_date(start_date),
        end_date = as_date(end_date),
        round_start_date = as_date(round_start_date)
      )
    
    FI <- FI %>%
      left_join(lookup_table, by = c("Response", "Vaccine.type", "roundNumber")) %>%
      mutate(
        start_date = coalesce(start_date.y, as_date(start_date.x)),
        end_date = coalesce(end_date.y, as_date(end_date.x)),
        round_start_date = coalesce(round_start_date, start_date - days(4))
      ) %>%
      select(-start_date.x, -start_date.y, -end_date.x, -end_date.y) %>%
      filter(District != "NA")
  } else {
    log_warn("Lookup file not found: {args$lookup_file}")
    FI <- FI %>%
      mutate(
        start_date = as_date(start_date),
        end_date = as_date(start_date) + 1,
        round_start_date = start_date - days(4)
      )
  }
  
  # Step 16: Final transformations
  F5 <- FI %>%
    mutate(
      tot_r = r_Non_Compliance + r_House_not_visited + r_childabsent +
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
      end_date = ymd(end_date),
      end_date = case_when(
        year(end_date) == 2028 ~ update(end_date, year = 2023),
        year(end_date) == 2025 ~ update(end_date, year = 2024),
        TRUE ~ end_date
      )
    ) %>%
    arrange(start_date)
  
  # Step 17: Calculate percentages and remove duplicates
  F6 <- F5 %>%
    mutate(
      across(
        c(r_Non_Compliance, r_House_not_visited, r_childabsent, r_Child_was_asleep,
          r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r),
        ~ ifelse(total_missed == 0, 0, (.x / total_missed) * 100),
        .names = "prct_{.col}"
      )
    ) %>%
    mutate(across(starts_with("prct_"), ~ round(.x, 2))) %>%
    mutate(
      proportion_missed_child = total_missed / total_sampled,
      proportion_missed_child = round(proportion_missed_child, 2)
    )
  
  # Remove duplicates
  F6 <- F6 %>%
    mutate(rnd_distinct = paste(country, province, district, response, roundNumber, sep = "_")) %>%
    distinct(rnd_distinct, .keep_all = TRUE) %>%
    mutate(round_start_date = case_when(
      country == "ALGERIA" & response == "ALG-2023-09-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-01-28"),
      TRUE ~ round_start_date
    )) %>%
    mutate(response = case_when(
      country == "ALGERIA" & response == "nOPV2022" ~ "Algeria Outbreak",
      TRUE ~ response
    ))
  
  # Step 18: Save output
  output_file <- file.path(args$output_dir, paste0(form_id, "_processed.csv"))
  dir_create(args$output_dir)
  
  tryCatch({
    fwrite(F6, output_file)
    log_info("✅ Saved to {output_file}")
    log_info("   📈 Final: {nrow(F6)} rows, {ncol(F6)} columns")
    processing_results[[form_id]] <- list(
      status = "success",
      rows = nrow(F6),
      cols = ncol(F6),
      file = output_file
    )
  }, error = function(e) {
    log_error("Failed to save {output_file}: {e$message}")
    processing_results[[form_id]] <- list(status = "failed", error = e$message)
  })
}

# ============================================================
# SUMMARY
# ============================================================

log_info("\n" %>% paste(rep("=", 60), collapse = ""))
log_info("📊 PROCESSING SUMMARY - Chunk {args$chunk}/{args$total_chunks}")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

success_count <- sum(sapply(processing_results, function(x) x$status == "success"))
failed_count <- sum(sapply(processing_results, function(x) x$status == "failed"))

log_info("Successfully processed: {success_count} files")
log_info("Failed: {failed_count} files")

for (res in names(processing_results)) {
  if (processing_results[[res]]$status == "success") {
    log_info("  ✅ {res}: {processing_results[[res]]$rows} rows")
  } else {
    log_info("  ❌ {res}: {processing_results[[res]]$error}")
  }
}

log_info("\n🎉 Processing complete for chunk {args$chunk}!")
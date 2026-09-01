# ============================================================
# 06_push_to_sharepoint.R
#
# Fills in the other "missing piece" needed to fully automate this
# pipeline: pushes data/final/afro_lqas_repositorty.csv up to the WHO
# AFRO GIS Workspace Data Repository on SharePoint AS "AFRO_LQAS_data_c.csv"
# -- refreshing that existing file in place, not adding a new one
# alongside it under the local output's own name. AFRO_LQAS_data_c.csv
# is the file other pipelines (im_workflow, AFRO-SIA-Dashboard-input)
# already download FROM this same folder as one of their own inputs,
# so this step is what keeps that shared file current instead of
# someone re-uploading it by hand.
#
# Target (from the folder link the workflow owner shared):
#   https://worldhealthorg.sharepoint.com/sites/AF-pep/GISWORKSPACE/
#     Shared Documents/7. SIA_Data/Data Repository
#   site path  : sites/AF-pep/GISWORKSPACE   (a site nested under AF-pep)
#   library    : Shared Documents (the site's default document library/drive)
#   folder path: 7. SIA_Data/Data Repository
#
# This REPLACES R/sharepoint_connection.r's "Method 1" (which uploaded
# to the ROOT of the site instead of this folder, because it resolved
# the site as just "sites/AF-pep" and left "GISWORKSPACE" out of the
# site path -- GISWORKSPACE is part of the site's own address, not a
# folder inside it) and its "Method 2" interactive-login fallback
# (which needed a person to click through a WHO SSO login each run --
# not usable from an unattended scheduled task). This script uses the
# same app-only (client-credentials) Microsoft Graph authentication
# already used successfully elsewhere in this WHO AFRO tooling (the
# im_workflow and AFRO-SIA-Dashboard-input projects both authenticate
# to the SAME "AF-pep/GISWORKSPACE" site this way already), and
# resolves the exact target folder before uploading -- if the folder
# path doesn't resolve, this FAILS LOUDLY instead of silently dropping
# the file somewhere else.
#
# SAFETY:
#   - Credentials come ONLY from config/secrets.env (git-ignored --
#     see config/secrets.env.example), never hardcoded in this file.
#   - The target folder is verified to exist before upload; if it
#     doesn't resolve, the script errors out with a clear message
#     instead of guessing an alternate location.
#   - Only data/final/afro_lqas_repositorty.csv is uploaded (that's
#     the one file actually requested -- 04_dashboard.html is not
#     pushed here), and it's uploaded AS "AFRO_LQAS_data_c.csv" so it
#     refreshes that existing shared file instead of creating a new one.
#   - Never prints the client secret or access token.
#
# Usage:
#   Rscript R/06_push_to_sharepoint.R
# (Runs relative to the project root, same as every other step in
# run_pipeline.r.)
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

source("R/utils/env_loader.R")
load_secrets_env(getwd())

log_msg <- function(msg) cat(sprintf("[06_push_to_sharepoint] %s\n", msg))

# ------------------------------------------------------------
# Configuration
# ------------------------------------------------------------
SITE_HOSTNAME <- "worldhealthorg.sharepoint.com"
SITE_PATH <- "sites/AF-pep/GISWORKSPACE"
TARGET_FOLDER <- "7. SIA_Data/Data Repository"
FILE_TO_UPLOAD <- "data/final/afro_lqas_repositorty.csv"
# The name this file is uploaded AS on SharePoint -- intentionally
# different from FILE_TO_UPLOAD's own local name, so this refreshes
# the existing AFRO_LQAS_data_c.csv that other pipelines already
# depend on, instead of creating a second, differently-named file.
REMOTE_FILE_NAME <- "AFRO_LQAS_data_c.csv"
GRAPH_ENDPOINT <- "https://graph.microsoft.com/v1.0"

# ------------------------------------------------------------
# Encode a server-relative path's segments for use in a Graph
# root:/{path}: addressing URL, without touching the "/" separators.
# ------------------------------------------------------------
encode_path <- function(path) {
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste(vapply(segments, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
}

get_access_token <- function(tenant_id, client_id, client_secret) {
  token_url <- paste0("https://login.microsoftonline.com/", tenant_id, "/oauth2/v2.0/token")
  body <- list(
    client_id = client_id,
    client_secret = client_secret,
    scope = "https://graph.microsoft.com/.default",
    grant_type = "client_credentials"
  )
  response <- POST(token_url, body = body, encode = "form")
  if (status_code(response) != 200) {
    error_content <- content(response, "parsed")
    stop("Failed to get access token: ", error_content$error_description %||% status_code(response))
  }
  content(response, "parsed")$access_token
}

`%||%` <- function(a, b) if (is.null(a)) b else a

get_drive_id <- function(access_token) {
  site_identifier <- paste0(SITE_HOSTNAME, ":/", SITE_PATH)
  site_url <- paste0(GRAPH_ENDPOINT, "/sites/", site_identifier)
  site_response <- GET(site_url, add_headers(Authorization = paste("Bearer", access_token)))
  if (status_code(site_response) != 200) {
    stop("Could not resolve SharePoint site '", SITE_PATH, "' (status ",
         status_code(site_response), "). Check SITE_PATH is still correct.")
  }
  site_id <- content(site_response, "parsed")$id
  log_msg(paste0("Resolved site: ", SITE_PATH, " -> ", site_id))

  drive_url <- paste0(GRAPH_ENDPOINT, "/sites/", site_id, "/drive")
  drive_response <- GET(drive_url, add_headers(Authorization = paste("Bearer", access_token)))
  if (status_code(drive_response) != 200) {
    stop("Could not resolve the site's default document library (status ",
         status_code(drive_response), ").")
  }
  content(drive_response, "parsed")$id
}

verify_target_folder <- function(drive_id, access_token) {
  folder_url <- paste0(GRAPH_ENDPOINT, "/drives/", drive_id, "/root:/", encode_path(TARGET_FOLDER))
  folder_response <- GET(folder_url, add_headers(Authorization = paste("Bearer", access_token)))
  if (status_code(folder_response) != 200) {
    stop("Target folder '", TARGET_FOLDER, "' was not found in the site's document library ",
         "(status ", status_code(folder_response), "). Refusing to upload to a fallback ",
         "location -- check the folder still exists at that exact path.")
  }
  log_msg(paste0("Confirmed target folder exists: ", TARGET_FOLDER))
  invisible(TRUE)
}

upload_file <- function(local_file_path, remote_file_name, drive_id, access_token) {
  file_size <- file.info(local_file_path)$size
  item_path <- paste0(TARGET_FOLDER, "/", remote_file_name)
  log_msg(sprintf("Uploading %s (%.1f KB) -> %s/%s (refreshing the existing file)",
                   basename(local_file_path), file_size / 1024, TARGET_FOLDER, remote_file_name))

  upload_url <- paste0(GRAPH_ENDPOINT, "/drives/", drive_id, "/root:/", encode_path(item_path), ":/content")

  if (file_size > 4 * 1024 * 1024) {
    # Large-file path: create an upload session and upload in chunks.
    # NOTE: Graph's createUploadSession endpoint needs a real
    # "Content-Type: application/json" header to parse the request
    # body -- sending a literal "{}" string via encode = "raw" (as the
    # old sharepoint_connection.r did) never sets that header, and
    # Graph rejects it with a 400 before ever looking at the body.
    # encode = "json" makes httr set the header and serialize the body
    # correctly. The body itself also now explicitly asks Graph to
    # replace any existing file of the same name (matching how the
    # small-file PUT path below behaves by default), so a re-run
    # doesn't fail with a name conflict.
    session_url <- paste0(GRAPH_ENDPOINT, "/drives/", drive_id, "/root:/",
                           encode_path(item_path), ":/createUploadSession")
    session_response <- POST(
      session_url,
      add_headers(Authorization = paste("Bearer", access_token)),
      body = list(item = list(`@microsoft.graph.conflictBehavior` = "replace")),
      encode = "json"
    )
    if (status_code(session_response) != 200) {
      stop("Failed to create an upload session (status ", status_code(session_response), "): ",
           jsonlite::toJSON(content(session_response, "parsed"), auto_unbox = TRUE))
    }
    upload_session_url <- content(session_response, "parsed")$uploadUrl

    chunk_size <- 10 * 1024 * 1024
    file_conn <- file(local_file_path, "rb")
    file_content <- readBin(file_conn, "raw", n = file_size)
    close(file_conn)

    total_chunks <- ceiling(file_size / chunk_size)
    for (i in seq_len(total_chunks)) {
      start_byte <- (i - 1) * chunk_size
      end_byte <- min(i * chunk_size - 1, file_size - 1)
      chunk_data <- file_content[(start_byte + 1):(end_byte + 1)]
      content_range <- sprintf("bytes %d-%d/%d", start_byte, end_byte, file_size)
      chunk_response <- PUT(
        upload_session_url,
        add_headers("Content-Range" = content_range, "Content-Type" = "application/octet-stream"),
        body = chunk_data
      )
      if (!(status_code(chunk_response) %in% c(200, 201, 202))) {
        stop("Chunk ", i, " of ", total_chunks, " failed (status ", status_code(chunk_response), ").")
      }
      log_msg(sprintf("  uploaded chunk %d/%d", i, total_chunks))
    }
    item <- content(chunk_response, "parsed")
  } else {
    file_content <- readBin(local_file_path, "raw", n = file_size)
    upload_response <- PUT(
      upload_url,
      add_headers(Authorization = paste("Bearer", access_token), "Content-Type" = "text/csv"),
      body = file_content
    )
    if (!(status_code(upload_response) %in% c(200, 201))) {
      stop("Upload failed (status ", status_code(upload_response), "): ",
           jsonlite::toJSON(content(upload_response, "parsed"), auto_unbox = TRUE))
    }
    item <- content(upload_response, "parsed")
  }

  log_msg("SUCCESS: upload complete.")
  if (!is.null(item$webUrl)) {
    log_msg(paste0("  View at: ", item$webUrl))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------
# Main
# ------------------------------------------------------------
main <- function() {
  tenant_id <- Sys.getenv("SHAREPOINT_TENANT_ID")
  client_id <- Sys.getenv("SHAREPOINT_CLIENT_ID")
  client_secret <- Sys.getenv("SHAREPOINT_CLIENT_SECRET")

  if (tenant_id == "" || client_id == "" || client_secret == "") {
    log_msg("FAILED: SHAREPOINT_TENANT_ID / SHAREPOINT_CLIENT_ID / SHAREPOINT_CLIENT_SECRET ")
    log_msg("are not all set. Add them to config/secrets.env (see config/secrets.env.example).")
    quit(status = 1)
  }

  if (!file.exists(FILE_TO_UPLOAD)) {
    log_msg(paste0("FAILED: ", FILE_TO_UPLOAD, " does not exist -- run the pipeline's ",
                    "processing steps first."))
    quit(status = 1)
  }

  result <- tryCatch({
    access_token <- get_access_token(tenant_id, client_id, client_secret)
    log_msg("Authenticated to Microsoft Graph.")
    drive_id <- get_drive_id(access_token)
    verify_target_folder(drive_id, access_token)
    upload_file(FILE_TO_UPLOAD, REMOTE_FILE_NAME, drive_id, access_token)
    TRUE
  }, error = function(e) {
    log_msg(paste0("FAILED: ", conditionMessage(e)))
    FALSE
  })

  quit(status = ifelse(isTRUE(result), 0, 1))
}

main()

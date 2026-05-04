# ============================================================
# 📤 Upload files to SharePoint - FIXED UPLOAD METHOD
# ============================================================

# Load required libraries
library(httr)
library(jsonlite)

# SharePoint configuration
tenant_id <- "f610c0b7-bd24-4b39-810b-3dc280afb590"
client_id <- "f75b3a4b-6fbd-489b-b358-79eaf0b4e8c0"
client_secret <- "6Lb8Q~tXZVlSShLW_CCMH~JxRfpwXOAmsB674biZ"

# Your SharePoint details
site_hostname <- "worldhealthorg.sharepoint.com"
site_path <- "sites/AF-pep"
folder_relative_path <- "GISWORKSPACE/Shared Documents/7. SIA_Data/Data Repository"

# Files to upload
files_to_upload <- c(
  "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/data/final/afro_lqas_repositorty.csv",
  "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/04_dashboard.html"
)

# ============================================================
# FUNCTION: Get Access Token
# ============================================================

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
    stop("Failed to get access token: ", error_content$error_description)
  }
  
  token_content <- content(response, "parsed")
  return(token_content$access_token)
}

# ============================================================
# FUNCTION: Upload file using correct SharePoint REST API
# ============================================================

upload_file_to_sharepoint <- function(local_file_path, remote_folder_path, access_token) {
  
  tryCatch({
    file_name <- basename(local_file_path)
    file_size <- file.info(local_file_path)$size
    cat("  📄", file_name, "(", round(file_size/1024, 2), "KB)\n")
    
    # Step 1: Get SharePoint site
    site_identifier <- paste0(site_hostname, ":/", site_path)
    graph_endpoint <- "https://graph.microsoft.com/v1.0"
    
    site_url <- paste0(graph_endpoint, "/sites/", site_identifier)
    site_response <- GET(
      site_url,
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(site_response) != 200) {
      stop("Failed to get site. Status: ", status_code(site_response))
    }
    
    site_content <- content(site_response, "parsed")
    site_id <- site_content$id
    cat("  ✓ Site found:", site_content$display_name, "\n")
    
    # Step 2: Get the default drive (document library)
    drive_url <- paste0(graph_endpoint, "/sites/", site_id, "/drive")
    drive_response <- GET(
      drive_url,
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(drive_response) != 200) {
      stop("Failed to get drive. Status: ", status_code(drive_response))
    }
    
    drive_content <- content(drive_response, "parsed")
    drive_id <- drive_content$id
    cat("  ✓ Using drive:", drive_content$name, "\n")
    
    # Step 3: Check if folder exists, navigate to it
    # URL encode the folder path
    encoded_folder <- URLencode(remote_folder_path, reserved = TRUE)
    
    # First, check if the folder exists
    folder_url <- paste0(graph_endpoint, "/drives/", drive_id, "/root:/", encoded_folder)
    folder_response <- GET(
      folder_url,
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(folder_response) == 404) {
      cat("  📁 Creating folder structure...\n")
      # Create folders recursively
      folder_parts <- strsplit(remote_folder_path, "/")[[1]]
      current_path <- ""
      
      for (part in folder_parts) {
        if (current_path == "") {
          current_path <- part
        } else {
          current_path <- paste0(current_path, "/", part)
        }
        
        encoded_current <- URLencode(current_path, reserved = TRUE)
        check_url <- paste0(graph_endpoint, "/drives/", drive_id, "/root:/", encoded_current)
        
        check_response <- GET(
          check_url,
          add_headers(Authorization = paste("Bearer", access_token))
        )
        
        if (status_code(check_response) == 404) {
          # Create folder using PUT method
          create_url <- paste0(graph_endpoint, "/drives/", drive_id, "/root:/", encoded_current)
          create_body <- list(
            name = part,
            folder = list(),
            "@microsoft.graph.conflictBehavior" = "rename"
          )
          
          create_response <- PUT(
            create_url,
            add_headers(
              Authorization = paste("Bearer", access_token),
              "Content-Type" = "application/json"
            ),
            body = toJSON(create_body, auto_unbox = TRUE)
          )
          
          if (status_code(create_response) >= 400) {
            stop("Failed to create folder: ", part)
          }
          cat("    Created folder:", part, "\n")
        }
      }
    }
    
    # Step 4: Upload the file using createUploadSession for large files
    # Both files are > 4MB, so we need session-based upload
    
    cat("  📤 Starting upload session...\n")
    
    # Create upload session
    session_url <- paste0(
      graph_endpoint, "/drives/", drive_id,
      "/root:/", encoded_folder, "/", file_name, ":/createUploadSession"
    )
    
    session_response <- POST(
      session_url,
      add_headers(
        Authorization = paste("Bearer", access_token),
        "Content-Type" = "application/json"
      ),
      body = "{}"
    )
    
    if (status_code(session_response) != 200) {
      stop("Failed to create upload session. Status: ", status_code(session_response))
    }
    
    session_content <- content(session_response, "parsed")
    upload_url <- session_content$uploadUrl
    
    # Upload file in chunks (10 MB chunks)
    chunk_size <- 10 * 1024 * 1024  # 10 MB
    file_content <- readBin(local_file_path, "raw", n = file_size)
    total_chunks <- ceiling(file_size / chunk_size)
    
    for (chunk in 1:total_chunks) {
      start_byte <- (chunk - 1) * chunk_size
      end_byte <- min(chunk * chunk_size - 1, file_size - 1)
      
      chunk_content <- file_content[(start_byte + 1):(end_byte + 1)]
      
      # Set Content-Range header
      content_range <- sprintf("bytes %d-%d/%d", start_byte, end_byte, file_size)
      
      chunk_response <- PUT(
        upload_url,
        add_headers(
          Authorization = paste("Bearer", access_token),
          "Content-Range" = content_range,
          "Content-Type" = "application/octet-stream"
        ),
        body = chunk_content
      )
      
      if (status_code(chunk_response) %in% c(200, 201, 202)) {
        cat("    Uploaded chunk", chunk, "of", total_chunks, "\r")
      } else {
        stop("Failed to upload chunk ", chunk, ". Status: ", status_code(chunk_response))
      }
    }
    
    cat("  ✅ Upload successful!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# SIMPLIFIED ALTERNATIVE: Use Microsoft365R package
# ============================================================

upload_with_microsoft365r <- function(local_file_path, site_url, folder_path) {
  
  tryCatch({
    if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
      install.packages("Microsoft365R")
    }
    
    library(Microsoft365R)
    
    # Connect to SharePoint site
    site <- get_sharepoint_site(site_url)
    
    # Get document library
    doc_lib <- site$get_drive()
    
    # Navigate to folder
    folder_parts <- strsplit(folder_path, "/")[[1]]
    current_item <- doc_lib
    
    for (part in folder_parts) {
      current_item <- current_item$get_item(part)
    }
    
    # Upload file
    current_item$upload_file(local_file_path)
    cat("  ✅ Upload successful!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ❌ Microsoft365R error:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# MAIN EXECUTION
# ============================================================

cat("\n📤 Starting SharePoint upload process...\n")
cat("============================================================\n\n")

# Get access token
cat("🔑 Authenticating with client credentials...\n")
access_token <- get_access_token(tenant_id, client_id, client_secret)
cat("✅ Authentication successful!\n\n")

# Display target location
full_url <- paste0("https://", site_hostname, "/", site_path, "/", folder_relative_path)
cat("📍 Target:", full_url, "\n\n")

# Try Microsoft365R method first (simpler for large files)
cat("💡 Attempting upload with Microsoft365R (simpler method)...\n")
cat("   This will open a browser window for authentication.\n\n")

site_full_url <- paste0("https://", site_hostname, "/", site_path)

# Try Microsoft365R with device authentication
success <- tryCatch({
  upload_with_microsoft365r(files_to_upload[1], site_full_url, folder_relative_path)
}, error = function(e) {
  cat("  Microsoft365R failed, falling back to Graph API...\n")
  FALSE
})

if (!success) {
  cat("\n📤 Using Graph API with chunked upload...\n")
  
  # Upload each file
  results <- list()
  
  for (file_path in files_to_upload) {
    cat("\nProcessing:", basename(file_path), "\n")
    
    if (!file.exists(file_path)) {
      cat("  ⚠️ File not found:", file_path, "\n")
      results[[basename(file_path)]] <- FALSE
      next
    }
    
    success <- upload_file_to_sharepoint(file_path, folder_relative_path, access_token)
    results[[basename(file_path)]] <- success
  }
  
  # Summary
  cat("\n============================================================\n")
  cat("📊 UPLOAD SUMMARY\n")
  cat("============================================================\n")
  
  for (file_name in names(results)) {
    status <- ifelse(results[[file_name]], "✅ SUCCESS", "❌ FAILED")
    cat(status, "-", file_name, "\n")
  }
}

cat("\n💡 TIP: If uploads continue to fail, try the interactive method:\n")
cat("   library(Microsoft365R)\n")
cat("   site <- get_sharepoint_site('https://worldhealthorg.sharepoint.com/sites/AF-pep')\n")
cat("   site$get_drive()$upload_file('path/to/file')\n")
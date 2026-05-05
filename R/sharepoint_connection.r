# ============================================================
# 📤 Upload files to SharePoint - SIMPLIFIED WORKING VERSION
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
# SIMPLE UPLOAD TO ROOT (MOST RELIABLE)
# ============================================================

upload_to_root <- function(local_file_path, access_token) {
  
  tryCatch({
    file_name <- basename(local_file_path)
    file_size <- file.info(local_file_path)$size
    cat("  📄", file_name, "(", round(file_size/1024, 2), "KB)\n")
    
    # Get site
    site_identifier <- paste0(site_hostname, ":/", site_path)
    graph_endpoint <- "https://graph.microsoft.com/v1.0"
    
    site_url <- paste0(graph_endpoint, "/sites/", site_identifier)
    site_response <- GET(
      site_url,
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(site_response) != 200) {
      stop("Failed to get site")
    }
    
    site_content <- content(site_response, "parsed")
    site_id <- site_content$id
    
    # Get drive
    drive_url <- paste0(graph_endpoint, "/sites/", site_id, "/drive")
    drive_response <- GET(
      drive_url,
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    drive_content <- content(drive_response, "parsed")
    drive_id <- drive_content$id
    
    # Upload directly to root (simplest approach)
    upload_url <- paste0(graph_endpoint, "/drives/", drive_id, "/root:/", file_name, ":/content")
    
    cat("  📤 Uploading to root of SharePoint...\n")
    
    # For files > 4MB, need session upload
    if (file_size > 4 * 1024 * 1024) {
      # Create upload session
      session_url <- paste0(graph_endpoint, "/drives/", drive_id, "/root:/", file_name, ":/createUploadSession")
      session_response <- POST(
        session_url,
        add_headers(Authorization = paste("Bearer", access_token)),
        body = "{}",
        encode = "raw"
      )
      
      if (status_code(session_response) != 200) {
        stop("Failed to create upload session")
      }
      
      session_content <- content(session_response, "parsed")
      upload_session_url <- session_content$uploadUrl
      
      # Upload in chunks
      chunk_size <- 10 * 1024 * 1024  # 10MB chunks
      file_conn <- file(local_file_path, "rb")
      file_content <- readBin(file_conn, "raw", n = file_size)
      close(file_conn)
      
      total_chunks <- ceiling(file_size / chunk_size)
      
      for (i in 1:total_chunks) {
        start_byte <- (i - 1) * chunk_size
        end_byte <- min(i * chunk_size - 1, file_size - 1)
        chunk_data <- file_content[(start_byte + 1):(end_byte + 1)]
        
        content_range <- sprintf("bytes %d-%d/%d", start_byte, end_byte, file_size)
        
        chunk_response <- PUT(
          upload_session_url,
          add_headers(
            "Content-Range" = content_range,
            "Content-Type" = "application/octet-stream"
          ),
          body = chunk_data
        )
        
        if (status_code(chunk_response) %in% c(200, 201, 202)) {
          cat("    Uploaded chunk", i, "of", total_chunks, "\r")
        } else {
          stop(paste("Chunk", i, "failed with status:", status_code(chunk_response)))
        }
      }
      cat("\n")
    } else {
      # Small file, direct upload
      file_content <- readBin(local_file_path, "raw", n = file_size)
      upload_response <- PUT(
        upload_url,
        add_headers(
          Authorization = paste("Bearer", access_token),
          "Content-Type" = "application/octet-stream"
        ),
        body = file_content
      )
      
      if (status_code(upload_response) != 201) {
        stop(paste("Upload failed with status:", status_code(upload_response)))
      }
    }
    
    cat("  ✅ Upload successful!\n")
    cat("  📍 Location: Root of Shared Documents\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# ALTERNATIVE: Use Microsoft365R with interactive login
# ============================================================

upload_interactive <- function() {
  cat("\n🔄 Attempting interactive upload with Microsoft365R...\n")
  cat("   A browser window will open for you to login.\n")
  cat("   Please login with your WHO credentials.\n\n")
  
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    install.packages("Microsoft365R")
  }
  
  library(Microsoft365R)
  
  tryCatch({
    # Connect to SharePoint site
    site <- get_sharepoint_site("https://worldhealthorg.sharepoint.com/sites/AF-pep")
    
    # Get document library
    doc_lib <- site$get_drive()
    
    # Navigate to the specific folder
    # Try to navigate to the target folder
    folder_path_parts <- c("GISWORKSPACE", "Shared Documents", "7. SIA_Data", "Data Repository")
    current_folder <- doc_lib
    
    for (folder in folder_path_parts) {
      tryCatch({
        current_folder <- current_folder$get_item(folder)
        cat("  ✓ Navigated to:", folder, "\n")
      }, error = function(e) {
        cat("  ⚠️ Folder", folder, "not found, creating...\n")
        current_folder <- current_folder$create_folder(folder)
      })
    }
    
    # Upload files
    for (file_path in files_to_upload) {
      if (file.exists(file_path)) {
        cat("\n  Uploading:", basename(file_path), "\n")
        current_folder$upload_file(file_path)
        cat("  ✅ Success!\n")
      }
    }
    
    cat("\n🎉 All files uploaded successfully!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ❌ Interactive upload failed:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# MAIN EXECUTION
# ============================================================

cat("\n📤 SharePoint Upload - Multiple Methods\n")
cat("============================================================\n\n")

# Method 1: Try root upload with app credentials first
cat("Method 1: Upload to root using app credentials\n")
cat("--------------------------------------------\n")

access_token <- get_access_token(tenant_id, client_id, client_secret)
cat("✅ Authentication successful\n\n")

results_root <- list()
for (file_path in files_to_upload) {
  cat("Processing:", basename(file_path), "\n")
  if (file.exists(file_path)) {
    success <- upload_to_root(file_path, access_token)
    results_root[[basename(file_path)]] <- success
  } else {
    cat("  ❌ File not found\n")
    results_root[[basename(file_path)]] <- FALSE
  }
  cat("\n")
}

if (all(unlist(results_root))) {
  cat("\n🎉 SUCCESS! Files uploaded to root of SharePoint.\n")
  cat("📍 Check: https://worldhealthorg.sharepoint.com/sites/AF-pep/Shared%20Documents\n")
} else {
  cat("\n⚠️ Root upload failed. Trying interactive method...\n\n")
  
  # Method 2: Interactive upload (most reliable for specific folders)
  cat("Method 2: Interactive upload to specific folder\n")
  cat("--------------------------------------------\n")
  success_interactive <- upload_interactive()
  
  if (!success_interactive) {
    cat("\n❌ Both methods failed.\n")
    cat("\n📋 MANUAL WORKAROUND:\n")
    cat("1. Open in browser:\n")
    cat("   https://worldhealthorg.sharepoint.com/sites/AF-pep/Shared%20Documents\n")
    cat("2. Navigate to or create: GISWORKSPACE → Shared Documents → 7. SIA_Data → Data Repository\n")
    cat("3. Upload these files manually:\n")
    for (file_path in files_to_upload) {
      if (file.exists(file_path)) {
        cat("   -", basename(file_path), "\n")
      }
    }
  }
}
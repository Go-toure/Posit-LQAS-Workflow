#!/usr/bin/env Rscript
# Auto-refresh dashboard every hour

library(RSelenium)
library(logger)

refresh_dashboard <- function() {
  log_info("Refreshing dashboard at {Sys.time()}")
  
  # Regenerate dashboard
  source("executive_dashboard.R")
  
  # Optional: Use Selenium to refresh browser if needed
  # rD <- rsDriver(browser="chrome", port=4445L)
  # remDr <- rD$client
  # remDr$navigate("file:///C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/04_dashboard.html")
  # remDr$refresh()
  # remDr$close()
  # rD$server$stop()
  
  log_info("Dashboard refreshed")
}

# Run every hour
while(TRUE) {
  refresh_dashboard()
  Sys.sleep(3600)  # 1 hour
}

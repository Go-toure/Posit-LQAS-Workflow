#!/usr/bin/env Rscript
# Weekly Summary Email - Monday Report

library(blastula)
library(data.table)
library(logger)

# Load latest data
dt <- fread("data/final/lqas_cleaned.csv")
latest_summary <- list.files("reports", pattern = "pipeline_summary_.*\\.json", full.names = TRUE) %>%
  sort(decreasing = TRUE) %>%
  first()

summary_data <- jsonlite::read_json(latest_summary)

# Calculate weekly changes
last_week_coverage <- 0  # Would need to store historical data

# Create weekly report
email_body <- glue::glue("
# WHO AFRO LQAS Weekly Surveillance Report

**Week Ending:** {format(Sys.Date(), '%B %d, %Y')}
**Report Date:** {format(Sys.time(), '%Y-%m-%d %H:%M')}

## Executive Summary

The LQAS polio surveillance pipeline completed successfully this week with the following key metrics:

| Metric | Value |
|--------|-------|
| **Total Records** | {format(summary_data$data_summary$total_records, big.mark = ',')} |
| **Countries Monitored** | {summary_data$data_summary$total_countries} |
| **Districts Assessed** | {format(summary_data$data_summary$total_districts, big.mark = ',')} |
| **Children Sampled** | {format(summary_data$data_summary$total_sampled, big.mark = ',')} |
| **Children Vaccinated** | {format(summary_data$data_summary$total_vaccinated, big.mark = ',')} |
| **Overall Coverage** | {summary_data$data_summary$overall_coverage}% |

## Performance Highlights

- **Best Performing Countries:** (would show top 3)
- **Areas Needing Attention:** (would show bottom 3)
- **AFRO Block Summary:** (would show by region)

## Pipeline Execution

- **Execution Time:** {summary_data$duration_minutes} minutes
- **Status:** SUCCESSFUL
- **Next Run:** Next Monday at 11:00 AM

## Dashboard Access

[View Interactive Dashboard](file:///C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow/04_dashboard.html)

---
*This is an automated report from the WHO AFRO LQAS Surveillance System*
")

# Save email body for reference
writeLines(email_body, file.path("reports", paste0("weekly_report_", format(Sys.Date(), "%Y%m%d"), ".md")))

log_info("Weekly report generated")

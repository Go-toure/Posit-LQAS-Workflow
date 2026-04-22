# Set working directory
setwd("C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow")

# Create the Rmd content as a single string
rmd_content <- '---
title: "LQAS Monitoring Dashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
    theme: flatly
    source_code: embed
    self_contained: true
---

```{r setup, include=FALSE}
library(flexdashboard)
library(ggplot2)
library(data.table)
library(DT)
library(arrow)

# Load parquet data
dt <- as.data.table(read_parquet("data/final/lqas_dashboard_input.parquet"))

# Metrics
total_records <- nrow(dt)
pass_count <- sum(dt$status == "Pass", na.rm = TRUE)
fail_count <- sum(dt$status == "Fail", na.rm = TRUE)
pass_rate <- round(pass_count / total_records * 100, 1)

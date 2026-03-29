#!/usr/bin/env Rscript
library(data.table)
library(ggplot2)

# Read data
dt <- fread("data/final/lqas_cleaned.csv")

# Create HTML output
html_content <- '
<!DOCTYPE html>
<html>
<head>
    <title>LQAS Dashboard</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .metric { display: inline-block; margin: 10px; padding: 15px; background: #f0f0f0; border-radius: 5px; min-width: 150px; }
        .metric-value { font-size: 24px; font-weight: bold; color: #2c3e50; }
        .metric-label { color: #7f8c8d; }
        table { border-collapse: collapse; width: 100%; margin-top: 20px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #3498db; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .chart-container { margin: 20px 0; }
    </style>
</head>
<body>
    <h1>LQAS Dashboard</h1>
    <div id="metrics">
'

# Calculate metrics
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
coverage <- round(total_vaccinated / total_sampled * 100, 1)

html_content <- paste0(html_content, '
        <div class="metric">
            <div class="metric-value">', format(total_records, big.mark = ","), '</div>
            <div class="metric-label">Total Records</div>
        </div>
        <div class="metric">
            <div class="metric-value">', total_countries, '</div>
            <div class="metric-label">Countries</div>
        </div>
        <div class="metric">
            <div class="metric-value">', format(total_districts, big.mark = ","), '</div>
            <div class="metric-label">Districts</div>
        </div>
        <div class="metric">
            <div class="metric-value">', format(total_sampled, big.mark = ","), '</div>
            <div class="metric-label">Children Sampled</div>
        </div>
        <div class="metric">
            <div class="metric-value">', format(total_vaccinated, big.mark = ","), '</div>
            <div class="metric-label">Children Vaccinated</div>
        </div>
        <div class="metric">
            <div class="metric-value">', coverage, '%</div>
            <div class="metric-label">Overall Coverage</div>
        </div>
    </div>
')

# Calculate country coverage
country_stats <- dt[, .(
    coverage = round(sum(total_vaccinated, na.rm = TRUE) / sum(total_sampled, na.rm = TRUE) * 100, 1)
), by = country][order(-coverage)]

html_content <- paste0(html_content, '
    <div class="chart-container">
        <h2>Coverage by Country</h2>
        <table>
            <tr><th>Country</th><th>Coverage (%)</th></tr>
')

for (i in 1:nrow(country_stats)) {
    html_content <- paste0(html_content, '
            <tr>
                <td>', country_stats$country[i], '</td>
                <td>', country_stats$coverage[i], '%</td>
            </tr>
    ')
}

html_content <- paste0(html_content, '
        </table>
    </div>
')

# Create data table
html_content <- paste0(html_content, '
    <div>
        <h2>Data Sample (First 100 rows)</h2>
        <table>
            <tr>
                <th>Country</th>
                <th>Province</th>
                <th>District</th>
                <th>Response</th>
                <th>Round</th>
                <th>Sampled</th>
                <th>Vaccinated</th>
                <th>Coverage</th>
            </tr>
')

sample_data <- head(dt, 100)
for (i in 1:nrow(sample_data)) {
    row_coverage <- round(sample_data$total_vaccinated[i] / sample_data$total_sampled[i] * 100, 1)
    html_content <- paste0(html_content, '
            <tr>
                <td>', sample_data$country[i], '</td>
                <td>', sample_data$province[i], '</td>
                <td>', sample_data$district[i], '</td>
                <td>', sample_data$response[i], '</td>
                <td>', sample_data$roundNumber[i], '</td>
                <td>', format(sample_data$total_sampled[i], big.mark = ","), '</td>
                <td>', format(sample_data$total_vaccinated[i], big.mark = ","), '</td>
                <td>', row_coverage, '%</td>
            </tr>
    ')
}

html_content <- paste0(html_content, '
        </table>
    </div>
</body>
</html>
')

# Write HTML file
writeLines(html_content, "04_dashboard.html")
cat("✅ Dashboard created: 04_dashboard.html\n")

#!/usr/bin/env Rscript
# Simple LQAS Dashboard
library(data.table)

cat("Loading data...\n")
dt <- fread("data/final/lqas_cleaned.csv")
cat(sprintf("Loaded %d rows\n", nrow(dt)))

# Calculate metrics
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
coverage <- round(total_vaccinated / total_sampled * 100, 1)

# Country data
country_data <- dt[, .(
    coverage = round(sum(total_vaccinated) / sum(total_sampled) * 100, 1)
), by = country][order(-coverage)]

# Top districts
top_districts <- dt[order(-total_vaccinated/total_sampled*100)][1:30, .(
    country, province, district,
    sampled = total_sampled,
    vaccinated = total_vaccinated,
    coverage = round(total_vaccinated/total_sampled*100, 1),
    status = ifelse(total_missed <= 3, "PASS", "FAIL")
)]

# Create HTML
html <- paste0('<!DOCTYPE html>
<html>
<head>
    <title>LQAS Dashboard</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .header { background: #2c3e50; color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; }
        .metrics { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-bottom: 20px; }
        .metric { background: white; padding: 15px; border-radius: 10px; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .metric-value { font-size: 28px; font-weight: bold; color: #2c3e50; }
        .metric-label { color: #7f8c8d; margin-top: 5px; }
        .chart-container { background: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; }
        table { width: 100%; border-collapse: collapse; background: white; border-radius: 10px; overflow: hidden; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
        th { background: #2c3e50; color: white; }
        .pass { color: green; font-weight: bold; }
        .fail { color: red; font-weight: bold; }
    </style>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
</head>
<body>
    <div class="header">
        <h1>LQAS Monitoring Dashboard</h1>
        <p>Polio Vaccination Campaign Performance | Updated: ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</p>
    </div>
    
    <div class="metrics">
        <div class="metric"><div class="metric-value">', format(total_records, big.mark = ","), '</div><div class="metric-label">Records</div></div>
        <div class="metric"><div class="metric-value">', total_countries, '</div><div class="metric-label">Countries</div></div>
        <div class="metric"><div class="metric-value">', format(total_districts, big.mark = ","), '</div><div class="metric-label">Districts</div></div>
        <div class="metric"><div class="metric-value">', format(total_sampled, big.mark = ","), '</div><div class="metric-label">Children Sampled</div></div>
        <div class="metric"><div class="metric-value">', format(total_vaccinated, big.mark = ","), '</div><div class="metric-label">Children Vaccinated</div></div>
        <div class="metric"><div class="metric-value">', coverage, '%</div><div class="metric-label">Overall Coverage</div></div>
    </div>
    
    <div class="chart-container">
        <h3>Coverage by Country</h3>
        <canvas id="countryChart" height="300"></canvas>
    </div>
    
    <div class="chart-container">
        <h3>Top 30 Districts by Coverage</h3>
        <table>
            <thead><tr><th>Country</th><th>Province</th><th>District</th><th>Coverage</th><th>Sampled</th><th>Vaccinated</th><th>Status</th></tr></thead>
            <tbody>')

for(i in 1:nrow(top_districts)) {
    html <- paste0(html, '<tr><td>', top_districts$country[i], '</td><td>', top_districts$province[i], '</td><td>', top_districts$district[i], '</td><td><strong>', top_districts$coverage[i], '%</strong></td><td>', format(top_districts$sampled[i], big.mark = ","), '</td><td>', format(top_districts$vaccinated[i], big.mark = ","), '</td><td class="', tolower(top_districts$status[i]), '">', top_districts$status[i], '</td></tr>')
}

html <- paste0(html, '</tbody></table></div>

<script>
    const countries = ', jsonlite::toJSON(country_data$country), ';
    const coverages = ', jsonlite::toJSON(country_data$coverage), ';
    new Chart(document.getElementById("countryChart"), {
        type: "bar",
        data: { labels: countries, datasets: [{ label: "Coverage (%)", data: coverages, backgroundColor: "rgba(44, 62, 80, 0.7)" }] },
        options: { scales: { y: { beginAtZero: true, max: 100 } } }
    });
</script>
</body>
</html>')

writeLines(html, "04_dashboard.html")
cat("Dashboard created: 04_dashboard.html\n")
cat(sprintf("Data: %s records, %d countries, %d districts, %.1f%% coverage\n", 
    format(total_records, big.mark = ","), total_countries, total_districts, coverage))

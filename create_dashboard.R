#!/usr/bin/env Rscript
# Create LQAS Dashboard
library(data.table)
library(ggplot2)

# Load data
cat("Loading data...\n")
dt <- fread("data/final/lqas_cleaned.csv")
cat(sprintf("Loaded %d rows, %d columns\n", nrow(dt), ncol(dt)))

# Calculate metrics
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
coverage <- round(total_vaccinated / total_sampled * 100, 1)

# Calculate coverage by country
country_coverage <- dt[, .(
    coverage = round(sum(total_vaccinated, na.rm = TRUE) / sum(total_sampled, na.rm = TRUE) * 100, 1),
    records = .N
), by = country][order(-coverage)]

# Get top 20 districts
top_districts <- dt[, .(
    country, province, district,
    sampled = total_sampled,
    vaccinated = total_vaccinated,
    coverage = round(total_vaccinated / total_sampled * 100, 1),
    status = ifelse(total_missed <= 3, "PASS", "FAIL")
)][order(-coverage)][1:20]

# Create HTML
cat("Creating HTML...\n")

html <- sprintf('<!DOCTYPE html>
<html>
<head>
    <title>LQAS Dashboard</title>
    <meta charset="UTF-8">
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: "Segoe UI", Arial, sans-serif; background: #f0f2f5; padding: 20px; }
        .container { max-width: 1400px; margin: 0 auto; }
        .header { background: linear-gradient(135deg, #1e3c72 0%%, #2a5298 100%%); color: white; padding: 30px; border-radius: 15px; margin-bottom: 25px; }
        .header h1 { font-size: 28px; margin-bottom: 10px; }
        .header p { opacity: 0.9; }
        .metrics { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 20px; margin-bottom: 30px; }
        .metric-card { background: white; padding: 20px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); text-align: center; transition: transform 0.2s; }
        .metric-card:hover { transform: translateY(-3px); }
        .metric-value { font-size: 32px; font-weight: bold; color: #1e3c72; }
        .metric-label { color: #666; margin-top: 8px; font-size: 14px; }
        .chart-container { background: white; padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        .chart-container h2 { color: #333; margin-bottom: 20px; font-size: 20px; border-left: 4px solid #1e3c72; padding-left: 15px; }
        table { width: 100%%; border-collapse: collapse; background: white; border-radius: 12px; overflow: hidden; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        th, td { padding: 12px 15px; text-align: left; border-bottom: 1px solid #eee; }
        th { background: #1e3c72; color: white; font-weight: 600; }
        tr:hover { background: #f8f9fa; }
        .pass { color: #28a745; font-weight: bold; }
        .fail { color: #dc3545; font-weight: bold; }
        .coverage-bar { background: #e9ecef; border-radius: 10px; height: 8px; overflow: hidden; margin-top: 8px; }
        .coverage-fill { background: linear-gradient(90deg, #28a745, #ffc107, #dc3545); height: 100%%; border-radius: 10px; width: 0%%; }
        .footer { text-align: center; padding: 20px; color: #666; font-size: 12px; margin-top: 30px; }
        @media (max-width: 768px) { .metrics { grid-template-columns: repeat(2, 1fr); } }
    </style>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
</head>
<body>
<div class="container">
    <div class="header">
        <h1>📊 LQAS Monitoring Dashboard</h1>
        <p>Polio Vaccination Campaign Performance | Last updated: %s</p>
    </div>
    
    <div class="metrics">
        <div class="metric-card"><div class="metric-value">%s</div><div class="metric-label">Total Records</div></div>
        <div class="metric-card"><div class="metric-value">%s</div><div class="metric-label">Countries</div></div>
        <div class="metric-card"><div class="metric-value">%s</div><div class="metric-label">Districts</div></div>
        <div class="metric-card"><div class="metric-value">%s</div><div class="metric-label">Children Sampled</div></div>
        <div class="metric-card"><div class="metric-value">%s</div><div class="metric-label">Children Vaccinated</div></div>
        <div class="metric-card"><div class="metric-value">%s%%</div><div class="metric-label">Overall Coverage</div></div>
    </div>
    
    <div class="chart-container">
        <h2>📈 Vaccination Coverage by Country</h2>
        <canvas id="coverageChart" height="300"></canvas>
    </div>
    
    <div class="chart-container">
        <h2>🏆 Top 20 Districts by Coverage</h2>
        <table>
            <thead><tr><th>Country</th><th>Province</th><th>District</th><th>Coverage</th><th>Sampled</th><th>Vaccinated</th><th>Status</th></tr></thead>
            <tbody id="districtsBody"></tbody>
        </table>
    </div>
    
    <div class="footer">
        <p>Source: LQAS Data | Data as of: %s</p>
    </div>
</div>

<script>
    // Country coverage data
    const countryNames = %s;
    const coverageValues = %s;
    
    // Create chart
    const ctx = document.getElementById("coverageChart").getContext("2d");
    new Chart(ctx, {
        type: "bar",
        data: {
            labels: countryNames,
            datasets: [{
                label: "Coverage (%)",
                data: coverageValues,
                backgroundColor: "rgba(30, 60, 114, 0.7)",
                borderColor: "rgba(30, 60, 114, 1)",
                borderWidth: 1,
                borderRadius: 5
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
            scales: {
                y: { beginAtZero: true, max: 100, title: { display: true, text: "Coverage (%)" } },
                x: { ticks: { autoSkip: false, maxRotation: 45, minRotation: 45 } }
            },
            plugins: { legend: { position: "top" }, tooltip: { callbacks: { label: function(t) { return t.raw + "%"; } } } }
        }
    });
    
    // District data
    const districts = %s;
    const tbody = document.getElementById("districtsBody");
    districts.forEach(d => {
        const row = tbody.insertRow();
        row.insertCell(0).textContent = d.country;
        row.insertCell(1).textContent = d.province;
        row.insertCell(2).textContent = d.district;
        row.insertCell(3).innerHTML = `<strong>${d.coverage}%</strong><div class="coverage-bar"><div class="coverage-fill" style="width: ${d.coverage}%%"></div></div>`;
        row.insertCell(4).textContent = d.sampled.toLocaleString();
        row.insertCell(5).textContent = d.vaccinated.toLocaleString();
        row.insertCell(6).innerHTML = `<span class="${d.status === "PASS" ? "pass" : "fail"}">${d.status}</span>`;
    });
</script>
</body>
</html>',
format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
format(total_records, big.mark = ","),
total_countries,
format(total_districts, big.mark = ","),
format(total_sampled, big.mark = ","),
format(total_vaccinated, big.mark = ","),
coverage,
format(Sys.time(), "%Y-%m-%d"),
jsonlite::toJSON(country_coverage$country),
jsonlite::toJSON(country_coverage$coverage),
jsonlite::toJSON(top_districts[, .(country, province, district, coverage, sampled, vaccinated, status)])
)

# Write file
writeLines(html, "04_dashboard.html")
cat("✅ Dashboard created: 04_dashboard.html\n")
cat(sprintf("   Records: %s | Countries: %d | Districts: %d | Coverage: %.1f%%\n", 
    format(total_records, big.mark = ","), total_countries, total_districts, coverage))

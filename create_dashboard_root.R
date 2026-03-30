#!/usr/bin/env Rscript
# Create LQAS Dashboard in Root Directory

library(data.table)

# Load data
cat("Loading data...\n")
dt <- fread("data/final/lqas_cleaned.csv")
cat(sprintf("Loaded %s rows\n", format(nrow(dt), big.mark = ",")))

# Calculate metrics
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
overall_coverage <- round(total_vaccinated / total_sampled * 100, 1)
pass_rate <- round(sum(dt$total_missed <= 3, na.rm = TRUE) / total_records * 100, 1)

# Calculate coverage by country
country_data <- dt[, .(
    coverage = round(sum(total_vaccinated, na.rm = TRUE) / sum(total_sampled, na.rm = TRUE) * 100, 1),
    districts = uniqueN(district)
), by = country][order(-coverage)]

# Get top 50 districts
top_districts <- dt[, .(
    country, province, district,
    sampled = total_sampled,
    vaccinated = total_vaccinated,
    coverage = round(total_vaccinated / total_sampled * 100, 1),
    missed = total_missed,
    status = ifelse(total_missed <= 3, "PASS", "FAIL"),
    performance = performance
)][order(-coverage)][1:50]

# Get AFRO block summary
afro_data <- dt[, .(
    coverage = round(sum(total_vaccinated, na.rm = TRUE) / sum(total_sampled, na.rm = TRUE) * 100, 1),
    districts = uniqueN(district)
), by = afro_block][order(-coverage)]

# Create HTML directly in root
html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LQAS Dashboard - Polio Vaccination Monitoring</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <link rel="stylesheet" href="https://cdn.datatables.net/1.11.5/css/jquery.dataTables.min.css">
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="https://cdn.datatables.net/1.11.5/js/jquery.dataTables.min.js"></script>
    <style>
        body { background: #f8f9fa; font-family: "Segoe UI", Arial, sans-serif; }
        .header { background: linear-gradient(135deg, #1a472a 0%%, #2e7d32 100%%); color: white; padding: 30px 0; margin-bottom: 30px; }
        .stat-card { background: white; border-radius: 15px; padding: 20px; text-align: center; box-shadow: 0 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px; transition: transform 0.2s; }
        .stat-card:hover { transform: translateY(-5px); }
        .stat-number { font-size: 36px; font-weight: bold; color: #2e7d32; }
        .stat-label { color: #6c757d; font-size: 14px; margin-top: 5px; }
        .chart-card { background: white; border-radius: 15px; padding: 20px; margin-bottom: 30px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .chart-title { font-size: 18px; font-weight: bold; margin-bottom: 20px; color: #333; border-left: 4px solid #2e7d32; padding-left: 15px; }
        .table-container { background: white; border-radius: 15px; padding: 20px; margin-bottom: 30px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .badge-pass { background-color: #28a745; color: white; padding: 5px 12px; border-radius: 20px; font-size: 12px; }
        .badge-fail { background-color: #dc3545; color: white; padding: 5px 12px; border-radius: 20px; font-size: 12px; }
        .footer { text-align: center; padding: 20px; color: #6c757d; font-size: 12px; border-top: 1px solid #dee2e6; margin-top: 30px; }
    </style>
</head>
<body>
    <div class="header">
        <div class="container">
            <h1 class="display-5">📊 LQAS Monitoring Dashboard</h1>
            <p class="lead">Polio Vaccination Campaign Performance | Updated: %s</p>
        </div>
    </div>
    
    <div class="container">
        <div class="row">
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s</div><div class="stat-label">Records</div></div></div>
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s</div><div class="stat-label">Countries</div></div></div>
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s</div><div class="stat-label">Districts</div></div></div>
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s</div><div class="stat-label">Children</div></div></div>
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s%%</div><div class="stat-label">Coverage</div></div></div>
            <div class="col-md-2 col-sm-4 col-6"><div class="stat-card"><div class="stat-number">%s%%</div><div class="stat-label">Pass Rate</div></div></div>
        </div>
        
        <div class="row">
            <div class="col-md-6">
                <div class="chart-card">
                    <div class="chart-title">🌍 Coverage by Country</div>
                    <canvas id="countryChart" height="300"></canvas>
                </div>
            </div>
            <div class="col-md-6">
                <div class="chart-card">
                    <div class="chart-title">🗺️ AFRO Block Performance</div>
                    <canvas id="afroChart" height="300"></canvas>
                </div>
            </div>
        </div>
        
        <div class="table-container">
            <div class="chart-title">🏆 Top 50 Districts by Coverage</div>
            <div style="overflow-x: auto;">
                <table id="districtsTable" class="display" style="width:100%%">
                    <thead>
                        <tr><th>Country</th><th>Province</th><th>District</th><th>Coverage</th><th>Sampled</th><th>Vaccinated</th><th>Missed</th><th>Status</th><th>Performance</th></tr>
                    </thead>
                    <tbody id="districtsBody"></tbody>
                </table>
            </div>
        </div>
    </div>
    
    <div class="footer">
        <p>Source: LQAS Data | Data as of: %s | Total Children: %s | Total Vaccinated: %s</p>
    </div>
    
    <script>
        // Country chart
        const countryNames = %s;
        const countryCoverage = %s;
        new Chart(document.getElementById("countryChart"), {
            type: "bar", data: { labels: countryNames, datasets: [{ label: "Coverage (%)", data: countryCoverage, backgroundColor: "rgba(46, 125, 50, 0.7)", borderColor: "#2e7d32", borderWidth: 1 }] },
            options: { responsive: true, maintainAspectRatio: true, scales: { y: { beginAtZero: true, max: 100, title: { display: true, text: "Coverage (%)" } } }, plugins: { tooltip: { callbacks: { label: function(t) { return t.raw + "%"; } } } } }
        });
        
        // AFRO chart
        const afroNames = %s;
        const afroCoverage = %s;
        new Chart(document.getElementById("afroChart"), {
            type: "bar", data: { labels: afroNames, datasets: [{ label: "Coverage (%)", data: afroCoverage, backgroundColor: "rgba(30, 60, 114, 0.7)", borderColor: "#1e3c72", borderWidth: 1 }] },
            options: { responsive: true, maintainAspectRatio: true, scales: { y: { beginAtZero: true, max: 100 } } }
        });
        
        // Districts table data
        const districts = %s;
        const tbody = document.getElementById("districtsBody");
        districts.forEach(d => {
            const row = tbody.insertRow();
            row.insertCell(0).textContent = d.country;
            row.insertCell(1).textContent = d.province;
            row.insertCell(2).textContent = d.district;
            row.insertCell(3).innerHTML = `<strong>${d.coverage}%</strong>`;
            row.insertCell(4).textContent = d.sampled.toLocaleString();
            row.insertCell(5).textContent = d.vaccinated.toLocaleString();
            row.insertCell(6).textContent = d.missed;
            row.insertCell(7).innerHTML = `<span class="badge-${d.status === "PASS" ? "pass" : "fail"}">${d.status}</span>`;
            row.insertCell(8).textContent = d.performance;
        });
        
        // Initialize DataTable
        $(document).ready(function() { $("#districtsTable").DataTable({ pageLength: 10, order: [[3, "desc"]], responsive: true }); });
    </script>
</body>
</html>',
format(Sys.time(), "%Y-%m-%d %H:%M"),
format(total_records, big.mark = ","),
total_countries,
format(total_districts, big.mark = ","),
format(total_sampled, big.mark = ","),
overall_coverage,
pass_rate,
format(Sys.time(), "%Y-%m-%d"),
format(total_sampled, big.mark = ","),
format(total_vaccinated, big.mark = ","),
jsonlite::toJSON(country_data$country),
jsonlite::toJSON(country_data$coverage),
jsonlite::toJSON(afro_data$afro_block),
jsonlite::toJSON(afro_data$coverage),
jsonlite::toJSON(top_districts[, .(country, province, district, coverage, sampled, vaccinated, missed, status, performance)])
)

# Write file to root
writeLines(html, "04_dashboard.html")
cat("\n✅ Dashboard created: 04_dashboard.html\n")
cat(sprintf("   📊 %s records | 🌍 %d countries | 🏙️ %d districts | 💉 %.1f%% coverage\n", 
    format(total_records, big.mark = ","), total_countries, total_districts, overall_coverage))

#!/usr/bin/env Rscript
# Attractive LQAS Dashboard
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
pass_rate <- round(sum(dt$total_missed <= 3, na.rm = TRUE) / total_records * 100, 1)

# Country data
country_data <- dt[, .(
    coverage = round(sum(total_vaccinated) / sum(total_sampled) * 100, 1),
    districts = uniqueN(district)
), by = country][order(-coverage)]

# AFRO block data
afro_data <- dt[, .(
    coverage = round(sum(total_vaccinated) / sum(total_sampled) * 100, 1),
    districts = uniqueN(district)
), by = afro_block][order(-coverage)]

# Top districts
top_districts <- dt[order(-total_vaccinated/total_sampled*100)][1:30, .(
    country, province, district,
    sampled = total_sampled,
    vaccinated = total_vaccinated,
    coverage = round(total_vaccinated/total_sampled*100, 1),
    missed = total_missed,
    status = ifelse(total_missed <= 3, "PASS", "FAIL"),
    performance = performance
)]

# Create HTML
html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LQAS Dashboard - Polio Vaccination Monitoring</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-datalabels@2.0.0"></script>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: "Inter", sans-serif; 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        .container { max-width: 1400px; margin: 0 auto; }
        
        /* Header */
        .header { 
            background: white; 
            border-radius: 20px; 
            padding: 30px; 
            margin-bottom: 30px;
            box-shadow: 0 10px 40px rgba(0,0,0,0.1);
        }
        .header h1 { 
            font-size: 32px; 
            color: #2d3748; 
            margin-bottom: 10px;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .header p { color: #718096; font-size: 14px; }
        .badge-date { 
            background: #e9ecef; 
            padding: 5px 12px; 
            border-radius: 20px; 
            font-size: 12px;
            display: inline-block;
            margin-top: 10px;
        }
        
        /* Metrics Grid */
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .metric-card {
            background: white;
            border-radius: 20px;
            padding: 20px;
            transition: all 0.3s ease;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            position: relative;
            overflow: hidden;
        }
        .metric-card:hover { transform: translateY(-5px); box-shadow: 0 10px 25px rgba(0,0,0,0.15); }
        .metric-icon {
            position: absolute;
            right: 20px;
            top: 20px;
            font-size: 40px;
            opacity: 0.2;
        }
        .metric-value { font-size: 32px; font-weight: bold; color: #2d3748; margin-bottom: 5px; }
        .metric-label { color: #718096; font-size: 14px; text-transform: uppercase; letter-spacing: 0.5px; }
        .metric-trend { font-size: 12px; margin-top: 10px; color: #48bb78; }
        
        /* Chart Cards */
        .chart-card {
            background: white;
            border-radius: 20px;
            padding: 25px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .chart-title {
            font-size: 18px;
            font-weight: 600;
            color: #2d3748;
            margin-bottom: 20px;
            display: flex;
            align-items: center;
            gap: 10px;
            border-left: 4px solid #667eea;
            padding-left: 15px;
        }
        
        /* Table */
        .table-container {
            background: white;
            border-radius: 20px;
            padding: 25px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            overflow-x: auto;
        }
        table { width: 100%; border-collapse: collapse; }
        th {
            text-align: left;
            padding: 15px;
            background: #f7fafc;
            color: #4a5568;
            font-weight: 600;
            font-size: 14px;
        }
        td {
            padding: 12px 15px;
            border-bottom: 1px solid #e2e8f0;
            font-size: 14px;
        }
        tr:hover { background: #f7fafc; }
        .status-pass { 
            background: #c6f6d5; 
            color: #22543d; 
            padding: 4px 12px; 
            border-radius: 20px; 
            font-size: 12px;
            font-weight: 600;
            display: inline-block;
        }
        .status-fail { 
            background: #fed7d7; 
            color: #742a2a; 
            padding: 4px 12px; 
            border-radius: 20px; 
            font-size: 12px;
            font-weight: 600;
            display: inline-block;
        }
        .performance-high { background: #c6f6d5; color: #22543d; padding: 4px 8px; border-radius: 12px; font-size: 11px; }
        .performance-moderate { background: #feebc8; color: #7b341e; padding: 4px 8px; border-radius: 12px; font-size: 11px; }
        .performance-poor { background: #fed7d7; color: #742a2a; padding: 4px 8px; border-radius: 12px; font-size: 11px; }
        .performance-very-poor { background: #e9d8fd; color: #44337a; padding: 4px 8px; border-radius: 12px; font-size: 11px; }
        
        .coverage-bar {
            background: #e2e8f0;
            border-radius: 10px;
            height: 6px;
            width: 100%;
            overflow: hidden;
            margin-top: 5px;
        }
        .coverage-fill {
            background: linear-gradient(90deg, #48bb78, #38a169);
            height: 100%;
            border-radius: 10px;
            transition: width 0.5s ease;
        }
        
        /* Two column layout */
        .two-columns {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 30px;
            margin-bottom: 30px;
        }
        
        /* Footer */
        .footer {
            text-align: center;
            padding: 20px;
            color: rgba(255,255,255,0.8);
            font-size: 12px;
            margin-top: 30px;
        }
        
        @media (max-width: 768px) {
            .two-columns { grid-template-columns: 1fr; }
            .metrics-grid { grid-template-columns: repeat(2, 1fr); }
        }
    </style>
</head>
<body>
<div class="container">
    <!-- Header -->
    <div class="header">
        <h1>
            <i class="fas fa-chart-line" style="color: #667eea;"></i>
            LQAS Monitoring Dashboard
        </h1>
        <p>Polio Vaccination Campaign Performance | Real-time monitoring and analysis</p>
        <div class="badge-date">
            <i class="far fa-calendar-alt"></i> Updated: ', format(Sys.time(), "%Y-%m-%d %H:%M"), '
        </div>
    </div>
    
    <!-- Metrics -->
    <div class="metrics-grid">
        <div class="metric-card">
            <i class="fas fa-database metric-icon"></i>
            <div class="metric-value">', format(total_records, big.mark = ","), '</div>
            <div class="metric-label">Total Records</div>
        </div>
        <div class="metric-card">
            <i class="fas fa-globe metric-icon"></i>
            <div class="metric-value">', total_countries, '</div>
            <div class="metric-label">Countries</div>
        </div>
        <div class="metric-card">
            <i class="fas fa-map-marker-alt metric-icon"></i>
            <div class="metric-value">', format(total_districts, big.mark = ","), '</div>
            <div class="metric-label">Districts</div>
        </div>
        <div class="metric-card">
            <i class="fas fa-child metric-icon"></i>
            <div class="metric-value">', format(total_sampled, big.mark = ","), '</div>
            <div class="metric-label">Children Sampled</div>
        </div>
        <div class="metric-card">
            <i class="fas fa-syringe metric-icon"></i>
            <div class="metric-value">', format(total_vaccinated, big.mark = ","), '</div>
            <div class="metric-label">Children Vaccinated</div>
        </div>
        <div class="metric-card">
            <i class="fas fa-heartbeat metric-icon"></i>
            <div class="metric-value">', coverage, '%</div>
            <div class="metric-label">Overall Coverage</div>
            <div class="metric-trend"><i class="fas fa-arrow-up"></i> ', pass_rate, '% Pass Rate</div>
        </div>
    </div>
    
    <!-- Charts Row -->
    <div class="two-columns">
        <div class="chart-card">
            <div class="chart-title">
                <i class="fas fa-chart-bar" style="color: #667eea;"></i>
                Coverage by Country
            </div>
            <canvas id="countryChart" height="300"></canvas>
        </div>
        <div class="chart-card">
            <div class="chart-title">
                <i class="fas fa-chart-pie" style="color: #667eea;"></i>
                AFRO Block Performance
            </div>
            <canvas id="afroChart" height="300"></canvas>
        </div>
    </div>
    
    <!-- Top Districts Table -->
    <div class="table-container">
        <div class="chart-title">
            <i class="fas fa-trophy" style="color: #fbbf24;"></i>
            Top 30 Districts by Coverage
        </div>
        <table>
            <thead>
                <tr>
                    <th>Country</th>
                    <th>Province</th>
                    <th>District</th>
                    <th>Coverage</th>
                    <th>Sampled</th>
                    <th>Vaccinated</th>
                    <th>Missed</th>
                    <th>Status</th>
                    <th>Performance</th>
                </tr>
            </thead>
            <tbody>')

for(i in 1:nrow(top_districts)) {
    perf_class <- switch(top_districts$performance[i],
        "high" = "performance-high",
        "moderate" = "performance-moderate",
        "poor" = "performance-poor",
        "very poor" = "performance-very-poor",
        "performance-moderate"
    )
    
    html <- paste0(html, '<tr>
        <td><strong>', top_districts$country[i], '</strong></td>
        <td>', top_districts$province[i], '</td>
        <td>', top_districts$district[i], '</td>
        <td>
            <strong>', top_districts$coverage[i], '%</strong>
            <div class="coverage-bar">
                <div class="coverage-fill" style="width: ', top_districts$coverage[i], '%;"></div>
            </div>
        </td>
        <td>', format(top_districts$sampled[i], big.mark = ","), '</td>
        <td>', format(top_districts$vaccinated[i], big.mark = ","), '</td>
        <td>', top_districts$missed[i], '</td>
        <td><span class="status-', tolower(top_districts$status[i]), '">', top_districts$status[i], '</span></td>
        <td><span class="', perf_class, '">', top_districts$performance[i], '</span></td>
    </tr>')
}

html <- paste0(html, '</tbody></table></div>

    <!-- Footer -->
    <div class="footer">
        <p><i class="far fa-chart-bar"></i> LQAS Data Analysis | Data as of ', format(Sys.time(), "%Y-%m-%d"), ' | Total Coverage: ', coverage, '%</p>
    </div>
</div>

<script>
    // Country chart
    const countries = ', jsonlite::toJSON(country_data$country), ';
    const coverages = ', jsonlite::toJSON(country_data$coverage), ';
    new Chart(document.getElementById("countryChart"), {
        type: "bar",
        data: {
            labels: countries,
            datasets: [{
                label: "Coverage (%)",
                data: coverages,
                backgroundColor: "rgba(102, 126, 234, 0.7)",
                borderColor: "rgba(102, 126, 234, 1)",
                borderWidth: 1,
                borderRadius: 8
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
            plugins: { legend: { position: "top" } },
            scales: {
                y: { beginAtZero: true, max: 100, title: { display: true, text: "Coverage (%)", font: { weight: "bold" } } },
                x: { ticks: { autoSkip: false, maxRotation: 45, minRotation: 45 } }
            }
        }
    });
    
    // AFRO Block chart
    const afroBlocks = ', jsonlite::toJSON(afro_data$afro_block), ';
    const afroCoverage = ', jsonlite::toJSON(afro_data$coverage), ';
    new Chart(document.getElementById("afroChart"), {
        type: "bar",
        data: {
            labels: afroBlocks,
            datasets: [{
                label: "Coverage (%)",
                data: afroCoverage,
                backgroundColor: "rgba(118, 75, 162, 0.7)",
                borderColor: "rgba(118, 75, 162, 1)",
                borderWidth: 1,
                borderRadius: 8
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
            scales: { y: { beginAtZero: true, max: 100, title: { display: true, text: "Coverage (%)" } } }
        }
    });
</script>
</body>
</html>')

writeLines(html, "04_dashboard.html")
cat("\n✅ Beautiful Dashboard Created!\n")
cat(sprintf("   📊 %s records | 🌍 %d countries | 🏙️ %d districts | 💉 %.1f%% coverage\n", 
    format(total_records, big.mark = ","), total_countries, total_districts, coverage))
cat("   🎨 Dashboard saved as: 04_dashboard.html\n")

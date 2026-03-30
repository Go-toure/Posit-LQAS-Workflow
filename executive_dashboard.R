#!/usr/bin/env Rscript
# Executive LQAS Dashboard - WHO Senior Management Edition
library(data.table)
library(ggplot2)

cat("Loading LQAS data...\n")
dt <- fread("data/final/lqas_cleaned.csv")
cat(sprintf("Loaded %s records\n", format(nrow(dt), big.mark = ",")))

# Calculate metrics
total_records <- nrow(dt)
total_countries <- uniqueN(dt$country)
total_districts <- uniqueN(dt$district)
total_sampled <- sum(dt$total_sampled, na.rm = TRUE)
total_vaccinated <- sum(dt$total_vaccinated, na.rm = TRUE)
coverage <- round(total_vaccinated / total_sampled * 100, 1)
pass_rate <- round(sum(dt$total_missed <= 3, na.rm = TRUE) / total_records * 100, 1)

# Date range
date_min <- min(as.Date(dt$lqas_start_date), na.rm = TRUE)
date_max <- max(as.Date(dt$lqas_start_date), na.rm = TRUE)

# Country performance
country_data <- dt[, .(
    coverage = round(sum(total_vaccinated) / sum(total_sampled) * 100, 1),
    districts = uniqueN(district),
    sampled = sum(total_sampled, na.rm = TRUE),
    vaccinated = sum(total_vaccinated, na.rm = TRUE),
    pass_rate = round(sum(total_missed <= 3, na.rm = TRUE) / .N * 100, 1)
), by = country][order(-coverage)]

# AFRO block data
afro_data <- dt[, .(
    coverage = round(sum(total_vaccinated) / sum(total_sampled) * 100, 1),
    districts = uniqueN(district),
    sampled = sum(total_sampled, na.rm = TRUE),
    vaccinated = sum(total_vaccinated, na.rm = TRUE)
), by = afro_block][order(-coverage)]

# Performance distribution
performance_dist <- dt[, .(
    count = .N,
    percentage = round(.N / total_records * 100, 1)
), by = performance][order(factor(performance, levels = c("high", "moderate", "poor", "very poor")))]

# Top performing districts
top_districts <- dt[order(-total_vaccinated/total_sampled*100)][1:25, .(
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
    <title>WHO AFRO | LQAS Polio Surveillance Dashboard</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        
        body {
            font-family: "Inter", sans-serif;
            background: #f0f4f8;
            color: #1a2c3e;
            line-height: 1.5;
        }
        
        /* WHO Blue Color Scheme */
        :root {
            --who-blue: #0078a8;
            --who-dark-blue: #005a82;
            --who-green: #009639;
            --who-orange: #f68b1f;
            --who-red: #e30613;
            --who-gray: #eef2f5;
            --who-dark-gray: #6c7a89;
        }
        
        /* Header */
        .header {
            background: linear-gradient(135deg, #005a82 0%, #0078a8 100%);
            color: white;
            padding: 35px 0;
            margin-bottom: 30px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }
        .header-container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 0 30px;
        }
        .header h1 {
            font-size: 28px;
            font-weight: 600;
            letter-spacing: -0.5px;
            margin-bottom: 8px;
        }
        .header h1 i {
            margin-right: 12px;
            color: #f68b1f;
        }
        .header p {
            font-size: 14px;
            opacity: 0.9;
        }
        .who-badge {
            background: rgba(255,255,255,0.2);
            padding: 8px 16px;
            border-radius: 30px;
            display: inline-block;
            margin-top: 15px;
            font-size: 12px;
            font-weight: 500;
        }
        
        /* Main Container */
        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 0 30px;
        }
        
        /* KPI Cards */
        .kpi-grid {
            display: grid;
            grid-template-columns: repeat(6, 1fr);
            gap: 20px;
            margin-bottom: 30px;
        }
        .kpi-card {
            background: white;
            border-radius: 20px;
            padding: 20px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.05);
            border: 1px solid #e9ecef;
            transition: all 0.2s ease;
            position: relative;
            overflow: hidden;
        }
        .kpi-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 25px rgba(0,0,0,0.1);
        }
        .kpi-icon {
            font-size: 32px;
            color: var(--who-blue);
            margin-bottom: 12px;
            opacity: 0.7;
        }
        .kpi-value {
            font-size: 32px;
            font-weight: 700;
            color: #1a2c3e;
            line-height: 1.2;
            margin-bottom: 5px;
        }
        .kpi-label {
            font-size: 12px;
            font-weight: 500;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            color: var(--who-dark-gray);
        }
        .kpi-trend {
            font-size: 11px;
            margin-top: 8px;
            color: var(--who-green);
        }
        
        /* Two Column Layout */
        .two-col {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 25px;
            margin-bottom: 25px;
        }
        
        /* Cards */
        .card {
            background: white;
            border-radius: 20px;
            padding: 25px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.05);
            border: 1px solid #e9ecef;
        }
        .card-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 20px;
            padding-bottom: 15px;
            border-bottom: 2px solid #f0f4f8;
        }
        .card-title {
            font-size: 16px;
            font-weight: 600;
            color: #1a2c3e;
        }
        .card-title i {
            color: var(--who-blue);
            margin-right: 8px;
        }
        .card-subtitle {
            font-size: 12px;
            color: var(--who-dark-gray);
        }
        
        /* Table */
        .table-wrapper {
            overflow-x: auto;
            margin-top: 15px;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            font-size: 13px;
        }
        th {
            text-align: left;
            padding: 12px 10px;
            background: #f8fafc;
            font-weight: 600;
            color: #1a2c3e;
            border-bottom: 2px solid #e2e8f0;
        }
        td {
            padding: 12px 10px;
            border-bottom: 1px solid #eef2f5;
        }
        tr:hover {
            background: #fafcff;
        }
        
        /* Status Badges */
        .badge-pass {
            background: #e6f7e6;
            color: var(--who-green);
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 11px;
            font-weight: 600;
            display: inline-block;
        }
        .badge-fail {
            background: #fee;
            color: var(--who-red);
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 11px;
            font-weight: 600;
            display: inline-block;
        }
        
        /* Performance Colors */
        .perf-high { color: var(--who-green); font-weight: 600; }
        .perf-moderate { color: var(--who-orange); font-weight: 600; }
        .perf-poor { color: var(--who-red); font-weight: 600; }
        .perf-very-poor { color: #8b5cf6; font-weight: 600; }
        
        /* Coverage Bar */
        .coverage-wrapper {
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .coverage-value {
            font-weight: 600;
            min-width: 45px;
        }
        .coverage-bar-container {
            flex: 1;
            background: #e9ecef;
            border-radius: 20px;
            height: 6px;
            overflow: hidden;
        }
        .coverage-bar {
            background: linear-gradient(90deg, var(--who-green), #2ecc71);
            height: 100%;
            border-radius: 20px;
            transition: width 0.3s ease;
        }
        
        /* Stat Highlight */
        .stat-highlight {
            text-align: center;
            padding: 15px;
            background: #f8fafc;
            border-radius: 12px;
        }
        .stat-number {
            font-size: 28px;
            font-weight: 700;
            color: var(--who-blue);
        }
        .stat-label-sm {
            font-size: 11px;
            color: var(--who-dark-gray);
            margin-top: 5px;
        }
        
        /* Footer */
        .footer {
            margin-top: 40px;
            padding: 20px 0;
            text-align: center;
            border-top: 1px solid #e2e8f0;
            font-size: 12px;
            color: var(--who-dark-gray);
        }
        
        @media (max-width: 1200px) {
            .kpi-grid { grid-template-columns: repeat(3, 1fr); }
            .two-col { grid-template-columns: 1fr; }
        }
        @media (max-width: 768px) {
            .kpi-grid { grid-template-columns: repeat(2, 1fr); }
            .container { padding: 0 20px; }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-container">
            <h1><i class="fas fa-chart-line"></i> LQAS Polio Surveillance Dashboard</h1>
            <p>Lot Quality Assurance Sampling | Real-time Vaccination Coverage Monitoring</p>
            <div class="who-badge">
                <i class="fas fa-calendar-alt"></i> Period: ', format(date_min, "%b %Y"), ' - ', format(date_max, "%b %Y"), '
            </div>
        </div>
    </div>
    
    <div class="container">
        <!-- KPI Grid -->
        <div class="kpi-grid">
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-globe-africa"></i></div>
                <div class="kpi-value">', total_countries, '</div>
                <div class="kpi-label">Countries</div>
                <div class="kpi-trend"><i class="fas fa-flag-checkered"></i> Active Surveillance</div>
            </div>
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-map-marker-alt"></i></div>
                <div class="kpi-value">', format(total_districts, big.mark = ","), '</div>
                <div class="kpi-label">Districts</div>
                <div class="kpi-trend"><i class="fas fa-chart-line"></i> Assessed</div>
            </div>
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-child"></i></div>
                <div class="kpi-value">', format(total_sampled, big.mark = ","), '</div>
                <div class="kpi-label">Children Sampled</div>
                <div class="kpi-trend"><i class="fas fa-users"></i> Target Population</div>
            </div>
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-syringe"></i></div>
                <div class="kpi-value">', format(total_vaccinated, big.mark = ","), '</div>
                <div class="kpi-label">Vaccinated</div>
                <div class="kpi-trend"><i class="fas fa-check-circle"></i> Protected</div>
            </div>
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-heartbeat"></i></div>
                <div class="kpi-value">', coverage, '%</div>
                <div class="kpi-label">Coverage Rate</div>
                <div class="kpi-trend"><i class="fas fa-chart-line"></i> ', ifelse(coverage >= 90, "On Track", ifelse(coverage >= 75, "Moderate", "Alert")), '</div>
            </div>
            <div class="kpi-card">
                <div class="kpi-icon"><i class="fas fa-clipboard-check"></i></div>
                <div class="kpi-value">', pass_rate, '%</div>
                <div class="kpi-label">Pass Rate</div>
                <div class="kpi-trend"><i class="fas fa-', ifelse(pass_rate >= 80, "arrow-up", "arrow-down"), '"></i> LQAS Standard</div>
            </div>
        </div>
        
        <!-- Country & AFRO Block Performance -->
        <div class="two-col">
            <div class="card">
                <div class="card-header">
                    <div class="card-title"><i class="fas fa-chart-bar"></i> Country Performance</div>
                    <div class="card-subtitle">Vaccination Coverage by Country</div>
                </div>
                <canvas id="countryChart" height="280"></canvas>
            </div>
            <div class="card">
                <div class="card-header">
                    <div class="card-title"><i class="fas fa-chart-pie"></i> AFRO Block Performance</div>
                    <div class="card-subtitle">Coverage by WHO Region</div>
                </div>
                <canvas id="afroChart" height="280"></canvas>
            </div>
        </div>
        
        <!-- Performance Distribution -->
        <div class="card" style="margin-bottom: 25px;">
            <div class="card-header">
                <div class="card-title"><i class="fas fa-chart-simple"></i> Performance Distribution</div>
                <div class="card-subtitle">District-Level Performance Categories</div>
            </div>
            <div style="display: flex; gap: 20px; flex-wrap: wrap;">
                ', paste0(sapply(1:nrow(performance_dist), function(i) {
                    colors <- c("#009639", "#f68b1f", "#e30613", "#8b5cf6")
                    sprintf('
                <div style="flex: 1; text-align: center; padding: 15px; background: #f8fafc; border-radius: 12px;">
                    <div style="font-size: 28px; font-weight: 700; color: %s;">%s%%</div>
                    <div style="font-size: 12px; color: #6c7a89; margin-top: 5px;">%s</div>
                    <div style="font-size: 11px; color: #95a5a6;">(%s districts)</div>
                </div>', 
                    colors[i], performance_dist$percentage[i], 
                    toupper(performance_dist$performance[i]), 
                    format(performance_dist$count[i], big.mark = ","))
                }), collapse = ""), '
            </div>
        </div>
        
        <!-- Top Performing Districts -->
        <div class="card">
            <div class="card-header">
                <div class="card-title"><i class="fas fa-trophy"></i> Top 25 Performing Districts</div>
                <div class="card-subtitle">Highest Vaccination Coverage</div>
            </div>
            <div class="table-wrapper">
                <table>
                    <thead>
                        <tr><th>Country</th><th>Province</th><th>District</th><th>Coverage</th><th>Children</th><th>Vaccinated</th><th>Missed</th><th>Status</th><th>Performance</th></tr>
                    </thead>
                    <tbody>')

for(i in 1:nrow(top_districts)) {
    perf_class <- switch(top_districts$performance[i],
        "high" = "perf-high",
        "moderate" = "perf-moderate",
        "poor" = "perf-poor",
        "very poor" = "perf-very-poor",
        ""
    )
    perf_text <- switch(top_districts$performance[i],
        "high" = "High",
        "moderate" = "Moderate",
        "poor" = "Poor",
        "very poor" = "Very Poor",
        ""
    )
    
    html <- paste0(html, ' 
        <tr>
            <td><strong>', top_districts$country[i], '</strong></td>
            <td>', top_districts$province[i], '</td>
            <td>', top_districts$district[i], '</td>
            <td>
                <div class="coverage-wrapper">
                    <span class="coverage-value">', top_districts$coverage[i], '%</span>
                    <div class="coverage-bar-container">
                        <div class="coverage-bar" style="width: ', top_districts$coverage[i], '%;"></div>
                    </div>
                </div>
            </td>
            <td>', format(top_districts$sampled[i], big.mark = ","), '</td>
            <td>', format(top_districts$vaccinated[i], big.mark = ","), '</td>
            <td>', top_districts$missed[i], '</td>
            <td><span class="badge-', tolower(top_districts$status[i]), '">', top_districts$status[i], '</span></td>
            <td class="', perf_class, '">', perf_text, '</td>
        </tr>')
}

html <- paste0(html, '
                    </tbody>
                </table>
            </div>
        </div>
        
        <!-- Footer -->
        <div class="footer">
            <p><i class="fas fa-chart-line"></i> WHO AFRO LQAS Polio Surveillance System | Data updated: ', format(Sys.time(), "%Y-%m-%d %H:%M"), ' | Overall Coverage: ', coverage, '% | Pass Rate: ', pass_rate, '%</p>
            <p style="margin-top: 8px; font-size: 11px;">Source: LQAS Field Data | Analysis based on Lot Quality Assurance Sampling methodology</p>
        </div>
    </div>
    
    <script>
        // Country Chart
        const countries = ', jsonlite::toJSON(country_data$country), ';
        const countryCoverage = ', jsonlite::toJSON(country_data$coverage), ';
        new Chart(document.getElementById("countryChart"), {
            type: "bar",
            data: {
                labels: countries,
                datasets: [{
                    label: "Coverage (%)",
                    data: countryCoverage,
                    backgroundColor: "rgba(0, 120, 168, 0.7)",
                    borderColor: "#0078a8",
                    borderWidth: 1,
                    borderRadius: 6
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: true,
                plugins: { legend: { position: "top" } },
                scales: {
                    y: { beginAtZero: true, max: 100, title: { display: true, text: "Coverage (%)", font: { weight: "bold" } }, grid: { color: "#eef2f5" } },
                    x: { ticks: { autoSkip: false, maxRotation: 45, minRotation: 45 }, grid: { display: false } }
                }
            }
        });
        
        // AFRO Block Chart
        const afroBlocks = ', jsonlite::toJSON(afro_data$afro_block), ';
        const afroCoverage = ', jsonlite::toJSON(afro_data$coverage), ';
        new Chart(document.getElementById("afroChart"), {
            type: "bar",
            data: {
                labels: afroBlocks,
                datasets: [{
                    label: "Coverage (%)",
                    data: afroCoverage,
                    backgroundColor: "rgba(0, 150, 57, 0.7)",
                    borderColor: "#009639",
                    borderWidth: 1,
                    borderRadius: 6
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
cat("\n✅ WHO Executive Dashboard Created!\n")
cat(sprintf("   📊 %s records | 🌍 %d countries | 🏙️ %d districts\n", 
    format(total_records, big.mark = ","), total_countries, total_districts))
cat(sprintf("   💉 Coverage: %.1f%% | ✅ Pass Rate: %.1f%%\n", coverage, pass_rate))
cat("   🎯 Dashboard optimized for WHO Senior Management\n")

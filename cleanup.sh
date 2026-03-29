#!/bin/bash
cd "C:/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow"

echo "=========================================="
echo "LQAS Workflow Directory Cleanup"
echo "=========================================="

# 1. Rename files with wrong extensions
echo "📝 Renaming files with correct extensions..."

if [ -f "R/02_process_lqas.r" ]; then
    mv R/02_process_lqas.r R/02_process_lqas.R
    echo "  ✅ Renamed: 02_process_lqas.r → 02_process_lqas.R"
else
    echo "  ⏭️  02_process_lqas.R already has correct extension"
fi

if [ -f "R/04_dashboard.md" ]; then
    mv R/04_dashboard.md R/04_dashboard.Rmd
    echo "  ✅ Renamed: 04_dashboard.md → 04_dashboard.Rmd"
else
    echo "  ⏭️  04_dashboard.Rmd already has correct extension"
fi

# 2. Remove unnecessary special case files (handled by 02_process_lqas.R)
echo ""
echo "🗑️  Removing redundant special case files..."

if [ -f "R/special_272.R" ]; then
    rm -f R/special_272.R
    echo "  ✅ Removed: special_272.R (handled by 02_process_lqas.R)"
fi

if [ -f "R/special_nigeria_csv.R" ]; then
    rm -f R/special_nigeria_csv.R
    echo "  ✅ Removed: special_nigeria_csv.R (handled by 02_process_lqas.R)"
fi

# 3. Create main pipeline orchestrator (optional - can run scripts directly)
echo ""
echo "📄 Checking for run_pipeline.R..."

if [ ! -f "run_pipeline.R" ]; then
    cat > run_pipeline.R << 'EOF'
#!/usr/bin/env Rscript
# ============================================================
# LQAS Pipeline Orchestrator
# Runs the complete workflow: fetch → process → clean → dashboard → QC
# ============================================================

library(logger)
library(fs)

# Setup logging
dir_create("logs")
log_appender(appender_file("logs/pipeline.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting LQAS Pipeline")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
force_full <- "--force-full" %in% args
skip_fetch <- "--skip-fetch" %in% args

# Step 1: Fetch data from ONA
if (!skip_fetch) {
    log_info("📡 STEP 1: Fetching data from ONA...")
    fetch_cmd <- paste("python fetch_ona_data.py", if(force_full) "--force-full")
    system(fetch_cmd)
    log_info("✅ Data fetch complete")
} else {
    log_info("⏭️  Skipping data fetch (--skip-fetch)")
}

# Step 2: Process raw data
log_info("\n🔧 STEP 2: Processing LQAS data...")
source("R/02_process_lqas.R")
process_lqas_data(force_full_run = force_full)
log_info("✅ Processing complete")

# Step 3: Clean geonames
log_info("\n🌍 STEP 3: Cleaning geonames...")
source("R/03_clean_geonames.R")
log_info("✅ Geoname cleaning complete")

# Step 4: Generate dashboard
log_info("\n📊 STEP 4: Generating dashboard...")
rmarkdown::render("R/04_dashboard.Rmd", 
                  output_file = "reports/dashboards/lqas_dashboard.html",
                  quiet = FALSE)
log_info("✅ Dashboard generated")

# Step 5: Quality control
log_info("\n🔍 STEP 5: Running quality control...")
source("R/05_quality_control.R")
log_info("✅ Quality control complete")

log_info("\n" %>% paste(rep("=", 60), collapse = ""))
log_info("🎉 PIPELINE COMPLETED SUCCESSFULLY!")
log_info("=" %>% paste(rep("=", 60), collapse = ""))
EOF
    echo "  ✅ Created: run_pipeline.R"
else
    echo "  ⏭️  run_pipeline.R already exists"
fi

# Make run_pipeline.R executable (on Unix-like systems)
if [[ "$OSTYPE" == "linux-gnu"* ]] || [[ "$OSTYPE" == "darwin"* ]]; then
    chmod +x run_pipeline.R
fi

# 4. Clean up old/duplicate files (optional)
echo ""
echo "🧹 Cleaning up old files..."

# Remove backup files if they exist
if [ -f "R/process_lqas_backup.R" ]; then
    rm -f R/process_lqas_backup.R
    echo "  ✅ Removed: process_lqas_backup.R"
fi

if [ -f "R/process_all_parquet.R" ]; then
    rm -f R/process_all_parquet.R
    echo "  ✅ Removed: process_all_parquet.R"
fi

if [ -f "R/01_fetch_data_simple.R" ]; then
    rm -f R/01_fetch_data_simple.R
    echo "  ✅ Removed: 01_fetch_data_simple.R"
fi

# 5. Remove old QS files (optional - to save space)
echo ""
echo "💾 Checking for old QS files..."

QS_COUNT=$(ls -1 data/raw/*.qs 2>/dev/null | wc -l)
if [ $QS_COUNT -gt 0 ]; then
    echo "  Found $QS_COUNT .qs files in data/raw/"
    read -p "  Remove old .qs files? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -f data/raw/*.qs
        echo "  ✅ Removed all .qs files"
    else
        echo "  ⏭️  Kept .qs files"
    fi
else
    echo "  No .qs files found"
fi

# 6. Verify directory structure
echo ""
echo "📁 Verifying directory structure..."

# Create any missing directories
for dir in data/raw data/processed data/final data/lookup data/metadata logs reports/dashboards; do
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
        echo "  ✅ Created: $dir"
    fi
done

# 7. Summary
echo ""
echo "=========================================="
echo "✅ DIRECTORY CLEANUP COMPLETE!"
echo "=========================================="
echo ""
echo "📊 Final file structure:"
echo "  ├── run_pipeline.R              # Main orchestrator"
echo "  ├── fetch_ona_data.py           # Data fetching"
echo "  ├── config/config.yml           # Configuration"
echo "  ├── R/"
echo "  │   ├── 02_process_lqas.R       # Main processing (handles all cases)"
echo "  │   ├── 03_clean_geonames.R     # Geoname cleaning"
echo "  │   ├── 04_dashboard.Rmd        # Dashboard"
echo "  │   ├── 05_quality_control.R    # QC checks"
echo "  │   └── utils/helpers.R         # Utilities"
echo "  └── data/"
echo "      ├── raw/                     # Parquet files"
echo "      ├── processed/               # Intermediate CSV"
echo "      └── final/                   # Cleaned output"
echo ""
echo "=========================================="
echo "🚀 NEXT STEPS:"
echo "=========================================="
echo ""
echo "1. Update config/config.yml with your settings"
echo ""
echo "2. Run the complete pipeline:"
echo "   Rscript run_pipeline.R"
echo ""
echo "   Or run steps individually:"
echo "   python fetch_ona_data.py --force-full"
echo "   Rscript R/02_process_lqas.R"
echo "   Rscript R/03_clean_geonames.R"
echo "   Rscript -e \"rmarkdown::render('R/04_dashboard.Rmd')\""
echo "   Rscript R/05_quality_control.R"
echo ""
echo "3. View the dashboard:"
echo "   open reports/dashboards/lqas_dashboard.html"
echo ""

# Optional: Run a quick test
read -p "Run a quick test with form 4500? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo ""
    echo "🧪 Running quick test..."
    python fetch_ona_data.py --form-ids 4500
    Rscript R/02_process_lqas.R
    echo ""
    echo "✅ Quick test complete!"
fi
#!/bin/bash
# Monday Morning Auto-Run Script

echo "========================================"
echo "Monday LQAS Pipeline - Auto Run"
echo "========================================"
echo "Date: $(date)"
echo ""

# Pull latest changes
echo "📥 Pulling latest from GitHub..."
git pull origin main

# Run pipeline
echo ""
echo "🚀 Running LQAS Pipeline..."
Rscript run_pipeline.R

# Check if pipeline succeeded
if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Pipeline completed"
    
    # Check for changes
    if [[ -n $(git status -s) ]]; then
        echo ""
        echo "📝 Changes detected, pushing to GitHub..."
        git add data/final/lqas_cleaned.csv
        git add data/final/lqas_cleaned.parquet
        git add 04_dashboard.html
        git commit -m "Monday update: $(date +'%Y-%m-%d %H:%M')"
        git push origin main
        echo "✅ Changes pushed"
    fi
    
    # Open dashboard
    echo ""
    echo "📊 Opening dashboard..."
    start 04_dashboard.html
else
    echo ""
    echo "❌ Pipeline failed"
fi

echo ""
echo "========================================"
echo "Monday run complete"
echo "========================================"

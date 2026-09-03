#!/bin/bash
# Git Sync Script for LQAS Pipeline

echo "========================================"
echo "LQAS Pipeline - Git Sync"
echo "========================================"
echo ""

# Run pipeline
# --force-full: without this, fetch_ona_data.py skips any form whose
# raw file already exists on disk, so a scheduled run can go weeks
# without ever pulling fresh ONA data. Always force a full refetch here.
# Full path to Rscript.exe: a Task Scheduler / Git Bash environment
# does not reliably have R's bin directory on PATH.
echo "🚀 Running LQAS Pipeline..."
"/c/Program Files/R/R-4.4.1/bin/x64/Rscript.exe" run_pipeline.R --force-full

# Check if pipeline succeeded
if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Pipeline completed successfully"
    
    # Check for changes
    if [[ -n $(git status -s) ]]; then
        echo ""
        echo "📝 Changes detected, committing..."
        
        # Add files
        git add data/final/lqas_cleaned.csv
        git add data/final/lqas_cleaned.parquet
        git add 04_dashboard.html
        git add logs/*.log
        
        # Commit with date
        git commit -m "Monday update: $(date +'%Y-%m-%d %H:%M')"
        
        # Push to GitHub
        echo ""
        echo "📤 Pushing to GitHub..."
        git push origin main
        
        echo ""
        echo "✅ Changes pushed to GitHub"
    else
        echo ""
        echo "📭 No changes detected"
    fi
else
    echo ""
    echo "❌ Pipeline failed, not syncing"
    exit 1
fi

echo ""
echo "========================================"
echo "Git Sync Complete"
echo "========================================"

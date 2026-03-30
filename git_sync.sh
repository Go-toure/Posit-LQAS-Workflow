#!/bin/bash
# Git Sync Script for LQAS Pipeline

echo "========================================"
echo "LQAS Pipeline - Git Sync"
echo "========================================"
echo ""

# Run pipeline
echo "🚀 Running LQAS Pipeline..."
Rscript run_pipeline.R

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

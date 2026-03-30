#!/bin/bash
# Download the enhanced LQAS dashboard

echo "Downloading enhanced dashboard..."
curl -o R/04_dashboard.Rmd https://gist.githubusercontent.com/raw/your-gist-id/04_dashboard.Rmd
echo "Done! Dashboard saved to R/04_dashboard.Rmd"

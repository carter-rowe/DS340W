#!/usr/bin/env Rscript

# Quick script to refresh 2025 NFL data with current stats
cat("🔄 Refreshing 2025 NFL data...\n")

# Remove stale data
if (file.exists("data/team_metrics_2025.csv")) {
  file.remove("data/team_metrics_2025.csv")
  cat("✅ Removed stale data file\n")
}

# Load pipeline and refresh
source("code/data_enhancement/live_2025_pipeline.R")

# Create fresh data
cat("📊 Creating fresh team metrics...\n")
fresh_metrics <- create_live_2025_prediction_data()

cat("✅ Data refresh complete! Ravens should now reflect current 1-5 record.\n")
cat("📈 Run the predictor again to see updated rankings.\n")
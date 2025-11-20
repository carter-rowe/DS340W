#!/usr/bin/env Rscript

# Launch Script for XGBoost NFL Predictor App
# Run this script to start the interactive interface

cat("🚀 Launching XGBoost NFL Predictor Interface...\n\n")

# Check if model exists
if (!file.exists("models/xgboost_nfl_model.rds")) {
  cat("⚠️  WARNING: No trained model found!\n")
  cat("The app will load, but you'll need to train the model first.\n")
  cat("Use the 'Model Performance' tab to retrain.\n\n")
}

# Set working directory to the script location
setwd(dirname(sys.frame(1)$ofile))

# Launch the Shiny app
cat("📱 Starting Shiny app...\n")
cat("The app will open in your default web browser.\n")
cat("Press Ctrl+C or Cmd+C to stop the app.\n\n")

shiny::runApp("xgboost_predictor_app.R", launch.browser = TRUE)

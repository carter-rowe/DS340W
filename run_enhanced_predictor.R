#!/usr/bin/env Rscript

# Simple launcher for Enhanced NFL Predictor
# Suppresses warnings and provides clean startup

cat("🚀 Starting Enhanced NFL Predictor...\n")

# Suppress all warnings during startup
options(warn = -1)

# Load required libraries quietly
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(dplyr)
  library(readr)
})

# Load the enhanced predictor (try fixed version first)
predictor_loaded <- FALSE

tryCatch({
  source("code/prediction_engine/fixed_enhanced_predictor.R")
  cat("✅ Fixed enhanced predictor loaded successfully!\n")
  predictor_loaded <- TRUE
}, error = function(e) {
  cat("⚠️  Fixed predictor failed, trying safe version...\n")

  tryCatch({
    source("code/prediction_engine/simple_enhanced_predictor.R")
    cat("✅ Safe enhanced predictor loaded successfully!\n")
    predictor_loaded <- TRUE
  }, error = function(e2) {
    cat("❌ Error loading predictors:", e2$message, "\n")
    cat("💡 Make sure you're in the DS340W directory\n")
    stop()
  })
})

if (!predictor_loaded) {
  stop("Could not load any predictor version")
}

# Reset warning level for normal operation
options(warn = 0)

cat("🎯 Opening prediction interface...\n")
cat("📱 The app will open in your browser\n")
cat("🛑 Press Ctrl+C to stop the server\n\n")

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  runApp(list(ui = ui, server = server), port = 8080, host = "127.0.0.1")
}
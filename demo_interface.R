#!/usr/bin/env Rscript

# Demo script showing the 2025 NFL prediction interface functionality
library(dplyr)
library(readr)

# Load prediction functions
source("code/prediction_engine/nfl_2025_predictor.R")

cat("🏈 2025-2026 NFL PREDICTION INTERFACE DEMO 🏈\n")
cat("============================================\n\n")

# Show available teams (simulating dropdown menu)
cat("📋 AVAILABLE TEAMS (Dropdown Options):\n")
cat("=====================================\n")
for (i in 1:nrow(nfl_teams)) {
  cat(sprintf("%2d. %s - %s\n", i, nfl_teams$code[i], nfl_teams$name[i]))
}

cat("\n" , paste(rep("=", 50), collapse=""), "\n")
cat("🎮 INTERACTIVE PREDICTION DEMO\n")
cat(paste(rep("=", 50), collapse=""), "\n\n")

# Load team metrics
team_metrics <- load_prediction_data()

# Demo function simulating user interface
demo_prediction <- function(home_team, away_team) {
  cat(sprintf("🏠 HOME TEAM: %s (%s)\n", 
              nfl_teams[nfl_teams$code == home_team, "name"], home_team))
  cat(sprintf("🛫 AWAY TEAM: %s (%s)\n", 
              nfl_teams[nfl_teams$code == away_team, "name"], away_team))
  cat("\n🔮 CALCULATING PREDICTION...\n")
  
  # Make prediction
  prediction <- predict_2025_game(home_team, away_team, team_metrics)
  
  # Display results (simulating interface output)
  cat("\n" , paste(rep("🏈", 20), collapse=" "), "\n")
  cat("        PREDICTION RESULTS\n")
  cat(paste(rep("🏈", 20), collapse=" "), "\n\n")
  
  winner_name <- nfl_teams[nfl_teams$code == prediction$predicted_winner, "name"]
  
  cat("🏆 PREDICTED WINNER:\n")
  cat(sprintf("   %s (%s)\n\n", winner_name, prediction$predicted_winner))
  
  cat("📊 PREDICTED SCORE:\n")
  cat(sprintf("   %s: %d points\n", home_team, prediction$home_score))
  cat(sprintf("   %s: %d points\n", away_team, prediction$away_score))
  cat(sprintf("   Margin: %.1f points\n\n", abs(prediction$predicted_margin)))
  
  cat("🎯 WIN PROBABILITY:\n")
  cat(sprintf("   %s: %.1f%%\n", home_team, prediction$home_win_prob * 100))
  cat(sprintf("   %s: %.1f%%\n", away_team, prediction$away_win_prob * 100))
  cat(sprintf("   Confidence: %s\n\n", prediction$confidence))
  
  cat("🔍 KEY FACTORS:\n")
  cat(sprintf("   EPA Advantage: %+.3f\n", prediction$key_factors$epa_advantage))
  cat(sprintf("   Defensive Advantage: %+.3f\n", prediction$key_factors$def_advantage))
  cat(sprintf("   Recent Trend: %+.3f\n", prediction$key_factors$trend_advantage))
  cat(sprintf("   Overall Strength Diff: %+.3f\n", prediction$key_factors$overall_strength))
  
  cat("\n" , paste(rep("=", 60), collapse=""), "\n\n")
}

# Demo several matchups
cat("🎯 SAMPLE PREDICTIONS:\n\n")

demo_prediction("KC", "BUF")
demo_prediction("SF", "PHI") 
demo_prediction("BAL", "CIN")

cat("✅ INTERFACE FEATURES DEMONSTRATED:\n")
cat("==================================\n")
cat("✓ Dropdown team selection (32 NFL teams)\n")
cat("✓ Predicted winner identification\n")
cat("✓ Exact score prediction for both teams\n")
cat("✓ Winning margin calculation\n")
cat("✓ Win probability percentages\n")
cat("✓ Confidence levels (High/Medium/Low)\n")
cat("✓ Key factor analysis\n")
cat("✓ Time-weighted data (2024 season emphasized)\n")
cat("✓ Recent trend bias (last 4 games of 2024)\n\n")

cat("🖥️ TO LAUNCH FULL INTERFACE:\n")
cat("shiny::runApp('code/prediction_engine/nfl_2025_predictor.R')\n\n")

cat("🏈 Your 2025-2026 NFL prediction system is complete and ready! 🏈\n")
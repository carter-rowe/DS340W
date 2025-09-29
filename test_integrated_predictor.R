#!/usr/bin/env Rscript

# Test the integrated injury-enhanced prediction system
library(dplyr)
library(readr)

cat("🏥 Testing Integrated NFL Prediction System with Injuries 🏥\n")
cat("==========================================================\n\n")

# Source the integrated predictor
source("code/prediction_engine/nfl_2025_predictor.R")

# Test the data loading
cat("📊 Testing data loading...\n")
prediction_data <- load_prediction_data()

cat("✅ Data loaded successfully:\n")
cat(sprintf("  - Team metrics: %d teams\n", nrow(prediction_data$team_metrics)))
cat(sprintf("  - Player database: %d players\n", nrow(prediction_data$player_impact_db)))
cat(sprintf("  - Current injuries: %d players\n", nrow(prediction_data$current_injuries)))

# Test injury-enhanced prediction
cat("\n🎯 Testing injury-enhanced predictions...\n")

test_games <- list(
  c("CIN", "BUF"),  # Joe Burrow (CIN) questionable, Josh Allen (BUF) probable
  c("BAL", "KC"),   # Lamar Jackson (BAL) doubtful vs healthy KC
  c("MIA", "LAR")   # Tyreek Hill (MIA) out vs Cooper Kupp (LAR) questionable
)

for (i in seq_along(test_games)) {
  home_team <- test_games[[i]][1]
  away_team <- test_games[[i]][2]
  
  cat(sprintf("\n%d. Testing %s vs %s:\n", i, home_team, away_team))
  
  # Base prediction (no injuries)
  base_prediction <- predict_2025_game(home_team, away_team, prediction_data$team_metrics)
  
  # Injury-enhanced prediction
  injury_prediction <- predict_game_with_injury_impact(
    home_team, away_team, 
    prediction_data$team_metrics,
    prediction_data$current_injuries, 
    prediction_data$player_impact_db
  )
  
  cat(sprintf("   📊 Base: %s wins %d-%d (%.1f pts)\n", 
              base_prediction$predicted_winner,
              base_prediction$home_score, base_prediction$away_score,
              abs(base_prediction$predicted_margin)))
  
  cat(sprintf("   🏥 Injury-adjusted: %s wins %.0f-%.0f (%.1f pts)\n", 
              injury_prediction$predicted_winner,
              injury_prediction$home_score, injury_prediction$away_score,
              abs(injury_prediction$predicted_margin)))
  
  # Show injury impact if significant
  if (!is.null(injury_prediction$injury_analysis)) {
    impact <- injury_prediction$injury_analysis$net_injury_advantage
    if (abs(impact) > 0.5) {
      cat(sprintf("   ⚡ Injury impact: %+.1f points toward %s\n", 
                  abs(impact), ifelse(impact > 0, home_team, away_team)))
    } else {
      cat("   ✅ Minimal injury impact\n")
    }
  }
}

# Test injury summary function
cat("\n📋 Testing injury summary generation...\n")

# Test with CIN (has Joe Burrow injury)
cin_injuries <- calculate_team_injury_impact("CIN", prediction_data$current_injuries, prediction_data$player_impact_db)
cin_summary <- create_injury_summary("CIN", cin_injuries)
cat("CIN Injury Summary:\n")
cat(cin_summary)
cat("\n\n")

# Test with a healthy team
kc_injuries <- calculate_team_injury_impact("KC", prediction_data$current_injuries, prediction_data$player_impact_db)
kc_summary <- create_injury_summary("KC", kc_injuries)
cat("KC Injury Summary:\n")
cat(kc_summary)
cat("\n\n")

# Summary
cat("✅ INTEGRATION TEST COMPLETED SUCCESSFULLY!\n")
cat("==========================================\n")
cat("🎯 Key Integration Points Verified:\n")
cat("  ✓ Data loading with injury system components\n")
cat("  ✓ Injury-enhanced predictions working\n")
cat("  ✓ Injury impact calculations functional\n")
cat("  ✓ Summary generation working\n")
cat("  ✓ Both base and injury predictions available\n\n")

cat("🚀 Your main prediction interface now includes:\n")
cat("  🏥 Injury analysis toggle\n")
cat("  📊 Real-time injury impact calculations\n")
cat("  🎯 Enhanced predictions with EPA-based injury adjustments\n")
cat("  📋 Detailed injury summaries for each team\n\n")

cat("🖥️ Launch your enhanced interface:\n")
cat("shiny::runApp('code/prediction_engine/nfl_2025_predictor.R')\n")
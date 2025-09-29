#!/usr/bin/env Rscript

# Test script for 2025 NFL predictions
library(dplyr)
library(readr)

# Load the prediction function
source("code/prediction_engine/nfl_2025_predictor.R")

cat("🏈 Testing 2025-2026 NFL Prediction System 🏈\n")
cat("============================================\n\n")

# Load team data
team_metrics <- load_prediction_data()
cat("✅ Loaded data for", nrow(team_metrics), "teams\n\n")

# Test some high-profile 2025 matchups
test_games <- list(
  c("KC", "BUF"),     # AFC Championship rematch
  c("SF", "PHI"),     # NFC powerhouses  
  c("BAL", "CIN"),    # AFC North rivalry
  c("DAL", "GB"),     # Classic matchup
  c("MIA", "NYJ")     # AFC East battle
)

cat("📊 SAMPLE 2025-2026 PREDICTIONS:\n")
cat("=================================\n\n")

for (i in seq_along(test_games)) {
  home_team <- test_games[[i]][1]
  away_team <- test_games[[i]][2]
  
  prediction <- predict_2025_game(home_team, away_team, team_metrics)
  
  home_name <- nfl_teams[nfl_teams$code == home_team, "name"]
  away_name <- nfl_teams[nfl_teams$code == away_team, "name"]
  
  cat(sprintf("%d. %s (home) vs %s (away)\n", i, home_team, away_team))
  cat(sprintf("   🏆 Winner: %s\n", prediction$predicted_winner))
  cat(sprintf("   📊 Score: %d - %d (Margin: %.1f)\n", 
              prediction$home_score, prediction$away_score, abs(prediction$predicted_margin)))
  cat(sprintf("   🎯 Win Prob: %.1f%% (%s confidence)\n", 
              max(prediction$home_win_prob, prediction$away_win_prob) * 100, prediction$confidence))
  cat(sprintf("   🔑 Key: EPA Adv=%.3f, Def Adv=%.3f\n\n", 
              prediction$key_factors$epa_advantage, prediction$key_factors$def_advantage))
}

# Show top 10 teams by strength
cat("🏆 TOP 10 TEAMS FOR 2025-2026:\n")
cat("==============================\n")
top_teams <- team_metrics %>%
  arrange(desc(team_strength)) %>%
  head(10) %>%
  mutate(rank = row_number())

for (i in 1:nrow(top_teams)) {
  team_code <- top_teams$team[i]
  team_name <- nfl_teams[nfl_teams$code == team_code, "name"]
  strength <- round(top_teams$team_strength[i], 3)
  epa_off <- round(top_teams$off_epa_per_play[i], 3)
  epa_def <- round(top_teams$def_epa_per_play[i], 3)
  
  cat(sprintf("%2d. %s (%s) - Strength: %+.3f (Off: %+.3f, Def: %+.3f)\n", 
              i, team_code, team_name, strength, epa_off, epa_def))
}

cat("\n✨ 2025-2026 NFL Predictor is ready!\n")
cat("🖥️  Launch interface: shiny::runApp('code/prediction_engine/nfl_2025_predictor.R')\n")
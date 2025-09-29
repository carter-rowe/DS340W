library(rstan)
library(nflreadr)
library(dplyr)
library(readr)

# Simple NFL Game Predictor
# Load the fitted model
cat("Loading fitted model...\n")
model <- readRDS("stan_results/simple_enhanced_model.rds")
posterior <- extract(model)

# Function to get recent team metrics (simplified version)
get_team_epa_metrics <- function(team, season = 2024, weeks_back = 4) {
  
  # For demonstration, we'll use sample data
  # In a real application, this would fetch current season data
  
  # Sample EPA values based on team strength (simplified)
  team_ratings <- list(
    "KC" = list(off_epa = 0.15, def_epa = -0.05),
    "BUF" = list(off_epa = 0.12, def_epa = -0.02),
    "SF" = list(off_epa = 0.10, def_epa = -0.04),
    "BAL" = list(off_epa = 0.08, def_epa = -0.01),
    "PHI" = list(off_epa = 0.09, def_epa = 0.01),
    "DAL" = list(off_epa = 0.06, def_epa = 0.02),
    "MIA" = list(off_epa = 0.04, def_epa = 0.03),
    "CIN" = list(off_epa = 0.07, def_epa = 0.00),
    "MIN" = list(off_epa = 0.03, def_epa = 0.04),
    "DET" = list(off_epa = 0.08, def_epa = 0.01),
    "SEA" = list(off_epa = 0.02, def_epa = 0.05),
    "LAC" = list(off_epa = 0.05, def_epa = 0.02),
    "GB" = list(off_epa = 0.04, def_epa = 0.03),
    "TB" = list(off_epa = 0.01, def_epa = 0.06),
    "JAX" = list(off_epa = -0.02, def_epa = 0.08),
    "NYJ" = list(off_epa = -0.03, def_epa = 0.07),
    "LV" = list(off_epa = -0.01, def_epa = 0.09),
    "TEN" = list(off_epa = -0.04, def_epa = 0.10),
    "IND" = list(off_epa = 0.00, def_epa = 0.05),
    "CLE" = list(off_epa = -0.02, def_epa = 0.08),
    "NO" = list(off_epa = -0.05, def_epa = 0.11),
    "ATL" = list(off_epa = -0.03, def_epa = 0.09),
    "LAR" = list(off_epa = 0.02, def_epa = 0.06),
    "PIT" = list(off_epa = 0.01, def_epa = 0.04),
    "HOU" = list(off_epa = 0.03, def_epa = 0.07),
    "WAS" = list(off_epa = -0.01, def_epa = 0.08),
    "NYG" = list(off_epa = -0.06, def_epa = 0.12),
    "ARI" = list(off_epa = -0.04, def_epa = 0.10),
    "DEN" = list(off_epa = -0.02, def_epa = 0.09),
    "CHI" = list(off_epa = -0.08, def_epa = 0.15),
    "CAR" = list(off_epa = -0.10, def_epa = 0.18),
    "NE" = list(off_epa = -0.07, def_epa = 0.13)
  )
  
  if (team %in% names(team_ratings)) {
    return(team_ratings[[team]])
  } else {
    return(list(off_epa = 0, def_epa = 0))
  }
}

# Function to predict game outcome
predict_nfl_game <- function(home_team, away_team) {
  
  cat(sprintf("Predicting: %s (home) vs %s (away)\n", home_team, away_team))
  
  # Get team metrics
  home_metrics <- get_team_epa_metrics(home_team)
  away_metrics <- get_team_epa_metrics(away_team)
  
  # Calculate EPA differences
  epa_diff <- home_metrics$off_epa - away_metrics$off_epa
  def_epa_diff <- away_metrics$def_epa - home_metrics$def_epa  # Lower allowed is better
  
  cat(sprintf("EPA differential: %.3f (offense), %.3f (defense)\n", epa_diff, def_epa_diff))
  
  # Use model coefficients to predict
  home_advantage <- mean(posterior$alpha)
  epa_coefficient <- mean(posterior$beta_epa)
  def_coefficient <- mean(posterior$beta_def)
  model_sigma <- mean(posterior$sigma)
  
  # Predicted margin
  predicted_margin <- home_advantage + (epa_coefficient * epa_diff) + (def_coefficient * def_epa_diff)
  
  # Calculate win probability (assuming normal distribution)
  win_prob_home <- pnorm(predicted_margin / model_sigma)
  
  # Create prediction summary
  prediction <- list(
    home_team = home_team,
    away_team = away_team,
    predicted_margin = round(predicted_margin, 1),
    home_win_prob = round(win_prob_home, 3),
    away_win_prob = round(1 - win_prob_home, 3),
    predicted_winner = ifelse(predicted_margin > 0, home_team, away_team),
    confidence = ifelse(abs(predicted_margin) > 7, "High", 
                       ifelse(abs(predicted_margin) > 3, "Medium", "Low"))
  )
  
  return(prediction)
}

# Function to display prediction nicely
display_prediction <- function(prediction) {
  cat("\n" , paste(rep("=", 50), collapse=""), "\n")
  cat("🏈 NFL GAME PREDICTION 🏈\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  cat(sprintf("🏠 %s (Home) vs 🛫 %s (Away)\n", 
              prediction$home_team, prediction$away_team))
  cat("\n")
  
  cat("📊 PREDICTION RESULTS:\n")
  cat(sprintf("🏆 Predicted Winner: %s\n", prediction$predicted_winner))
  cat(sprintf("📈 Predicted Margin: %s points\n", 
              abs(prediction$predicted_margin)))
  cat(sprintf("🎯 Win Probability: %s (%.1f%%) vs %s (%.1f%%)\n",
              prediction$home_team, prediction$home_win_prob * 100,
              prediction$away_team, prediction$away_win_prob * 100))
  cat(sprintf("🔍 Confidence Level: %s\n", prediction$confidence))
  
  cat(paste(rep("=", 50), collapse=""), "\n\n")
}

# Interactive prediction function
interactive_predictor <- function() {
  
  teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
             "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
             "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG",
             "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")
  
  cat("🏈 Welcome to the NFL Game Predictor! 🏈\n")
  cat("Available teams:", paste(teams, collapse=", "), "\n\n")
  
  while(TRUE) {
    cat("Enter home team (or 'quit' to exit): ")
    home_team <- toupper(trimws(readline()))
    
    if (home_team == "QUIT") {
      cat("Thanks for using the NFL Predictor! 🏈\n")
      break
    }
    
    if (!home_team %in% teams) {
      cat("Invalid team. Please use 3-letter abbreviations (e.g., KC, BUF, SF)\n")
      next
    }
    
    cat("Enter away team: ")
    away_team <- toupper(trimws(readline()))
    
    if (!away_team %in% teams) {
      cat("Invalid team. Please use 3-letter abbreviations (e.g., KC, BUF, SF)\n")
      next
    }
    
    if (home_team == away_team) {
      cat("Teams cannot play themselves!\n")
      next
    }
    
    # Make prediction
    prediction <- predict_nfl_game(home_team, away_team)
    display_prediction(prediction)
  }
}

# Example predictions
cat("🏈 NFL Enhanced Prediction System Ready! 🏈\n")
cat("Model trained on EPA data from nflreadr\n")
cat("Key findings from model:\n")
cat(sprintf("- Home field advantage: %.1f points\n", mean(posterior$alpha)))
cat(sprintf("- EPA impact: ~%.0f points per 0.1 EPA advantage\n", mean(posterior$beta_epa) * 0.1))
cat("\n")

# Run some example predictions
cat("📈 Example Predictions:\n")

example_games <- list(
  c("KC", "BUF"),
  c("SF", "PHI"), 
  c("DAL", "GB"),
  c("BAL", "CIN")
)

for (game in example_games) {
  prediction <- predict_nfl_game(game[1], game[2])
  display_prediction(prediction)
}

cat("🎮 To run interactive predictor, call: interactive_predictor()\n")

# Make the function available for interactive use
if (interactive()) {
  interactive_predictor()
}
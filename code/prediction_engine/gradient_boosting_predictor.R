library(xgboost)
library(dplyr)
library(readr)
library(caret)
library(Matrix)

# Gradient Boosting NFL Prediction Model
# Phase 1 implementation as secondary model to complement Bayesian approach

# Function to prepare data for XGBoost
prepare_xgboost_data <- function(team_metrics, injury_data = NULL) {

  cat("Preparing data for gradient boosting model...\n")

  # Convert team metrics to matrix format
  feature_cols <- c(
    # Core EPA metrics
    "off_epa_per_play", "def_epa_per_play", "off_success_rate", "def_success_rate_allowed",

    # Enhanced passing metrics
    "pass_epa_per_play", "pass_epa_per_dropback", "cpoe", "epa_cpoe_composite",
    "avg_depth_of_target", "deep_pass_rate", "short_pass_efficiency", "deep_pass_efficiency",
    "avg_time_to_throw", "quick_pass_rate",

    # Enhanced rushing metrics
    "rush_epa_per_play", "rush_success_rate", "explosive_rush_rate",

    # Enhanced defensive metrics
    "pass_def_epa_per_play", "pass_def_epa_per_dropback", "def_cpoe_allowed",
    "sack_rate", "qb_hit_rate", "deep_pass_def_epa", "short_pass_def_epa",
    "rush_def_epa_per_play", "rush_def_success_rate", "explosive_rush_allowed_rate",

    # Situational metrics
    "rz_efficiency", "rz_epa_per_play", "third_down_rate", "third_down_epa",
    "late_game_epa", "two_min_epa", "goal_to_go_td_rate",
    "rz_def_efficiency", "rz_def_epa_per_play", "third_def_rate", "third_def_epa",
    "late_game_def_epa", "two_min_def_epa",

    # Special teams
    "st_off_epa", "st_def_epa", "fg_epa_offense", "punt_epa_offense",

    # Trends
    "off_trend", "def_trend", "cpoe_trend", "late_game_trend", "rz_trend"
  )

  # Select only available columns
  available_cols <- intersect(feature_cols, names(team_metrics))

  if (length(available_cols) == 0) {
    stop("No feature columns found in team_metrics data")
  }

  # Create feature matrix
  feature_matrix <- team_metrics[, available_cols, drop = FALSE]

  # Handle missing values
  for (col in names(feature_matrix)) {
    if (any(is.na(feature_matrix[[col]]))) {
      median_val <- median(feature_matrix[[col]], na.rm = TRUE)
      feature_matrix[[col]][is.na(feature_matrix[[col]])] <- median_val
    }
  }

  return(list(
    features = as.matrix(feature_matrix),
    feature_names = available_cols,
    team_names = team_metrics$team
  ))
}

# Train gradient boosting model on historical data
train_xgboost_model <- function(training_data, training_outcomes) {

  cat("Training XGBoost gradient boosting model...\n")

  # XGBoost parameters optimized for NFL prediction
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,                    # Learning rate
    max_depth = 6,                # Tree depth
    min_child_weight = 3,         # Minimum sum of instance weight
    subsample = 0.8,              # Subsample ratio
    colsample_bytree = 0.8,       # Feature sampling ratio
    lambda = 1,                   # L2 regularization
    alpha = 0,                    # L1 regularization
    gamma = 0                     # Minimum split loss
  )

  # Convert to DMatrix for XGBoost
  dtrain <- xgb.DMatrix(data = training_data$features, label = training_outcomes)

  # Train model with cross-validation
  set.seed(42)
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 1,
    print_every_n = 20,
    early_stopping_rounds = 10,
    watchlist = list(train = dtrain)
  )

  # Feature importance
  importance <- xgb.importance(
    feature_names = training_data$feature_names,
    model = xgb_model
  )

  cat("Top 10 Most Important Features:\n")
  print(importance[1:min(10, nrow(importance)), ])

  return(list(
    model = xgb_model,
    feature_names = training_data$feature_names,
    importance = importance,
    params = params
  ))
}

# Predict game outcome using XGBoost
predict_xgboost_game <- function(home_team, away_team, team_metrics, xgb_model_obj = NULL, injury_data = NULL) {

  # Load or use provided model
  if (is.null(xgb_model_obj)) {
    if (file.exists("models/xgboost_nfl_model.rds")) {
      cat("Loading pre-trained XGBoost model...\n")
      xgb_model_obj <- readRDS("models/xgboost_nfl_model.rds")
    } else {
      stop("No XGBoost model available. Please train model first.")
    }
  }

  # Get team metrics
  home_metrics <- team_metrics[team_metrics$team == home_team, ]
  away_metrics <- team_metrics[team_metrics$team == away_team, ]

  if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(
      error = "Team data not found",
      predicted_margin = 0,
      home_score = 21,
      away_score = 21,
      home_win_prob = 0.5,
      confidence = "Low",
      model_type = "XGBoost"
    ))
  }

  # Calculate differentials for all features
  feature_names <- xgb_model_obj$feature_names
  game_features <- numeric(length(feature_names))
  names(game_features) <- feature_names

  for (feature in feature_names) {
    if (feature %in% names(home_metrics) && feature %in% names(away_metrics)) {
      # For defensive metrics, reverse the differential (lower is better for defense)
      if (grepl("def_|defense", feature)) {
        game_features[feature] <- away_metrics[[feature]] - home_metrics[[feature]]
      } else {
        game_features[feature] <- home_metrics[[feature]] - away_metrics[[feature]]
      }
    } else {
      game_features[feature] <- 0  # Default value for missing features
    }
  }

  # Add home field advantage as a feature
  home_advantage <- 2.8
  if ("home_advantage" %in% feature_names) {
    game_features["home_advantage"] <- home_advantage
  }

  # Convert to matrix for prediction
  feature_matrix <- matrix(game_features, nrow = 1)
  colnames(feature_matrix) <- feature_names

  # Make prediction
  dtest <- xgb.DMatrix(data = feature_matrix)
  predicted_margin <- predict(xgb_model_obj$model, dtest)

  # Adjust for home field advantage if not already in features
  if (!"home_advantage" %in% feature_names) {
    predicted_margin <- predicted_margin + home_advantage
  }

  # Apply injury adjustments if available
  if (!is.null(injury_data)) {
    injury_adjustment <- calculate_injury_impact_xgb(home_team, away_team, injury_data)
    predicted_margin <- predicted_margin + injury_adjustment
  }

  # Calculate scores and probabilities
  league_avg_score <- 23.5
  home_score <- league_avg_score + (predicted_margin / 2)
  away_score <- league_avg_score - (predicted_margin / 2)

  # Calculate win probability using logistic function
  game_variance <- 14.0
  home_win_prob <- pnorm(predicted_margin / game_variance)

  # Ensure reasonable scores
  home_score <- max(7, min(45, home_score))
  away_score <- max(7, min(45, away_score))

  # Determine confidence level
  confidence <- ifelse(abs(predicted_margin) > 10, "High",
                      ifelse(abs(predicted_margin) > 5, "Medium", "Low"))

  return(list(
    predicted_margin = round(predicted_margin, 1),
    home_score = round(home_score, 0),
    away_score = round(away_score, 0),
    home_win_prob = round(home_win_prob, 3),
    away_win_prob = round(1 - home_win_prob, 3),
    predicted_winner = ifelse(predicted_margin > 0, home_team, away_team),
    confidence = confidence,
    model_type = "XGBoost",
    key_factors = list(
      top_feature_impacts = get_feature_impacts(game_features, xgb_model_obj$importance),
      prediction_uncertainty = abs(predicted_margin) / 14.0,
      model_confidence = confidence
    )
  ))
}

# Calculate feature impacts for interpretation
get_feature_impacts <- function(game_features, importance_df) {

  # Get top 5 most important features that have non-zero values
  top_features <- importance_df$Feature[1:min(5, nrow(importance_df))]

  impacts <- list()
  for (feature in top_features) {
    if (feature %in% names(game_features) && abs(game_features[feature]) > 0.001) {
      impacts[[feature]] <- round(game_features[feature], 3)
    }
  }

  return(impacts[1:min(3, length(impacts))])  # Return top 3 non-zero impacts
}

# Calculate injury impact for XGBoost model
calculate_injury_impact_xgb <- function(home_team, away_team, injury_data) {

  # Position impact weights for XGBoost model
  position_weights <- list(
    "QB" = 8.0,
    "RB" = 3.0,
    "WR" = 2.5,
    "TE" = 2.0,
    "OL" = 4.0,
    "DL" = 3.5,
    "LB" = 3.0,
    "CB" = 2.5,
    "S" = 2.0,
    "K" = 1.0,
    "P" = 0.5
  )

  home_injury_impact <- 0
  away_injury_impact <- 0

  # Calculate impact for each team
  for (team in c(home_team, away_team)) {
    team_injuries <- injury_data[injury_data$team == team, ]

    if (nrow(team_injuries) > 0) {
      team_impact <- 0

      for (i in 1:nrow(team_injuries)) {
        injury <- team_injuries[i, ]

        # Get position weight
        pos_weight <- ifelse(injury$position %in% names(position_weights),
                            position_weights[[injury$position]], 2.0)

        # Calculate impact based on participation probability and effectiveness
        participation_loss <- 1 - injury$participation_probability
        effectiveness_loss <- 1 - injury$effectiveness_when_playing

        # Total impact
        player_impact <- pos_weight * (participation_loss + effectiveness_loss * 0.5)
        team_impact <- team_impact + player_impact
      }

      if (team == home_team) {
        home_injury_impact <- team_impact
      } else {
        away_injury_impact <- team_impact
      }
    }
  }

  # Return net advantage (positive favors home team)
  return(away_injury_impact - home_injury_impact)
}

# Ensemble prediction combining Bayesian and XGBoost
predict_ensemble_game <- function(home_team, away_team, team_metrics,
                                 bayesian_weight = 0.6, xgb_weight = 0.4,
                                 injury_data = NULL) {

  cat(sprintf("Making ensemble prediction: %s vs %s\n", home_team, away_team))

  # Get Bayesian prediction (your existing model)
  if (exists("predict_2025_game")) {
    bayesian_pred <- predict_2025_game(home_team, away_team, team_metrics)
  } else {
    stop("Bayesian prediction function not found. Please load existing model.")
  }

  # Get XGBoost prediction
  xgb_pred <- predict_xgboost_game(home_team, away_team, team_metrics, injury_data = injury_data)

  if (!is.null(bayesian_pred$error) || !is.null(xgb_pred$error)) {
    return(list(error = "Prediction error in one or both models"))
  }

  # Weighted ensemble of predictions
  ensemble_margin <- bayesian_weight * bayesian_pred$predicted_margin +
                    xgb_weight * xgb_pred$predicted_margin

  ensemble_home_score <- bayesian_weight * bayesian_pred$home_score +
                        xgb_weight * xgb_pred$home_score

  ensemble_away_score <- bayesian_weight * bayesian_pred$away_score +
                        xgb_weight * xgb_pred$away_score

  ensemble_win_prob <- bayesian_weight * bayesian_pred$home_win_prob +
                      xgb_weight * xgb_pred$home_win_prob

  # Determine confidence based on agreement between models
  model_agreement <- abs(bayesian_pred$predicted_margin - xgb_pred$predicted_margin)
  ensemble_confidence <- ifelse(model_agreement < 3, "High",
                               ifelse(model_agreement < 7, "Medium", "Low"))

  return(list(
    predicted_margin = round(ensemble_margin, 1),
    home_score = round(ensemble_home_score, 0),
    away_score = round(ensemble_away_score, 0),
    home_win_prob = round(ensemble_win_prob, 3),
    away_win_prob = round(1 - ensemble_win_prob, 3),
    predicted_winner = ifelse(ensemble_margin > 0, home_team, away_team),
    confidence = ensemble_confidence,
    model_type = "Ensemble (Bayesian + XGBoost)",
    model_details = list(
      bayesian_margin = bayesian_pred$predicted_margin,
      xgb_margin = xgb_pred$predicted_margin,
      model_agreement = round(model_agreement, 1),
      bayesian_weight = bayesian_weight,
      xgb_weight = xgb_weight
    ),
    key_factors = list(
      bayesian_factors = bayesian_pred$key_factors,
      xgb_factors = xgb_pred$key_factors,
      ensemble_strength = ifelse(model_agreement < 3, "Strong Agreement", "Mixed Signals")
    )
  ))
}

# Create directories for model storage
create_model_directories <- function() {
  if (!dir.exists("models")) {
    dir.create("models", recursive = TRUE)
    cat("Created models directory\n")
  }
}

# Save trained XGBoost model
save_xgboost_model <- function(xgb_model_obj, filename = "models/xgboost_nfl_model.rds") {
  create_model_directories()
  saveRDS(xgb_model_obj, filename)
  cat(sprintf("XGBoost model saved to %s\n", filename))
}

# Load trained XGBoost model
load_xgboost_model <- function(filename = "models/xgboost_nfl_model.rds") {
  if (file.exists(filename)) {
    cat(sprintf("Loading XGBoost model from %s\n", filename))
    return(readRDS(filename))
  } else {
    stop(sprintf("Model file not found: %s", filename))
  }
}

cat("🤖 Gradient Boosting NFL Predictor Loaded!\n")
cat("📊 Functions available:\n")
cat("  - train_xgboost_model(): Train new XGBoost model\n")
cat("  - predict_xgboost_game(): XGBoost-only predictions\n")
cat("  - predict_ensemble_game(): Combined Bayesian + XGBoost\n")
cat("  - save_xgboost_model(): Save trained model\n")
cat("  - load_xgboost_model(): Load saved model\n")
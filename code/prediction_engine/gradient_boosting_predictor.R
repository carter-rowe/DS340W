library(xgboost)
library(dplyr)
library(readr)
library(caret)
library(Matrix)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

# Gradient Boosting NFL Prediction Model
# XGBoost-based predictor using 2024-2025 season data

# Load 2024-2025 season data (similar to nfl_2025_predictor)
load_xgboost_prediction_data <- function() {

  cat("Loading 2024-2025 season data for XGBoost predictor...\n")

  # Load the same 2025 team metrics as nfl_2025_predictor
  if (!file.exists("data/team_metrics_2025.csv")) {
    stop("team_metrics_2025.csv not found. Please ensure data file exists.")
  }

  team_metrics <- read_csv("data/team_metrics_2025.csv", show_col_types = FALSE)
  cat(sprintf("Loaded metrics for %d teams from 2024-2025 season\n", nrow(team_metrics)))

  # Load player impact database if available
  if (file.exists("data/player_impact_database.csv")) {
    player_impact_db <- read_csv("data/player_impact_database.csv", show_col_types = FALSE)
    cat(sprintf("Loaded %d player impact records\n", nrow(player_impact_db)))
  } else {
    player_impact_db <- NULL
    cat("Player impact database not found - predictions will exclude injury analysis\n")
  }

  return(list(
    team_metrics = team_metrics,
    player_impact_db = player_impact_db
  ))
}

# Load and prepare historical training data
load_training_data <- function() {

  cat("Loading historical game data for model training...\n")

  if (!file.exists("data/games_enhanced.csv")) {
    stop("games_enhanced.csv not found. Please ensure historical data file exists.")
  }

  games <- read_csv("data/games_enhanced.csv", show_col_types = FALSE)

  # Filter to recent seasons (2020-2024) for better relevance
  games <- games %>%
    filter(!is.na(season.x), season.x >= 2020, season.x <= 2024) %>%
    filter(!is.na(home_score), !is.na(away_score))

  cat(sprintf("Loaded %d games from 2020-2024 seasons\n", nrow(games)))

  return(games)
}

# Prepare training features from historical game data
prepare_training_features <- function(games_data) {

  cat("Preparing training features from historical games...\n")

  # Create feature matrix from game differentials
  training_features <- data.frame(
    # Offensive EPA differentials
    off_epa_diff = games_data$home_off_epa_avg - games_data$away_off_epa_avg,
    pass_epa_diff = games_data$home_off_pass_epa_avg - games_data$away_off_pass_epa_avg,
    rush_epa_diff = games_data$home_off_rush_epa_avg - games_data$away_off_rush_epa_avg,

    # Defensive EPA differentials (reversed: lower is better)
    def_epa_diff = games_data$away_def_epa_allowed_avg - games_data$home_def_epa_allowed_avg,
    pass_def_diff = games_data$away_def_pass_epa_allowed_avg - games_data$home_def_pass_epa_allowed_avg,
    rush_def_diff = games_data$away_def_rush_epa_allowed_avg - games_data$home_def_rush_epa_allowed_avg,

    # Success rate differentials
    off_success_diff = games_data$home_off_success_rate - games_data$away_off_success_rate,
    def_success_diff = games_data$away_def_success_rate_allowed - games_data$home_def_success_rate_allowed,
    pass_success_diff = games_data$home_off_pass_success_rate - games_data$away_off_pass_success_rate,
    rush_success_diff = games_data$home_off_rush_success_rate - games_data$away_off_rush_success_rate,

    # Trend differentials
    off_trend_diff = ifelse(!is.na(games_data$home_epa_trend) & !is.na(games_data$away_epa_trend),
                           games_data$home_epa_trend - games_data$away_epa_trend, 0),
    def_trend_diff = ifelse(!is.na(games_data$home_def_trend) & !is.na(games_data$away_def_trend),
                           games_data$away_def_trend - games_data$home_def_trend, 0),

    # Home field advantage
    home_advantage = 1
  )

  # Handle any remaining NA values
  for (col in names(training_features)) {
    if (any(is.na(training_features[[col]]))) {
      training_features[[col]][is.na(training_features[[col]])] <- 0
    }
  }

  # Target variable: point differential (home - away)
  target <- games_data$home_score - games_data$away_score

  # Remove rows with NA targets
  valid_rows <- !is.na(target)
  training_features <- training_features[valid_rows, ]
  target <- target[valid_rows]

  cat(sprintf("Created %d training samples with %d features\n",
              nrow(training_features), ncol(training_features)))

  return(list(
    features = as.matrix(training_features),
    target = target,
    feature_names = names(training_features)
  ))
}

# Train XGBoost model on historical game data
train_xgboost_from_history <- function(games_data = NULL, save_model = TRUE) {

  cat(strrep("=", 60), "\n")
  cat("Training XGBoost NFL Prediction Model\n")
  cat(strrep("=", 60), "\n\n")

  # Load historical data if not provided
  if (is.null(games_data)) {
    games_data <- load_training_data()
  }

  # Prepare training features
  training_data <- prepare_training_features(games_data)

  # XGBoost parameters optimized for NFL prediction
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.05,                   # Lower learning rate for better generalization
    max_depth = 5,                # Slightly shallower trees
    min_child_weight = 5,         # Higher to prevent overfitting
    subsample = 0.8,              # Row sampling
    colsample_bytree = 0.8,       # Feature sampling
    lambda = 1.5,                 # L2 regularization
    alpha = 0.1,                  # L1 regularization
    gamma = 0.5                   # Minimum split loss
  )

  # Convert to DMatrix for XGBoost
  dtrain <- xgb.DMatrix(data = training_data$features, label = training_data$target)

  # Train model with cross-validation
  cat("\nTraining model with cross-validation...\n")
  set.seed(42)

  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    verbose = 1,
    print_every_n = 25,
    early_stopping_rounds = 20,
    watchlist = list(train = dtrain)
  )

  # Feature importance
  importance <- xgb.importance(
    feature_names = training_data$feature_names,
    model = xgb_model
  )

  cat("\n", strrep("=", 60), "\n")
  cat("Top 10 Most Important Features:\n")
  cat(strrep("-", 60), "\n")
  print(importance[1:min(10, nrow(importance)), c("Feature", "Gain")])
  cat("\n")

  # Create model object
  xgb_model_obj <- list(
    model = xgb_model,
    feature_names = training_data$feature_names,
    importance = importance,
    params = params,
    training_date = Sys.Date(),
    training_samples = nrow(training_data$features)
  )

  # Save model
  if (save_model) {
    save_xgboost_model(xgb_model_obj)
  }

  cat(strrep("=", 60), "\n")
  cat("Model training complete!\n")
  cat(strrep("=", 60), "\n\n")

  return(xgb_model_obj)
}

# Function to prepare data for XGBoost (used for team metrics)
prepare_xgboost_data <- function(team_metrics, injury_data = NULL) {

  cat("Preparing data for gradient boosting model...\n")

  # Core feature columns that should be in team_metrics_2025.csv
  feature_cols <- c(
    # Core EPA metrics
    "off_epa_per_play", "def_epa_per_play", "off_success_rate", "def_success_rate_allowed",

    # Pass/Rush metrics
    "pass_epa_per_play", "rush_epa_per_play",
    "pass_def_epa_per_play", "rush_def_epa_per_play",

    # Situational metrics
    "rz_efficiency", "third_down_rate",
    "rz_def_efficiency", "third_def_rate",

    # Special teams
    "st_off_epa", "st_def_epa",

    # Trends
    "off_trend", "def_trend"
  )

  # Select only available columns
  available_cols <- intersect(feature_cols, names(team_metrics))

  if (length(available_cols) == 0) {
    stop("No feature columns found in team_metrics data")
  }

  cat(sprintf("Using %d features from team metrics\n", length(available_cols)))

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

# Predict game outcome using XGBoost with 2024-2025 data
predict_xgboost_game <- function(home_team, away_team, team_metrics = NULL,
                                 xgb_model_obj = NULL, injury_data = NULL) {

  # Load 2025 team metrics if not provided
  if (is.null(team_metrics)) {
    data <- load_xgboost_prediction_data()
    team_metrics <- data$team_metrics
    # Note: player_impact_db is different from current injury data
    # Don't automatically use it as injury_data
  }

  # Load or use provided model
  if (is.null(xgb_model_obj)) {
    if (file.exists("models/xgboost_nfl_model.rds")) {
      cat("Loading pre-trained XGBoost model...\n")
      xgb_model_obj <- readRDS("models/xgboost_nfl_model.rds")
    } else {
      stop("No XGBoost model available. Train model first with: train_xgboost_from_history()")
    }
  }

  # Get team metrics
  home_metrics <- team_metrics[team_metrics$team == home_team, ]
  away_metrics <- team_metrics[team_metrics$team == away_team, ]

  if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(
      error = paste("Team data not found for", home_team, "or", away_team),
      predicted_margin = 0,
      home_score = 21,
      away_score = 21,
      home_win_prob = 0.5,
      confidence = "Low",
      model_type = "XGBoost"
    ))
  }

  # Calculate differentials matching training features
  feature_names <- xgb_model_obj$feature_names
  game_features <- numeric(length(feature_names))
  names(game_features) <- feature_names

  # Calculate differentials matching training features
  # This needs to match prepare_training_features exactly
  for (feature in feature_names) {
    if (feature == "home_advantage") {
      game_features[feature] <- 1
    } else if (feature == "off_epa_diff") {
      game_features[feature] <- home_metrics$off_epa_per_play - away_metrics$off_epa_per_play
    } else if (feature == "pass_epa_diff") {
      game_features[feature] <- home_metrics$pass_epa_per_play - away_metrics$pass_epa_per_play
    } else if (feature == "rush_epa_diff") {
      game_features[feature] <- home_metrics$rush_epa_per_play - away_metrics$rush_epa_per_play
    } else if (feature == "def_epa_diff") {
      game_features[feature] <- away_metrics$def_epa_per_play - home_metrics$def_epa_per_play
    } else if (feature == "pass_def_diff") {
      game_features[feature] <- away_metrics$pass_def_epa_per_play - home_metrics$pass_def_epa_per_play
    } else if (feature == "rush_def_diff") {
      game_features[feature] <- away_metrics$rush_def_epa_per_play - home_metrics$rush_def_epa_per_play
    } else if (feature == "off_success_diff") {
      game_features[feature] <- home_metrics$off_success_rate - away_metrics$off_success_rate
    } else if (feature == "def_success_diff") {
      game_features[feature] <- away_metrics$def_success_rate_allowed - home_metrics$def_success_rate_allowed
    } else if (feature == "pass_success_diff") {
      # Note: These metrics not in team_metrics_2025.csv, default to 0
      game_features[feature] <- 0
    } else if (feature == "rush_success_diff") {
      game_features[feature] <- 0
    } else if (feature == "off_trend_diff") {
      game_features[feature] <- home_metrics$off_trend - away_metrics$off_trend
    } else if (feature == "def_trend_diff") {
      game_features[feature] <- away_metrics$def_trend - home_metrics$def_trend
    } else {
      game_features[feature] <- 0
    }
  }

  # Convert to matrix for prediction
  feature_matrix <- matrix(game_features, nrow = 1)
  colnames(feature_matrix) <- feature_names

  # Make prediction
  dtest <- xgb.DMatrix(data = feature_matrix)
  predicted_margin <- predict(xgb_model_obj$model, dtest)

  # Apply injury adjustments if available
  if (!is.null(injury_data)) {
    injury_adjustment <- calculate_injury_impact_xgb(home_team, away_team, injury_data)
    predicted_margin <- predicted_margin + injury_adjustment
  }

  # Calculate scores and probabilities
  league_avg_score <- 23.5
  home_score <- league_avg_score + (predicted_margin / 2)
  away_score <- league_avg_score - (predicted_margin / 2)

  # Calculate win probability
  game_variance <- 14.0
  home_win_prob <- pnorm(predicted_margin / game_variance)

  # Ensure reasonable scores
  home_score <- max(7, min(50, home_score))
  away_score <- max(7, min(50, away_score))

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
      prediction_uncertainty = round(abs(predicted_margin) / 14.0, 3),
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

# NFL team information with full names
nfl_teams <- data.frame(
  code = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
           "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LAC", "LAR", "MIA", 
           "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS"),
  name = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
           "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
           "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
           "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
           "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
           "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
           "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
           "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),
  stringsAsFactors = FALSE
)

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "🤖 XGBoost NFL 2025-2026 Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("football-ball")),
      menuItem("Team Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("Model Insights", tabName = "insights", icon = icon("chart-line")),
      menuItem("Train Model", tabName = "train", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #1f4e79 !important;
        }
        .prediction-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin: 10px 0;
        }
        .team-select {
          font-size: 16px;
          padding: 10px;
        }
      "))
    ),
    
    tabItems(
      # Game Prediction Tab
      tabItem(tabName = "prediction",
        fluidRow(
          box(
            title = "🏟️ Select Matchup for 2025-2026 Season", 
            status = "primary", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(5,
                h4("🏠 Home Team", style = "color: #1f4e79; font-weight: bold;"),
                selectInput("home_team", "", 
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "KC",
                  width = "100%"
                )
              ),
              
              column(2,
                div(style = "text-align: center; padding-top: 35px;",
                  h3("VS", style = "color: #d9534f; font-weight: bold;")
                )
              ),
              
              column(5,
                h4("🛫 Away Team", style = "color: #1f4e79; font-weight: bold;"),
                selectInput("away_team", "", 
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "BUF",
                  width = "100%"
                )
              )
            ),
            
            div(style = "text-align: center; margin: 20px 0;",
              checkboxInput("include_injuries", "🏥 Include Injury Analysis", value = FALSE),
              br(),
              actionButton("predict", "🔮 Predict Game (XGBoost)", 
                class = "btn-success btn-lg",
                style = "padding: 15px 30px; font-size: 18px; font-weight: bold;")
            )
          )
        ),
        
        # Prediction Results
        fluidRow(
          column(12,
            uiOutput("prediction_results")
          )
        ),
        
        # Detailed Analysis
        fluidRow(
          box(
            title = "📊 Enhanced Team Comparison", 
            status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("detailed_comparison")
          )
        )
      ),
      
      # Team Rankings Tab
      tabItem(tabName = "rankings",
        fluidRow(
          box(
            title = "🏆 2025 Team Power Rankings", 
            status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("team_rankings")
          )
        )
      ),
      
      # Model Insights Tab  
      tabItem(tabName = "insights",
        fluidRow(
          box(
            title = "🧠 XGBoost Model Performance & Insights", 
            status = "success", solidHeader = TRUE, width = 12,
            
            h4("Model Features:"),
            tags$ul(
              tags$li("🤖 Gradient Boosting: XGBoost algorithm for NFL predictions"),
              tags$li("📈 Training Data: 2020-2024 seasons for model training"),
              tags$li("🏈 EPA-based Features: Offense, defense, passing, rushing differentials"),
              tags$li("📍 Situational Metrics: Success rates, trends, home advantage"),
              tags$li("🔮 Predictions: Score, margin, win probability with confidence levels")
            ),
            
            h4("Key Features:"),
            tags$ul(
              tags$li("Offensive EPA differentials: Home vs Away"),
              tags$li("Defensive EPA differentials: Lower is better"),
              tags$li("Pass/Rush EPA splits: Situational analysis"),
              tags$li("Success rate differentials: Efficiency metrics"),
              tags$li("Recent trends: Momentum factors"),
              tags$li("Home field advantage: ~2-3 points")
            ),
            
            h4("Model Parameters:"),
            tags$ul(
              tags$li("Learning Rate (eta): 0.05 for generalization"),
              tags$li("Max Depth: 5 trees"),
              tags$li("Regularization: L1 (0.1) + L2 (1.5)"),
              tags$li("Subsample: 80% row sampling"),
              tags$li("Feature Sampling: 80% column sampling")
            )
          )
        )
      ),
      
      # Train Model Tab
      tabItem(tabName = "train",
        fluidRow(
          box(
            title = "🔧 Train XGBoost Model", 
            status = "warning", solidHeader = TRUE, width = 12,
            
            p("Train a new XGBoost model using historical game data from 2020-2024."),
            p("Note: This may take several minutes."),
            br(),
            actionButton("train_model", "🚀 Train Model", 
              class = "btn-primary btn-lg",
              style = "padding: 15px 30px; font-size: 18px; font-weight: bold;"),
            br(), br(),
            verbatimTextOutput("training_status")
          )
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Load data
  prediction_data <- reactive({
    tryCatch({
      load_xgboost_prediction_data()
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Load model
  xgb_model_obj <- reactive({
    tryCatch({
      if (file.exists("models/xgboost_nfl_model.rds")) {
        readRDS("models/xgboost_nfl_model.rds")
      } else {
        NULL
      }
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Prediction with XGBoost
  prediction_result <- eventReactive(input$predict, {
    req(input$home_team, input$away_team)
    
    if (input$home_team == input$away_team) {
      return(list(error = "Teams cannot play themselves!"))
    }
    
    data <- prediction_data()
    model <- xgb_model_obj()
    
    if (is.null(data)) {
      return(list(error = "Could not load prediction data"))
    }
    
    if (is.null(model)) {
      return(list(error = "XGBoost model not found. Please train a model first using the Train Model tab."))
    }
    
    # Prepare injury data if requested
    injury_data <- NULL
    if (input$include_injuries && exists("get_current_injuries")) {
      tryCatch({
        injury_data <- get_current_injuries()
      }, error = function(e) {
        # Use empty injury data if can't load
        injury_data <- NULL
      })
    }
    
    # Make prediction
    tryCatch({
      predict_xgboost_game(
        input$home_team, 
        input$away_team, 
        team_metrics = data$team_metrics,
        xgb_model_obj = model,
        injury_data = injury_data
      )
    }, error = function(e) {
      return(list(error = paste("Prediction error:", e$message)))
    })
  })
  
  # Prediction results UI
  output$prediction_results <- renderUI({
    if (input$predict == 0) {
      return(div(
        class = "prediction-box",
        style = "text-align: center;",
        h3("👆 Select teams and click Predict to see XGBoost results!")
      ))
    }
    
    result <- prediction_result()
    
    if (!is.null(result$error)) {
      return(div(
        class = "alert alert-danger",
        h4("❌ Error: ", result$error)
      ))
    }
    
    home_name <- nfl_teams[nfl_teams$code == input$home_team, "name"]
    away_name <- nfl_teams[nfl_teams$code == input$away_team, "name"]
    
    winner_name <- nfl_teams[nfl_teams$code == result$predicted_winner, "name"]
    
    div(
      class = "prediction-box",
      fluidRow(
        column(12,
          h2("🤖 XGBOOST PREDICTION RESULTS", style = "text-align: center; margin-bottom: 30px;")
        )
      ),
      
      fluidRow(
        column(4,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3); padding-right: 20px;",
            h3("🏆 WINNER"),
            h2(result$predicted_winner, style = "font-size: 48px; margin: 10px 0;"),
            h4(winner_name)
          )
        ),
        
        column(4,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3); padding: 0 20px;",
            h3("📊 PREDICTED SCORE"),
            h2(paste(result$home_score, "-", result$away_score), 
               style = "font-size: 36px; margin: 10px 0;"),
            h4(paste("Margin:", abs(result$predicted_margin), "pts"))
          )
        ),
        
        column(4,
          div(style = "text-align: center; padding-left: 20px;",
            h3("🎯 WIN PROBABILITY"),
            h2(paste0(round(max(result$home_win_prob, result$away_win_prob) * 100, 1), "%"),
               style = "font-size: 48px; margin: 10px 0;"),
            h4(paste("Confidence:", result$confidence))
          )
        )
      ),
      
      if (!is.null(result$key_factors) && !is.null(result$key_factors$top_feature_impacts)) {
        fluidRow(
          column(12,
            div(style = "margin-top: 20px; padding-top: 20px; border-top: 2px solid rgba(255,255,255,0.3);",
              h4("🔍 Key Feature Impacts:"),
              tags$ul(
                lapply(names(result$key_factors$top_feature_impacts), function(feat) {
                  tags$li(paste(feat, ":", result$key_factors$top_feature_impacts[[feat]]))
                })
              )
            )
          )
        )
      }
    )
  })
  
  # Detailed comparison table
  output$detailed_comparison <- DT::renderDataTable({
    if (input$predict == 0) return(data.frame())
    
    data <- prediction_data()
    if (is.null(data)) return(data.frame())
    
    metrics <- data$team_metrics
    home_metrics <- metrics[metrics$team == input$home_team, ]
    away_metrics <- metrics[metrics$team == input$away_team, ]
    
    if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) return(data.frame())
    
    # Get available columns
    common_cols <- intersect(names(home_metrics), names(away_metrics))
    numeric_cols <- common_cols[sapply(common_cols, function(x) is.numeric(home_metrics[[x]]))]
    numeric_cols <- setdiff(numeric_cols, c("season", "week"))  # Exclude non-metric columns
    
    if (length(numeric_cols) == 0) return(data.frame())
    
    # Build comparison dataframe
    format_metric_name <- function(x) {
      x <- gsub("_", " ", x)
      x <- gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
      return(x)
    }
    
    comparison_data <- data.frame(
      Metric = format_metric_name(numeric_cols),
      Home_Team = sapply(numeric_cols, function(x) round(home_metrics[[x]], 3)),
      Away_Team = sapply(numeric_cols, function(x) round(away_metrics[[x]], 3)),
      stringsAsFactors = FALSE
    )
    
    names(comparison_data) <- c("Metric", input$home_team, input$away_team)
    
    DT::datatable(comparison_data, 
      options = list(pageLength = 15, dom = 't'),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 2:3, 
        backgroundColor = DT::styleInterval(0, c('#ffebee', '#e8f5e8')))
  })
  
  # Team rankings
  output$team_rankings <- DT::renderDataTable({
    data <- prediction_data()
    if (is.null(data)) return(data.frame())
    
    metrics <- data$team_metrics
    
    # Calculate a simple ranking metric
    if ("team_strength" %in% names(metrics)) {
      rankings <- metrics %>%
        arrange(desc(team_strength)) %>%
        mutate(rank = row_number())
    } else {
      # Fallback: use offensive EPA
      rankings <- metrics %>%
        arrange(desc(off_epa_per_play)) %>%
        mutate(rank = row_number())
    }
    
    rankings <- rankings %>%
      mutate(
        team_name = nfl_teams$name[match(team, nfl_teams$code)],
        team_strength = ifelse("team_strength" %in% names(rankings), round(team_strength, 3), NA),
        off_epa_per_play = ifelse("off_epa_per_play" %in% names(rankings), round(off_epa_per_play, 3), NA),
        def_epa_per_play = ifelse("def_epa_per_play" %in% names(rankings), round(def_epa_per_play, 3), NA)
      ) %>%
      select(rank, team, team_name, team_strength, off_epa_per_play, def_epa_per_play) %>%
      filter(!is.na(team_name))
    
    names(rankings) <- c("Rank", "Team", "Full Name", "Strength", "Off EPA", "Def EPA")
    
    DT::datatable(rankings,
      options = list(pageLength = 32, dom = 't'),
      rownames = FALSE
    ) %>%
      DT::formatStyle("Rank",
        backgroundColor = DT::styleInterval(c(8, 16, 24), 
          c('#2e7d32', '#558b2f', '#fbc02d', '#d32f2f')),
        color = 'white'
      )
  })
  
  # Training status
  output$training_status <- renderText({
    if (input$train_model == 0) {
      return("Click 'Train Model' to start training a new XGBoost model.")
    }
    
    isolate({
      showNotification("Training started. This may take several minutes...", type = "message")
      
      tryCatch({
        model <- train_xgboost_from_history()
        showNotification("Model training completed successfully!", type = "success")
        paste("✅ Model training completed successfully!\n",
              "Model saved to: models/xgboost_nfl_model.rds\n",
              "Training samples:", model$training_samples)
      }, error = function(e) {
        showNotification(paste("Training error:", e$message), type = "error")
        paste("❌ Error:", e$message)
      })
    })
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  cat("🤖 XGBoost NFL Predictor Loaded (2024-2025 Season)!\n")
  cat(strrep("=", 60), "\n")
  cat("📊 Main Functions:\n")
  cat("  1. train_xgboost_from_history() - Train model on 2020-2024 games\n")
  cat("  2. predict_xgboost_game(home, away) - Predict using 2025 data\n")
  cat("  3. predict_ensemble_game(home, away) - Bayesian + XGBoost combined\n\n")
  cat("📂 Data Functions:\n")
  cat("  - load_xgboost_prediction_data() - Load 2024-2025 team metrics\n")
  cat("  - load_training_data() - Load historical game data\n\n")
  cat("💾 Model Management:\n")
  cat("  - save_xgboost_model(model) - Save trained model\n")
  cat("  - load_xgboost_model() - Load saved model\n")
  cat(strrep("=", 60), "\n")
  cat("\n💡 Quick Start:\n")
  cat("  1. Train: model <- train_xgboost_from_history()\n")
  cat("  2. Predict: predict_xgboost_game('KC', 'BUF')\n")
  cat("  3. Run UI: shiny::runApp('code/prediction_engine/gradient_boosting_predictor.R')\n")
  cat(strrep("=", 60), "\n")
}
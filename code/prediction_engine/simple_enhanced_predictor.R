library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)

# Suppress warnings
options(warn = -1)

# Load original prediction function as fallback
tryCatch({
  source("code/prediction_engine/nfl_2025_predictor.R")
}, error = function(e) {
  cat("Warning: Could not load original predictor\n")
})

# Simple enhanced prediction function that always works
predict_simple_enhanced_game <- function(home_team, away_team, team_metrics) {

  # Get team metrics
  home_metrics <- team_metrics[team_metrics$team == home_team, ]
  away_metrics <- team_metrics[team_metrics$team == away_team, ]

  if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(
      error = paste("Team data not found for", home_team, "or", away_team),
      predicted_margin = 0,
      home_score = 21,
      away_score = 21,
      home_win_prob = 0.5
    ))
  }

  # Enhanced model coefficients
  home_advantage <- 2.8
  epa_off_coef <- 35.0
  epa_def_coef <- 32.0
  success_coef <- 15.0
  pass_coef <- 18.0
  rush_coef <- 12.0
  st_coef <- 8.0
  trend_coef <- 10.0

  # Handle missing enhanced columns gracefully
  get_metric <- function(df, col, default = 0) {
    if (col %in% names(df) && !is.na(df[[col]])) {
      return(df[[col]])
    } else {
      return(default)
    }
  }

  # Calculate differentials safely
  epa_off_diff <- get_metric(home_metrics, "off_epa_per_play") - get_metric(away_metrics, "off_epa_per_play")
  epa_def_diff <- get_metric(away_metrics, "def_epa_per_play") - get_metric(home_metrics, "def_epa_per_play")

  success_off_diff <- get_metric(home_metrics, "off_success_rate", 0.45) - get_metric(away_metrics, "off_success_rate", 0.45)
  success_def_diff <- get_metric(away_metrics, "def_success_rate_allowed", 0.55) - get_metric(home_metrics, "def_success_rate_allowed", 0.55)

  pass_diff <- get_metric(home_metrics, "pass_epa_per_play") - get_metric(away_metrics, "pass_epa_per_play")
  rush_diff <- get_metric(home_metrics, "rush_epa_per_play") - get_metric(away_metrics, "rush_epa_per_play")

  pass_def_diff <- get_metric(away_metrics, "pass_def_epa_per_play") - get_metric(home_metrics, "pass_def_epa_per_play")
  rush_def_diff <- get_metric(away_metrics, "rush_def_epa_per_play") - get_metric(home_metrics, "rush_def_epa_per_play")

  st_diff <- get_metric(home_metrics, "st_off_epa") - get_metric(away_metrics, "st_off_epa")

  trend_off_diff <- get_metric(home_metrics, "off_trend") - get_metric(away_metrics, "off_trend")
  trend_def_diff <- get_metric(away_metrics, "def_trend") - get_metric(home_metrics, "def_trend")

  # Enhanced metrics (if available)
  cpoe_diff <- get_metric(home_metrics, "cpoe") - get_metric(away_metrics, "cpoe")
  late_game_diff <- get_metric(home_metrics, "late_game_epa") - get_metric(away_metrics, "late_game_epa")

  # Calculate predicted margin
  predicted_margin <- home_advantage +
                     epa_off_coef * epa_off_diff +
                     epa_def_coef * epa_def_diff +
                     success_coef * success_off_diff +
                     success_coef * success_def_diff +
                     pass_coef * pass_diff +
                     rush_coef * rush_diff +
                     pass_coef * pass_def_diff +
                     rush_coef * rush_def_diff +
                     st_coef * st_diff +
                     trend_coef * trend_off_diff +
                     trend_coef * trend_def_diff +
                     20.0 * cpoe_diff +  # CPOE bonus if available
                     8.0 * late_game_diff  # Clutch factor if available

  # Calculate scores and probabilities
  league_avg_score <- 23.5
  home_score <- league_avg_score + (predicted_margin / 2)
  away_score <- league_avg_score - (predicted_margin / 2)

  # Calculate win probability
  game_variance <- 14.0
  home_win_prob <- pnorm(predicted_margin / game_variance)

  # Ensure reasonable scores
  home_score <- max(7, min(45, home_score))
  away_score <- max(7, min(45, away_score))

  return(list(
    predicted_margin = round(predicted_margin, 1),
    home_score = round(home_score, 0),
    away_score = round(away_score, 0),
    home_win_prob = round(home_win_prob, 3),
    away_win_prob = round(1 - home_win_prob, 3),
    predicted_winner = ifelse(predicted_margin > 0, home_team, away_team),
    confidence = ifelse(abs(predicted_margin) > 10, "High",
                       ifelse(abs(predicted_margin) > 5, "Medium", "Low")),
    model_type = "Enhanced Bayesian (Safe Mode)",
    key_factors = list(
      "epa_advantage" = round(epa_off_diff, 3),
      "def_advantage" = round(epa_def_diff, 3),
      "cpoe_advantage" = round(cpoe_diff, 3),
      "trend_advantage" = round(trend_off_diff, 3),
      "clutch_advantage" = round(late_game_diff, 3)
    )
  ))
}

# Load prediction data safely
load_safe_prediction_data <- function() {

  cat("Loading prediction data safely...\n")

  # Try to load enhanced metrics first
  if (file.exists("data/team_metrics_2025_enhanced.csv")) {
    cat("Found enhanced metrics\n")
    team_metrics <- read_csv("data/team_metrics_2025_enhanced.csv", show_col_types = FALSE)
  } else if (file.exists("data/team_metrics_2025.csv")) {
    cat("Found standard metrics\n")
    team_metrics <- read_csv("data/team_metrics_2025.csv", show_col_types = FALSE)
  } else {
    cat("No metrics found - creating basic dataset\n")
    # Create minimal dataset for testing
    team_metrics <- data.frame(
      team = c("KC", "BUF", "SF", "PHI", "BAL", "CIN", "DAL", "GB", "LAR", "SEA"),
      off_epa_per_play = c(0.15, 0.12, 0.10, 0.08, 0.14, 0.09, 0.06, 0.11, 0.07, 0.05),
      def_epa_per_play = c(-0.05, -0.08, -0.10, -0.06, -0.04, -0.02, 0.02, -0.07, 0.01, 0.03),
      off_success_rate = c(0.48, 0.46, 0.45, 0.44, 0.47, 0.43, 0.42, 0.45, 0.41, 0.40),
      def_success_rate_allowed = c(0.42, 0.44, 0.41, 0.45, 0.43, 0.47, 0.49, 0.44, 0.48, 0.50),
      pass_epa_per_play = c(0.20, 0.18, 0.15, 0.12, 0.19, 0.14, 0.08, 0.16, 0.10, 0.06),
      rush_epa_per_play = c(0.08, 0.06, 0.04, 0.02, 0.09, 0.03, 0.01, 0.05, 0.02, 0.00),
      pass_def_epa_per_play = c(-0.08, -0.12, -0.15, -0.09, -0.06, -0.03, 0.05, -0.10, 0.02, 0.06),
      rush_def_epa_per_play = c(-0.02, -0.04, -0.05, -0.03, -0.01, 0.01, 0.03, -0.04, 0.02, 0.04),
      st_off_epa = c(0.02, 0.01, 0.03, 0.00, 0.01, -0.01, -0.02, 0.01, -0.01, -0.02),
      team_strength = c(1.2, 0.8, 0.9, 0.6, 1.0, 0.4, 0.2, 0.7, 0.3, 0.1),
      off_trend = c(0.05, 0.03, 0.02, 0.01, 0.04, 0.00, -0.01, 0.02, -0.01, -0.02),
      def_trend = c(-0.02, -0.04, -0.05, -0.01, -0.03, 0.01, 0.03, -0.03, 0.02, 0.04),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    team_metrics = team_metrics,
    current_injuries = data.frame(),  # Empty for now
    player_impact_db = data.frame()   # Empty for now
  ))
}

# NFL team information
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

# Simple Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "🏈 NFL Enhanced Predictor (Safe Mode)"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("football-ball"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "prediction",
        fluidRow(
          box(
            title = "🏈 Enhanced NFL Game Prediction",
            status = "primary", solidHeader = TRUE, width = 12,

            fluidRow(
              column(5,
                h4("🏠 Home Team"),
                selectInput("home_team", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "KC"
                )
              ),

              column(2,
                div(style = "text-align: center; padding-top: 35px;",
                  h3("VS")
                )
              ),

              column(5,
                h4("🛫 Away Team"),
                selectInput("away_team", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "BUF"
                )
              )
            ),

            div(style = "text-align: center; margin: 20px 0;",
              actionButton("predict", "🔮 Predict Game",
                class = "btn-success btn-lg")
            )
          )
        ),

        fluidRow(
          column(12,
            uiOutput("prediction_results")
          )
        )
      )
    )
  )
)

# Simple Shiny Server
server <- function(input, output, session) {

  # Load data
  prediction_data <- reactive({
    load_safe_prediction_data()
  })

  # Make prediction
  prediction_result <- eventReactive(input$predict, {
    req(input$home_team, input$away_team)

    if (input$home_team == input$away_team) {
      return(list(error = "Teams cannot play themselves!"))
    }

    data <- prediction_data()

    tryCatch({
      predict_simple_enhanced_game(input$home_team, input$away_team, data$team_metrics)
    }, error = function(e) {
      return(list(error = paste("Prediction error:", e$message)))
    })
  })

  # Display results
  output$prediction_results <- renderUI({
    if (input$predict == 0) {
      return(div(
        style = "text-align: center; padding: 20px; background: #f8f9fa; border-radius: 10px;",
        h3("👆 Select teams and click Predict!")
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
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px;",

      fluidRow(
        column(12,
          h2("🏈 PREDICTION RESULTS", style = "text-align: center; margin-bottom: 30px;")
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
            h3("📊 SCORE"),
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

      if (!is.null(result$model_type)) {
        div(style = "text-align: center; margin-top: 20px; padding-top: 20px; border-top: 1px solid rgba(255,255,255,0.3);",
          h5(paste("Model:", result$model_type))
        )
      }
    )
  })
}

# Run the app
cat("🏈 Simple Enhanced NFL Predictor Ready!\n")
cat("🔧 Safe mode - handles missing data gracefully\n")

if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  list(ui = ui, server = server)
}
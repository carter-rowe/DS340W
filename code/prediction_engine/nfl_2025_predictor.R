library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(rstan)
library(dplyr)
library(readr)
library(ggplot2)

# Load injury system components
source("code/injury_system/injury_integration.R")

# Load team data and fitted model with injury system
load_prediction_data <- function() {
  
  # Check if we have the 2025 team metrics
  if (!file.exists("data/team_metrics_2025.csv")) {
    cat("Creating 2025 team metrics...\n")
    source("code/data_enhancement/current_season_pipeline.R")
    team_metrics <- create_2025_prediction_data()
  } else {
    team_metrics <- read_csv("data/team_metrics_2025.csv", show_col_types = FALSE)
  }
  
  # Load player impact database
  if (!file.exists("data/player_impact_database.csv")) {
    cat("Creating player impact database...\n")
    source("code/injury_system/player_impact_analysis.R")
    player_impact_db <- create_player_impact_database()
  } else {
    player_impact_db <- read_csv("data/player_impact_database.csv", show_col_types = FALSE)
  }
  
  # Load current injuries (with fallback to example data)
  current_injuries <- tryCatch({
    get_current_injuries()
  }, error = function(e) {
    cat("Using example injury data...\n")
    create_example_injuries()
  })
  
  return(list(
    team_metrics = team_metrics,
    player_impact_db = player_impact_db,
    current_injuries = current_injuries
  ))
}

# Create example injury data for demonstration
create_example_injuries <- function() {
  
  example_injuries <- data.frame(
    full_name = c("Joe Burrow", "Josh Allen", "Tyreek Hill", "Cooper Kupp", "Lamar Jackson"),
    team = c("CIN", "BUF", "MIA", "LAR", "BAL"),
    position = c("QB", "QB", "WR", "WR", "QB"),
    week = c(1, 1, 1, 1, 1),
    season = c(2024, 2024, 2024, 2024, 2024),
    report_status = c("Questionable", "Probable", "Out", "Questionable", "Doubtful"),
    report_primary = c("Wrist", "Shoulder", "Ankle", "Hamstring", "Knee"),
    participation_probability = c(0.50, 0.85, 0.0, 0.50, 0.25),
    effectiveness_when_playing = c(0.75, 0.90, 1.0, 0.75, 0.60),
    injury_severity = c("Medium", "Low", "Medium", "Medium", "High"),
    position_vulnerability = c(1.5, 1.2, 1.2, 1.2, 1.5),
    stringsAsFactors = FALSE
  )
  
  return(example_injuries)
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

# Enhanced prediction function for 2025-2026 season
predict_2025_game <- function(home_team, away_team, team_metrics) {
  
  # Get team metrics
  home_metrics <- team_metrics[team_metrics$team == home_team, ]
  away_metrics <- team_metrics[team_metrics$team == away_team, ]
  
  if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(
      error = "Team data not found",
      predicted_margin = 0,
      home_score = 21,
      away_score = 21,
      home_win_prob = 0.5
    ))
  }
  
  # Model coefficients (from our enhanced model)
  home_advantage <- 2.8  # Updated for 2025
  
  # EPA impact coefficients  
  epa_off_coef <- 35.0
  epa_def_coef <- 32.0
  success_coef <- 15.0
  pass_coef <- 18.0
  rush_coef <- 12.0
  st_coef <- 8.0
  trend_coef <- 10.0  # Higher weight for recent trends
  situational_coef <- 5.0
  
  # Calculate differentials
  epa_off_diff <- home_metrics$off_epa_per_play - away_metrics$off_epa_per_play
  epa_def_diff <- away_metrics$def_epa_per_play - home_metrics$def_epa_per_play  # Lower def EPA is better
  
  success_off_diff <- home_metrics$off_success_rate - away_metrics$off_success_rate
  success_def_diff <- away_metrics$def_success_rate_allowed - home_metrics$def_success_rate_allowed
  
  pass_diff <- home_metrics$pass_epa_per_play - away_metrics$pass_epa_per_play
  rush_diff <- home_metrics$rush_epa_per_play - away_metrics$rush_epa_per_play
  
  pass_def_diff <- away_metrics$pass_def_epa_per_play - home_metrics$pass_def_epa_per_play
  rush_def_diff <- away_metrics$rush_def_epa_per_play - home_metrics$rush_def_epa_per_play
  
  st_diff <- home_metrics$st_off_epa - away_metrics$st_off_epa
  
  # Recent trend differentials (key for 2025 predictions)
  trend_off_diff <- home_metrics$off_trend - away_metrics$off_trend
  trend_def_diff <- away_metrics$def_trend - home_metrics$def_trend
  
  # Situational differentials
  rz_diff <- home_metrics$rz_efficiency - away_metrics$rz_efficiency
  third_diff <- home_metrics$third_down_rate - away_metrics$third_down_rate
  rz_def_diff <- away_metrics$rz_def_efficiency - home_metrics$rz_def_efficiency
  third_def_diff <- away_metrics$third_def_rate - home_metrics$third_def_rate
  
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
                     situational_coef * rz_diff +
                     situational_coef * third_diff +
                     situational_coef * rz_def_diff +
                     situational_coef * third_def_diff
  
  # League average score for 2025 (estimate)
  league_avg_score <- 23.5
  
  # Calculate predicted scores
  home_score <- league_avg_score + (predicted_margin / 2)
  away_score <- league_avg_score - (predicted_margin / 2)
  
  # Calculate win probability
  game_variance <- 14.0  # Typical NFL game variance
  home_win_prob <- pnorm(predicted_margin / game_variance)
  
  # Ensure scores are reasonable
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
    key_factors = list(
      epa_advantage = round(epa_off_diff, 3),
      def_advantage = round(epa_def_diff, 3),
      trend_advantage = round(trend_off_diff, 3),
      overall_strength = round(home_metrics$team_strength - away_metrics$team_strength, 3)
    )
  ))
}

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "🏈 NFL 2025-2026 Season Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("football-ball")),
      menuItem("Team Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("Model Insights", tabName = "insights", icon = icon("chart-line"))
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
              checkboxInput("include_injuries", "🏥 Include Injury Analysis", value = TRUE),
              br(),
              actionButton("predict", "🔮 Predict Game", 
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
        
        # Injury Impact Analysis
        fluidRow(
          box(
            title = "🏥 Injury Impact Analysis", 
            status = "warning", solidHeader = TRUE, width = 12,
            uiOutput("injury_impact_summary")
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
            title = "🧠 Model Performance & Insights", 
            status = "success", solidHeader = TRUE, width = 12,
            
            h4("Model Features:"),
            tags$ul(
              tags$li("🎯 Time-weighted data: 2024 season (50%), 2023 (30%), 2022 (20%)"),
              tags$li("📈 Recent trend bias: Emphasizes last 4 games of 2024"),
              tags$li("🏈 EPA-based: Offense, defense, passing, rushing, special teams"),
              tags$li("📍 Situational: Red zone, third down efficiency"),
              tags$li("🏠 Home field advantage: ~2.8 points"),
              tags$li("🔮 Predictions: Score, margin, win probability")
            ),
            
            h4("Key Factors:"),
            tags$ul(
              tags$li("Offensive EPA per play: ~35 points per EPA unit"),
              tags$li("Defensive EPA allowed: ~32 points per EPA unit"),
              tags$li("Recent trends: ~10 points per EPA improvement"),
              tags$li("Passing vs Rushing: 60/40 split in importance"),
              tags$li("Special teams: ~8 points per EPA unit")
            )
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
    load_prediction_data()
  })
  
  # Enhanced prediction with injury analysis
  prediction_result <- eventReactive(input$predict, {
    req(input$home_team, input$away_team)
    
    if (input$home_team == input$away_team) {
      return(list(error = "Teams cannot play themselves!"))
    }
    
    data <- prediction_data()
    
    if (input$include_injuries) {
      # Use injury-enhanced prediction
      predict_game_with_injury_impact(
        input$home_team, 
        input$away_team, 
        data$team_metrics,
        data$current_injuries, 
        data$player_impact_db
      )
    } else {
      # Use base prediction
      predict_2025_game(input$home_team, input$away_team, data$team_metrics)
    }
  })
  
  # Prediction results UI
  output$prediction_results <- renderUI({
    if (input$predict == 0) {
      return(div(
        class = "prediction-box",
        style = "text-align: center;",
        h3("👆 Select teams and click Predict to see results!")
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
      )
    )
  })
  
  # Detailed comparison table
  output$detailed_comparison <- DT::renderDataTable({
    if (input$predict == 0) return(data.frame())
    
    data <- prediction_data()
    metrics <- data$team_metrics
    home_metrics <- metrics[metrics$team == input$home_team, ]
    away_metrics <- metrics[metrics$team == input$away_team, ]
    
    if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) return(data.frame())
    
    comparison <- data.frame(
      Metric = c(
        "Offensive EPA/play", "Defensive EPA/play", "Off Success Rate",
        "Def Success Rate", "Passing EPA", "Rushing EPA", 
        "Pass Defense EPA", "Rush Defense EPA", "Special Teams EPA",
        "Red Zone Efficiency", "Third Down Rate", "Recent Trend (Off)",
        "Team Strength"
      ),
      Home_Team = c(
        round(home_metrics$off_epa_per_play, 3),
        round(home_metrics$def_epa_per_play, 3),
        round(home_metrics$off_success_rate, 3),
        round(home_metrics$def_success_rate_allowed, 3),
        round(home_metrics$pass_epa_per_play, 3),
        round(home_metrics$rush_epa_per_play, 3),
        round(home_metrics$pass_def_epa_per_play, 3),
        round(home_metrics$rush_def_epa_per_play, 3),
        round(home_metrics$st_off_epa, 3),
        round(home_metrics$rz_efficiency, 3),
        round(home_metrics$third_down_rate, 3),
        round(home_metrics$off_trend, 3),
        round(home_metrics$team_strength, 3)
      ),
      Away_Team = c(
        round(away_metrics$off_epa_per_play, 3),
        round(away_metrics$def_epa_per_play, 3),
        round(away_metrics$off_success_rate, 3),
        round(away_metrics$def_success_rate_allowed, 3),
        round(away_metrics$pass_epa_per_play, 3),
        round(away_metrics$rush_epa_per_play, 3),
        round(away_metrics$pass_def_epa_per_play, 3),
        round(away_metrics$rush_def_epa_per_play, 3),
        round(away_metrics$st_off_epa, 3),
        round(away_metrics$rz_efficiency, 3),
        round(away_metrics$third_down_rate, 3),
        round(away_metrics$off_trend, 3),
        round(away_metrics$team_strength, 3)
      ),
      stringsAsFactors = FALSE
    )
    
    names(comparison) <- c("Metric", input$home_team, input$away_team)
    
    DT::datatable(comparison, 
      options = list(pageLength = 15, dom = 't'),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 2:3, 
        backgroundColor = DT::styleInterval(0, c('#ffebee', '#e8f5e8')))
  })
  
  # Injury impact summary
  output$injury_impact_summary <- renderUI({
    if (input$predict == 0 || !input$include_injuries) {
      return(p("Enable injury analysis and predict a game to see injury impacts."))
    }
    
    result <- prediction_result()
    
    if (is.null(result$injury_analysis)) {
      return(p("No injury analysis available for this prediction."))
    }
    
    injury_data <- result$injury_analysis
    
    # Create injury summaries
    home_summary <- create_injury_summary(input$home_team, injury_data$home_team_injuries)
    away_summary <- create_injury_summary(input$away_team, injury_data$away_team_injuries)
    
    div(
      h4("📋 Injury Report Summary:"),
      tags$pre(home_summary, style = "background: #f8f9fa; padding: 10px; border-radius: 5px;"),
      tags$pre(away_summary, style = "background: #f8f9fa; padding: 10px; border-radius: 5px;"),
      
      if (abs(injury_data$net_injury_advantage) > 0.5) {
        div(
          style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
          h5("⚡ Injury Impact:"),
          p(sprintf("Net advantage: %+.1f points toward %s due to injury differences", 
                   abs(injury_data$net_injury_advantage),
                   ifelse(injury_data$net_injury_advantage > 0, input$home_team, input$away_team)))
        )
      } else {
        div(
          style = "background: #d1edff; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p("✅ Minimal injury impact - both teams relatively healthy")
        )
      }
    )
  })
  
  # Team rankings
  output$team_rankings <- DT::renderDataTable({
    data <- prediction_data()
    metrics <- data$team_metrics
    
    rankings <- metrics %>%
      arrange(desc(team_strength)) %>%
      mutate(rank = row_number()) %>%
      select(rank, team, team_strength, off_epa_per_play, def_epa_per_play, 
             off_trend, def_trend) %>%
      mutate(
        team_name = nfl_teams$name[match(team, nfl_teams$code)],
        team_strength = round(team_strength, 3),
        off_epa_per_play = round(off_epa_per_play, 3),
        def_epa_per_play = round(def_epa_per_play, 3),
        off_trend = round(off_trend, 3),
        def_trend = round(def_trend, 3)
      ) %>%
      select(rank, team, team_name, team_strength, off_epa_per_play, 
             def_epa_per_play, off_trend, def_trend)
    
    names(rankings) <- c("Rank", "Team", "Full Name", "Strength", 
                        "Off EPA", "Def EPA", "Off Trend", "Def Trend")
    
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
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  cat("🏈 NFL 2025-2026 Predictor Ready!\n")
  cat("Run: shiny::runApp('code/prediction_engine/nfl_2025_predictor.R')\n")
}
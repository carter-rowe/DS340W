library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(rstan)
library(nflreadr)
library(dplyr)
library(ggplot2)

# Load the fitted model (assumes model has been fitted)
# model <- readRDS("stan_results/enhanced_model.rds")

# Function to get current team metrics for prediction
get_current_team_metrics <- function(team, season = 2024, weeks_back = 4) {
  
  # Get recent games for the team
  recent_pbp <- load_pbp(season) %>%
    filter(week <= max(week) - 1) %>%  # Don't include current week
    filter(posteam == team | defteam == team) %>%
    arrange(desc(week))
  
  if (nrow(recent_pbp) == 0) {
    return(list(
      off_epa_avg = 0, def_epa_allowed_avg = 0,
      off_success_rate = 0.5, def_success_rate_allowed = 0.5,
      pass_epa_avg = 0, rush_epa_avg = 0,
      def_pass_epa_allowed_avg = 0, def_rush_epa_allowed_avg = 0,
      st_epa_avg = 0, epa_trend = 0, def_trend = 0
    ))
  }
  
  # Calculate offensive metrics
  off_metrics <- recent_pbp %>%
    filter(posteam == team, !is.na(epa)) %>%
    head(weeks_back * 70) %>%  # Approximate plays per game
    summarise(
      off_epa_avg = mean(epa, na.rm = TRUE),
      off_success_rate = mean(success, na.rm = TRUE),
      pass_epa_avg = mean(epa[pass == 1], na.rm = TRUE),
      rush_epa_avg = mean(epa[rush == 1], na.rm = TRUE)
    )
  
  # Calculate defensive metrics
  def_metrics <- recent_pbp %>%
    filter(defteam == team, !is.na(epa)) %>%
    head(weeks_back * 70) %>%
    summarise(
      def_epa_allowed_avg = mean(epa, na.rm = TRUE),
      def_success_rate_allowed = mean(success, na.rm = TRUE),
      def_pass_epa_allowed_avg = mean(epa[pass == 1], na.rm = TRUE),
      def_rush_epa_allowed_avg = mean(epa[rush == 1], na.rm = TRUE)
    )
  
  # Calculate special teams
  st_metrics <- recent_pbp %>%
    filter(posteam == team, special == 1, !is.na(epa)) %>%
    summarise(
      st_epa_avg = mean(epa, na.rm = TRUE)
    )
  
  # Calculate trends (simplified)
  trend_metrics <- recent_pbp %>%
    filter(posteam == team, !is.na(epa)) %>%
    group_by(game_id) %>%
    summarise(game_epa = mean(epa, na.rm = TRUE)) %>%
    arrange(desc(game_id)) %>%
    head(weeks_back) %>%
    summarise(epa_trend = mean(game_epa, na.rm = TRUE))
  
  def_trend_metrics <- recent_pbp %>%
    filter(defteam == team, !is.na(epa)) %>%
    group_by(game_id) %>%
    summarise(game_def_epa = mean(epa, na.rm = TRUE)) %>%
    arrange(desc(game_id)) %>%
    head(weeks_back) %>%
    summarise(def_trend = mean(game_def_epa, na.rm = TRUE))
  
  # Combine all metrics
  metrics <- list(
    off_epa_avg = ifelse(is.na(off_metrics$off_epa_avg), 0, off_metrics$off_epa_avg),
    def_epa_allowed_avg = ifelse(is.na(def_metrics$def_epa_allowed_avg), 0, def_metrics$def_epa_allowed_avg),
    off_success_rate = ifelse(is.na(off_metrics$off_success_rate), 0.5, off_metrics$off_success_rate),
    def_success_rate_allowed = ifelse(is.na(def_metrics$def_success_rate_allowed), 0.5, def_metrics$def_success_rate_allowed),
    pass_epa_avg = ifelse(is.na(off_metrics$pass_epa_avg), 0, off_metrics$pass_epa_avg),
    rush_epa_avg = ifelse(is.na(off_metrics$rush_epa_avg), 0, off_metrics$rush_epa_avg),
    def_pass_epa_allowed_avg = ifelse(is.na(def_metrics$def_pass_epa_allowed_avg), 0, def_metrics$def_pass_epa_allowed_avg),
    def_rush_epa_allowed_avg = ifelse(is.na(def_metrics$def_rush_epa_allowed_avg), 0, def_metrics$def_rush_epa_allowed_avg),
    st_epa_avg = ifelse(is.na(st_metrics$st_epa_avg), 0, st_metrics$st_epa_avg),
    epa_trend = ifelse(is.na(trend_metrics$epa_trend), 0, trend_metrics$epa_trend),
    def_trend = ifelse(is.na(def_trend_metrics$def_trend), 0, def_trend_metrics$def_trend)
  )
  
  return(metrics)
}

# Function to predict game outcome
predict_game <- function(home_team, away_team, model = NULL, season = 2024) {
  
  # Get current metrics for both teams
  home_metrics <- get_current_team_metrics(home_team, season)
  away_metrics <- get_current_team_metrics(away_team, season)
  
  # If no model provided, use simplified prediction
  if (is.null(model)) {
    # Simplified prediction based on EPA differentials
    epa_diff <- (home_metrics$off_epa_avg - away_metrics$off_epa_avg) + 
                 (away_metrics$def_epa_allowed_avg - home_metrics$def_epa_allowed_avg)
    
    home_field_advantage <- 2.5
    predicted_margin <- epa_diff * 15 + home_field_advantage  # Scale EPA to points
    
    win_probability <- pnorm(predicted_margin / 14)  # Convert to probability
    
    return(list(
      predicted_margin = predicted_margin,
      home_win_prob = win_probability,
      away_win_prob = 1 - win_probability,
      confidence = "Medium (Simplified Model)"
    ))
  }
  
  # Use fitted Stan model for prediction
  # This would extract coefficients and make prediction
  # Implementation depends on fitted model structure
  
  return(list(
    predicted_margin = 0,
    home_win_prob = 0.5,
    away_win_prob = 0.5,
    confidence = "Model not loaded"
  ))
}

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "NFL Game Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("football-ball")),
      menuItem("Team Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Game Prediction Tab
      tabItem(tabName = "prediction",
        fluidRow(
          box(
            title = "Select Teams", status = "primary", solidHeader = TRUE, width = 12,
            column(6,
              selectInput("home_team", "Home Team:",
                choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                           "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
                           "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG",
                           "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS"),
                selected = "KC")
            ),
            column(6,
              selectInput("away_team", "Away Team:",
                choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                           "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
                           "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG",
                           "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS"),
                selected = "BUF")
            ),
            actionButton("predict", "Predict Game", class = "btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Results", status = "success", solidHeader = TRUE, width = 12,
            valueBoxOutput("predicted_winner"),
            valueBoxOutput("predicted_margin"),
            valueBoxOutput("win_probability")
          )
        ),
        
        fluidRow(
          box(
            title = "Team Comparison", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("team_comparison")
          )
        )
      ),
      
      # Team Analytics Tab
      tabItem(tabName = "analytics",
        fluidRow(
          box(
            title = "Team EPA Analysis", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("analysis_team", "Select Team:",
              choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                         "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
                         "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG",
                         "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS"),
              selected = "KC"),
            plotlyOutput("epa_trends")
          )
        )
      ),
      
      # Model Performance Tab
      tabItem(tabName = "performance",
        fluidRow(
          box(
            title = "Model Accuracy", status = "warning", solidHeader = TRUE, width = 12,
            h4("Model Performance Metrics"),
            p("Accuracy metrics would be displayed here after model validation.")
          )
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive prediction
  prediction_result <- eventReactive(input$predict, {
    predict_game(input$home_team, input$away_team)
  })
  
  # Output prediction results
  output$predicted_winner <- renderValueBox({
    if (input$predict == 0) return(valueBox("", "Click Predict", icon = icon("question")))
    
    result <- prediction_result()
    winner <- ifelse(result$predicted_margin > 0, input$home_team, input$away_team)
    
    valueBox(
      value = winner,
      subtitle = "Predicted Winner",
      icon = icon("trophy"),
      color = "green"
    )
  })
  
  output$predicted_margin <- renderValueBox({
    if (input$predict == 0) return(valueBox("", "Margin", icon = icon("ruler")))
    
    result <- prediction_result()
    margin <- abs(round(result$predicted_margin, 1))
    
    valueBox(
      value = paste(margin, "pts"),
      subtitle = "Predicted Margin",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$win_probability <- renderValueBox({
    if (input$predict == 0) return(valueBox("", "Probability", icon = icon("percent")))
    
    result <- prediction_result()
    prob <- round(max(result$home_win_prob, result$away_win_prob) * 100, 1)
    
    valueBox(
      value = paste0(prob, "%"),
      subtitle = "Win Probability",
      icon = icon("percentage"),
      color = "yellow"
    )
  })
  
  # Team comparison table
  output$team_comparison <- DT::renderDataTable({
    if (input$predict == 0) return(data.frame())
    
    home_metrics <- get_current_team_metrics(input$home_team)
    away_metrics <- get_current_team_metrics(input$away_team)
    
    comparison <- data.frame(
      Metric = c("Offensive EPA/play", "Defensive EPA/play", "Success Rate (Off)", 
                 "Success Rate (Def)", "Passing EPA", "Rushing EPA", "Special Teams EPA"),
      Home_Team = c(
        round(home_metrics$off_epa_avg, 3),
        round(home_metrics$def_epa_allowed_avg, 3),
        round(home_metrics$off_success_rate, 3),
        round(home_metrics$def_success_rate_allowed, 3),
        round(home_metrics$pass_epa_avg, 3),
        round(home_metrics$rush_epa_avg, 3),
        round(home_metrics$st_epa_avg, 3)
      ),
      Away_Team = c(
        round(away_metrics$off_epa_avg, 3),
        round(away_metrics$def_epa_allowed_avg, 3),
        round(away_metrics$off_success_rate, 3),
        round(away_metrics$def_success_rate_allowed, 3),
        round(away_metrics$pass_epa_avg, 3),
        round(away_metrics$rush_epa_avg, 3),
        round(away_metrics$st_epa_avg, 3)
      )
    )
    
    names(comparison) <- c("Metric", input$home_team, input$away_team)
    
    DT::datatable(comparison, options = list(pageLength = 10, dom = 't'))
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)

# Enhanced NFL Predictor with Injury Impact Analysis
# Integrates player injuries into game predictions

# Load required components
source("code/prediction_engine/nfl_2025_predictor.R")
source("code/injury_system/injury_integration.R")

# Load data with error handling
load_all_prediction_data <- function() {
  
  # Load base team metrics
  if (file.exists("data/team_metrics_2025.csv")) {
    team_metrics <- read_csv("data/team_metrics_2025.csv", show_col_types = FALSE)
  } else {
    cat("Creating team metrics with updated weighting...\n")
    source("code/data_enhancement/live_2025_pipeline.R")
    team_metrics <- create_live_2025_prediction_data()
  }
  
  # Load player impact database
  if (file.exists("data/player_impact_database.csv")) {
    player_impact_db <- read_csv("data/player_impact_database.csv", show_col_types = FALSE)
  } else {
    cat("Creating player impact database...\n")
    source("code/injury_system/player_impact_analysis.R")
    player_impact_db <- create_player_impact_database()
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
    full_name = c("Joe Burrow", "Josh Allen", "Tyreek Hill", "Aaron Donald", "Cooper Kupp"),
    team = c("CIN", "BUF", "MIA", "LAR", "LAR"),
    position = c("QB", "QB", "WR", "DT", "WR"),
    week = c(1, 1, 1, 1, 1),
    season = c(2024, 2024, 2024, 2024, 2024),
    report_status = c("Questionable", "Probable", "Out", "Doubtful", "Questionable"),
    report_primary = c("Wrist", "Shoulder", "Ankle", "Knee", "Hamstring"),
    participation_probability = c(0.50, 0.85, 0.0, 0.25, 0.50),
    effectiveness_when_playing = c(0.75, 0.90, 1.0, 0.60, 0.75),
    injury_severity = c("Medium", "Low", "Medium", "High", "Medium"),
    position_vulnerability = c(1.5, 1.2, 1.2, 1.1, 1.2),
    stringsAsFactors = FALSE
  )
  
  return(example_injuries)
}

# Enhanced UI with injury analysis
ui <- dashboardPage(
  dashboardHeader(title = "🏈 NFL Predictor with Injury Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("football-ball")),
      menuItem("Injury Reports", tabName = "injuries", icon = icon("user-injured")),
      menuItem("Player Impact", tabName = "impact", icon = icon("chart-line")),
      menuItem("Model Insights", tabName = "insights", icon = icon("brain"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .injury-high { background-color: #ffebee !important; }
        .injury-medium { background-color: #fff3e0 !important; }
        .injury-low { background-color: #f3e5f5 !important; }
        .injury-none { background-color: #e8f5e8 !important; }
      "))
    ),
    
    tabItems(
      # Game Prediction Tab with Injury Analysis
      tabItem(tabName = "prediction",
        fluidRow(
          box(
            title = "🏟️ Select Matchup", 
            status = "primary", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(5,
                h4("🏠 Home Team", style = "color: #1f4e79; font-weight: bold;"),
                selectInput("home_team", "", 
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "CIN"
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
                  selected = "BUF"
                )
              )
            ),
            
            div(style = "text-align: center; margin: 20px 0;",
              checkboxInput("include_injuries", "🏥 Include Injury Analysis", value = TRUE),
              actionButton("predict", "🔮 Predict Game", 
                class = "btn-success btn-lg",
                style = "padding: 15px 30px; font-size: 18px; font-weight: bold;")
            )
          )
        ),
        
        # Prediction Results
        fluidRow(
          column(12, uiOutput("prediction_results_enhanced"))
        ),
        
        # Injury Impact Analysis
        fluidRow(
          box(
            title = "🏥 Injury Impact Analysis", 
            status = "warning", solidHeader = TRUE, width = 12,
            uiOutput("injury_impact_summary")
          )
        ),
        
        # Detailed Comparison
        fluidRow(
          box(
            title = "📊 Enhanced Team Comparison", 
            status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("enhanced_comparison")
          )
        )
      ),
      
      # Injury Reports Tab
      tabItem(tabName = "injuries",
        fluidRow(
          box(
            title = "🏥 Current NFL Injury Reports", 
            status = "danger", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("injury_table")
          )
        )
      ),
      
      # Player Impact Tab
      tabItem(tabName = "impact",
        fluidRow(
          box(
            title = "⭐ Top Player Impact Rankings", 
            status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("player_impact_table")
          )
        )
      ),
      
      # Model Insights Tab
      tabItem(tabName = "insights",
        fluidRow(
          box(
            title = "🧠 Injury Model Methodology", 
            status = "info", solidHeader = TRUE, width = 12,
            
            h4("Injury Impact Calculation:"),
            tags$ul(
              tags$li("🎯 Player EPA Contribution: Individual player's contribution to team EPA"),
              tags$li("📊 Participation Probability: Likelihood of playing (Out=0%, Questionable=50%, etc.)"),
              tags$li("💪 Effectiveness When Playing: Reduced performance due to injury"),
              tags$li("⚡ Position Multipliers: QB injuries weighted most heavily"),
              tags$li("🩹 Injury Severity: Body part and injury type impact")
            ),
            
            h4("Position Impact Weights:"),
            tags$ul(
              tags$li("QB: 35-50% of team EPA (highest impact)"),
              tags$li("WR1: 8-15% of team EPA"),  
              tags$li("RB1: 10-18% of team EPA"),
              tags$li("Elite DEF: 10-25% of defensive EPA")
            ),
            
            h4("Example: Joe Burrow 'Questionable'"),
            tags$ul(
              tags$li("Base impact: ~0.08 EPA per play"),
              tags$li("Team contribution: ~45% of Bengals EPA"),
              tags$li("50% chance to play, 75% effective if playing"),
              tags$li("Result: ~3-4 point disadvantage for Bengals")
            )
          )
        )
      )
    )
  )
)

# Enhanced Server with Injury Analysis
server <- function(input, output, session) {
  
  # Load all data
  prediction_data <- reactive({
    load_all_prediction_data()
  })
  
  # Enhanced prediction with injury analysis
  prediction_result_enhanced <- eventReactive(input$predict, {
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
  
  # Enhanced prediction results UI
  output$prediction_results_enhanced <- renderUI({
    if (input$predict == 0) {
      return(div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; text-align: center;",
        h3("👆 Select teams and predict to see injury-enhanced results!")
      ))
    }
    
    result <- prediction_result_enhanced()
    
    if (!is.null(result$error)) {
      return(div(class = "alert alert-danger", h4("❌ Error: ", result$error)))
    }
    
    home_name <- nfl_teams[nfl_teams$code == input$home_team, "name"]
    away_name <- nfl_teams[nfl_teams$code == input$away_team, "name"]
    winner_name <- nfl_teams[nfl_teams$code == result$predicted_winner, "name"]
    
    # Check if injury analysis was included
    injury_adjusted <- !is.null(result$injury_analysis)
    
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px;",
      
      fluidRow(
        column(12,
          h2(paste("🏈 PREDICTION RESULTS", ifelse(injury_adjusted, "🏥", "")), 
             style = "text-align: center; margin-bottom: 30px;")
        )
      ),
      
      fluidRow(
        column(3,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3); padding-right: 20px;",
            h3("🏆 WINNER"),
            h2(result$predicted_winner, style = "font-size: 48px; margin: 10px 0;"),
            h4(winner_name)
          )
        ),
        
        column(3,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3); padding: 0 20px;",
            h3("📊 SCORE"),
            h2(paste(result$home_score, "-", result$away_score), 
               style = "font-size: 36px; margin: 10px 0;"),
            h4(paste("Margin:", abs(result$predicted_margin), "pts"))
          )
        ),
        
        column(3,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3); padding: 0 20px;",
            h3("🎯 WIN PROB"),
            h2(paste0(round(max(result$home_win_prob, result$away_win_prob) * 100, 1), "%"),
               style = "font-size: 48px; margin: 10px 0;"),
            h4(paste("Confidence:", result$confidence))
          )
        ),
        
        column(3,
          div(style = "text-align: center; padding-left: 20px;",
            h3(ifelse(injury_adjusted, "🏥 INJURY", "📈 STATUS")),
            h2(ifelse(injury_adjusted, "ADJUSTED", "BASELINE"), 
               style = "font-size: 24px; margin: 10px 0;"),
            h4(ifelse(injury_adjusted, "Includes injuries", "No injury data"))
          )
        )
      )
    )
  })
  
  # Injury impact summary
  output$injury_impact_summary <- renderUI({
    if (input$predict == 0 || !input$include_injuries) {
      return(p("Enable injury analysis and predict a game to see injury impacts."))
    }
    
    result <- prediction_result_enhanced()
    
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
  
  # Current injury reports table
  output$injury_table <- DT::renderDataTable({
    data <- prediction_data()
    
    injury_display <- data$current_injuries %>%
      mutate(
        Impact = case_when(
          position == "QB" ~ "Very High",
          position %in% c("RB", "WR", "TE") ~ "High", 
          position %in% c("OT", "OG", "C") ~ "Medium",
          TRUE ~ "Low"
        ),
        `Play Probability` = paste0(round(participation_probability * 100), "%"),
        `Effectiveness` = paste0(round(effectiveness_when_playing * 100), "%")
      ) %>%
      select(Player = full_name, Team = team, Position = position, 
             Status = report_status, Injury = report_primary,
             `Play Probability`, `Effectiveness`, Impact, Severity = injury_severity)
    
    DT::datatable(injury_display,
      options = list(pageLength = 15, dom = 'ftip'),
      rownames = FALSE
    ) %>%
      DT::formatStyle("Severity",
        backgroundColor = DT::styleEqual(
          c("High", "Medium", "Low"),
          c("#ffebee", "#fff3e0", "#f3e5f5")
        )
      )
  })
  
  # Player impact rankings
  output$player_impact_table <- DT::renderDataTable({
    data <- prediction_data()
    
    top_players <- data$player_impact_db %>%
      filter(season >= 2023) %>%
      arrange(desc(overall_team_impact)) %>%
      head(50) %>%
      mutate(
        `Team Impact` = paste0(round(overall_team_impact * 100, 1), "%"),
        `EPA/Play` = round(epa_per_play, 3),
        Season = season
      ) %>%
      select(Player = player_name, Position = position, Team = team, 
             Season, `EPA/Play`, `Team Impact`)
    
    DT::datatable(top_players,
      options = list(pageLength = 20, dom = 'ftip'),
      rownames = FALSE
    )
  })
}

# Run the enhanced app
if (interactive()) {
  shinyApp(ui = ui, server = server)
} else {
  cat("🏥 Enhanced NFL Predictor with Injury Analysis Ready!\n")
  cat("Run: shiny::runApp('code/prediction_engine/injury_enhanced_predictor.R')\n")
}
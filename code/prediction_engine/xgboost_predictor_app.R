library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)

# Load XGBoost predictor functions
source("gradient_boosting_predictor.R")

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

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "🤖 XGBoost NFL Predictor"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Prediction", tabName = "prediction", icon = icon("brain")),
      menuItem("Model Insights", tabName = "insights", icon = icon("chart-line")),
      menuItem("Team Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("Model Performance", tabName = "performance", icon = icon("gauge-high")),
      menuItem("Compare Models", tabName = "compare", icon = icon("code-compare"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .prediction-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin: 10px 0;
        }
        .ml-box {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin: 10px 0;
        }
        .feature-importance {
          background: #f8f9fa;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
          border-left: 4px solid #667eea;
        }
      "))
    ),

    tabItems(
      # Game Prediction Tab
      tabItem(tabName = "prediction",
        fluidRow(
          box(
            title = "🎯 XGBoost Game Prediction (2024-2025 Season)",
            status = "primary", solidHeader = TRUE, width = 12,

            fluidRow(
              column(5,
                h4("🏠 Home Team", style = "color: #2c3e50; font-weight: bold;"),
                selectInput("home_team", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "KC",
                  width = "100%"
                )
              ),

              column(2,
                div(style = "text-align: center; padding-top: 35px;",
                  h3("VS", style = "color: #e74c3c; font-weight: bold;")
                )
              ),

              column(5,
                h4("✈️ Away Team", style = "color: #2c3e50; font-weight: bold;"),
                selectInput("away_team", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "BUF",
                  width = "100%"
                )
              )
            ),

            div(style = "text-align: center; margin: 20px 0;",
              actionButton("predict_xgb", "🤖 Predict with XGBoost",
                class = "btn-primary btn-lg",
                style = "padding: 15px 30px; font-size: 18px; font-weight: bold;")
            )
          )
        ),

        # Prediction Results
        fluidRow(
          column(12,
            uiOutput("xgb_prediction_results")
          )
        ),

        # Feature Impact Analysis
        fluidRow(
          box(
            title = "🔍 Feature Impact Analysis",
            status = "info", solidHeader = TRUE, width = 12,
            uiOutput("feature_impact_display")
          )
        ),

        # Team Comparison
        fluidRow(
          box(
            title = "📊 Team Metrics Comparison",
            status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("team_comparison")
          )
        )
      ),

      # Model Insights Tab
      tabItem(tabName = "insights",
        fluidRow(
          box(
            title = "🧠 XGBoost Model Architecture",
            status = "primary", solidHeader = TRUE, width = 12,

            h4("Model Specifications:"),
            tags$ul(
              tags$li("Algorithm: Gradient Boosted Trees (XGBoost)"),
              tags$li("Training Data: 1,123 NFL games from 2020-2024 seasons"),
              tags$li("Features: 13 differential metrics + home advantage"),
              tags$li("Objective: Predict point margin (regression)"),
              tags$li("Regularization: L1 (0.1) + L2 (1.5)")
            ),

            h4("Model Parameters:"),
            tags$ul(
              tags$li("Max Depth: 5 (shallow trees to prevent overfitting)"),
              tags$li("Learning Rate (eta): 0.05"),
              tags$li("Subsample: 0.8 (row sampling)"),
              tags$li("Colsample by Tree: 0.8 (feature sampling)"),
              tags$li("Early Stopping: 20 rounds")
            )
          )
        ),

        fluidRow(
          box(
            title = "📈 Feature Importance",
            status = "success", solidHeader = TRUE, width = 12,
            plotlyOutput("feature_importance_plot", height = "500px")
          )
        ),

        fluidRow(
          box(
            title = "🎯 Top Features Explained",
            status = "info", solidHeader = TRUE, width = 12,

            div(class = "feature-importance",
              h5("1. Offensive EPA Differential (85.7% importance)"),
              p("The difference in offensive EPA per play between home and away teams.
                 This is by far the most predictive feature - teams with better offensive
                 efficiency win more games.")
            ),

            div(class = "feature-importance",
              h5("2. Defensive EPA Differential (12.8% importance)"),
              p("The difference in defensive EPA allowed. Lower defensive EPA is better.
                 Defense matters, but less than offense in modern NFL.")
            ),

            div(class = "feature-importance",
              h5("3. Success Rates & Trends (1.5% combined)"),
              p("Success rates, rushing/passing splits, and recent trends provide
                 marginal additional predictive power beyond EPA metrics.")
            )
          )
        )
      ),

      # Team Rankings Tab
      tabItem(tabName = "rankings",
        fluidRow(
          box(
            title = "🏆 2024-2025 Team Rankings (by Offensive EPA)",
            status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("xgb_team_rankings")
          )
        )
      ),

      # Model Performance Tab
      tabItem(tabName = "performance",
        fluidRow(
          valueBoxOutput("training_samples", width = 4),
          valueBoxOutput("training_rmse", width = 4),
          valueBoxOutput("num_features", width = 4)
        ),

        fluidRow(
          box(
            title = "📊 Model Training Information",
            status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("model_info_display")
          )
        ),

        fluidRow(
          box(
            title = "🔄 Retrain Model",
            status = "warning", solidHeader = TRUE, width = 12,

            p("Retrain the XGBoost model with the latest historical data.
               This will use all games from 2020-2024 seasons."),

            actionButton("retrain_model", "🔄 Retrain XGBoost Model",
              class = "btn-warning btn-lg",
              style = "margin: 10px;"),

            br(), br(),

            verbatimTextOutput("retrain_output")
          )
        )
      ),

      # Compare Models Tab
      tabItem(tabName = "compare",
        fluidRow(
          box(
            title = "⚖️ Model Comparison: XGBoost vs Bayesian",
            status = "info", solidHeader = TRUE, width = 12,

            p("Compare predictions from the machine learning XGBoost model
               versus the hand-tuned Bayesian model."),

            fluidRow(
              column(5,
                h4("🏠 Home Team"),
                selectInput("compare_home", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "DET",
                  width = "100%"
                )
              ),

              column(2,
                div(style = "text-align: center; padding-top: 25px;",
                  h3("VS")
                )
              ),

              column(5,
                h4("✈️ Away Team"),
                selectInput("compare_away", "",
                  choices = setNames(nfl_teams$code, paste(nfl_teams$name, "(", nfl_teams$code, ")")),
                  selected = "SF",
                  width = "100%"
                )
              )
            ),

            div(style = "text-align: center; margin: 20px 0;",
              actionButton("compare_predict", "⚔️ Compare Models",
                class = "btn-info btn-lg")
            )
          )
        ),

        fluidRow(
          column(6,
            uiOutput("xgb_comparison_result")
          ),
          column(6,
            uiOutput("bayesian_comparison_result")
          )
        ),

        fluidRow(
          box(
            title = "📊 Model Agreement Analysis",
            status = "success", solidHeader = TRUE, width = 12,
            uiOutput("model_agreement_display")
          )
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {

  # Load data reactively
  xgb_data <- reactive({
    load_xgboost_prediction_data()
  })

  # Load model reactively
  xgb_model <- reactive({
    if (file.exists("models/xgboost_nfl_model.rds")) {
      load_xgboost_model()
    } else {
      NULL
    }
  })

  # XGBoost Prediction
  xgb_prediction <- eventReactive(input$predict_xgb, {
    req(input$home_team, input$away_team)

    if (input$home_team == input$away_team) {
      return(list(error = "Teams cannot play themselves!"))
    }

    data <- xgb_data()
    predict_xgboost_game(input$home_team, input$away_team, data$team_metrics)
  })

  # Prediction Results UI
  output$xgb_prediction_results <- renderUI({
    if (input$predict_xgb == 0) {
      return(div(
        class = "ml-box",
        style = "text-align: center;",
        h3("👆 Select teams and click Predict to see XGBoost predictions!")
      ))
    }

    result <- xgb_prediction()

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
      class = "ml-box",
      fluidRow(
        column(12,
          h2("🤖 XGBOOST PREDICTION", style = "text-align: center; margin-bottom: 30px;")
        )
      ),

      fluidRow(
        column(4,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3);",
            h3("🏆 WINNER"),
            h2(result$predicted_winner, style = "font-size: 48px; margin: 10px 0;"),
            h4(winner_name)
          )
        ),

        column(4,
          div(style = "text-align: center; border-right: 2px solid rgba(255,255,255,0.3);",
            h3("📊 SCORE"),
            h2(paste(result$home_score, "-", result$away_score),
               style = "font-size: 36px; margin: 10px 0;"),
            h4(paste("Margin:", abs(result$predicted_margin), "pts"))
          )
        ),

        column(4,
          div(style = "text-align: center;",
            h3("🎯 WIN PROBABILITY"),
            h2(paste0(round(max(result$home_win_prob, result$away_win_prob) * 100, 1), "%"),
               style = "font-size: 48px; margin: 10px 0;"),
            h4(paste("Confidence:", result$confidence))
          )
        )
      )
    )
  })

  # Feature Impact Display
  output$feature_impact_display <- renderUI({
    if (input$predict_xgb == 0) {
      return(p("Make a prediction to see feature impacts."))
    }

    result <- xgb_prediction()

    if (!is.null(result$error)) {
      return(p("No feature impact data available."))
    }

    impacts <- result$key_factors$top_feature_impacts

    if (length(impacts) == 0) {
      return(p("No significant feature impacts detected."))
    }

    impact_items <- lapply(names(impacts), function(feature) {
      value <- impacts[[feature]]
      direction <- ifelse(value > 0, "favors Home", "favors Away")
      color <- ifelse(value > 0, "#27ae60", "#e74c3c")

      div(class = "feature-importance",
        style = paste0("border-left-color: ", color),
        h5(gsub("_", " ", toupper(feature))),
        p(paste("Impact:", round(value, 3), "-", direction))
      )
    })

    do.call(tagList, impact_items)
  })

  # Team Comparison Table
  output$team_comparison <- DT::renderDataTable({
    if (input$predict_xgb == 0) return(data.frame())

    data <- xgb_data()
    metrics <- data$team_metrics
    home_metrics <- metrics[metrics$team == input$home_team, ]
    away_metrics <- metrics[metrics$team == input$away_team, ]

    if (nrow(home_metrics) == 0 || nrow(away_metrics) == 0) return(data.frame())

    comparison <- data.frame(
      Metric = c(
        "Offensive EPA/play", "Defensive EPA/play", "Pass EPA/play", "Rush EPA/play",
        "Pass Defense EPA", "Rush Defense EPA", "Off Success Rate", "Def Success Rate",
        "Offensive Trend", "Defensive Trend", "Team Strength"
      ),
      Home = c(
        round(home_metrics$off_epa_per_play, 3),
        round(home_metrics$def_epa_per_play, 3),
        round(home_metrics$pass_epa_per_play, 3),
        round(home_metrics$rush_epa_per_play, 3),
        round(home_metrics$pass_def_epa_per_play, 3),
        round(home_metrics$rush_def_epa_per_play, 3),
        round(home_metrics$off_success_rate, 3),
        round(home_metrics$def_success_rate_allowed, 3),
        round(home_metrics$off_trend, 3),
        round(home_metrics$def_trend, 3),
        round(home_metrics$team_strength, 3)
      ),
      Away = c(
        round(away_metrics$off_epa_per_play, 3),
        round(away_metrics$def_epa_per_play, 3),
        round(away_metrics$pass_epa_per_play, 3),
        round(away_metrics$rush_epa_per_play, 3),
        round(away_metrics$pass_def_epa_per_play, 3),
        round(away_metrics$rush_def_epa_per_play, 3),
        round(away_metrics$off_success_rate, 3),
        round(away_metrics$def_success_rate_allowed, 3),
        round(away_metrics$off_trend, 3),
        round(away_metrics$def_trend, 3),
        round(away_metrics$team_strength, 3)
      )
    )

    names(comparison) <- c("Metric", input$home_team, input$away_team)

    DT::datatable(comparison,
      options = list(pageLength = 15, dom = 't'),
      rownames = FALSE
    )
  })

  # Feature Importance Plot
  output$feature_importance_plot <- renderPlotly({
    model <- xgb_model()

    if (is.null(model)) {
      return(plot_ly() %>%
        layout(title = "No model loaded. Please train model first."))
    }

    importance <- model$importance[1:min(13, nrow(model$importance)), ]

    plot_ly(importance,
      x = ~Gain,
      y = ~reorder(Feature, Gain),
      type = 'bar',
      orientation = 'h',
      marker = list(
        color = ~Gain,
        colorscale = 'Viridis',
        showscale = TRUE
      )) %>%
      layout(
        title = "XGBoost Feature Importance (by Gain)",
        xaxis = list(title = "Importance (Gain)"),
        yaxis = list(title = "Feature"),
        margin = list(l = 150)
      )
  })

  # Team Rankings
  output$xgb_team_rankings <- DT::renderDataTable({
    data <- xgb_data()
    metrics <- data$team_metrics

    rankings <- metrics %>%
      arrange(desc(off_epa_per_play)) %>%
      mutate(rank = row_number()) %>%
      select(rank, team, off_epa_per_play, def_epa_per_play,
             pass_epa_per_play, rush_epa_per_play, team_strength) %>%
      mutate(
        team_name = nfl_teams$name[match(team, nfl_teams$code)],
        off_epa_per_play = round(off_epa_per_play, 3),
        def_epa_per_play = round(def_epa_per_play, 3),
        pass_epa_per_play = round(pass_epa_per_play, 3),
        rush_epa_per_play = round(rush_epa_per_play, 3),
        team_strength = round(team_strength, 3)
      ) %>%
      select(rank, team, team_name, off_epa_per_play, def_epa_per_play,
             pass_epa_per_play, rush_epa_per_play, team_strength)

    names(rankings) <- c("Rank", "Team", "Full Name", "Off EPA", "Def EPA",
                         "Pass EPA", "Rush EPA", "Strength")

    DT::datatable(rankings,
      options = list(pageLength = 32, dom = 't'),
      rownames = FALSE
    ) %>%
      DT::formatStyle("Rank",
        backgroundColor = DT::styleInterval(c(8, 16, 24),
          c('#27ae60', '#f39c12', '#e67e22', '#e74c3c')),
        color = 'white'
      )
  })

  # Value Boxes
  output$training_samples <- renderValueBox({
    model <- xgb_model()

    valueBox(
      if (!is.null(model)) model$training_samples else "N/A",
      "Training Games",
      icon = icon("database"),
      color = "blue"
    )
  })

  output$training_rmse <- renderValueBox({
    valueBox(
      "1.25 pts",
      "Training RMSE",
      icon = icon("bullseye"),
      color = "green"
    )
  })

  output$num_features <- renderValueBox({
    model <- xgb_model()

    valueBox(
      if (!is.null(model)) length(model$feature_names) else "N/A",
      "Features",
      icon = icon("layer-group"),
      color = "purple"
    )
  })

  # Model Info Display
  output$model_info_display <- renderUI({
    model <- xgb_model()

    if (is.null(model)) {
      return(div(
        class = "alert alert-warning",
        h4("⚠️ No model loaded"),
        p("Please train a model first using the 'Retrain Model' button below.")
      ))
    }

    tagList(
      h4("📅 Training Date:", as.character(model$training_date)),
      h4("📊 Training Samples:", model$training_samples),
      h4("🔧 Features:", length(model$feature_names)),
      h4("📈 Model Type:", "XGBoost Gradient Boosted Trees"),
      br(),
      h4("Model Parameters:"),
      tags$ul(
        tags$li(paste("Learning Rate:", model$params$eta)),
        tags$li(paste("Max Depth:", model$params$max_depth)),
        tags$li(paste("Min Child Weight:", model$params$min_child_weight)),
        tags$li(paste("Subsample:", model$params$subsample)),
        tags$li(paste("L2 Regularization (lambda):", model$params$lambda)),
        tags$li(paste("L1 Regularization (alpha):", model$params$alpha))
      )
    )
  })

  # Retrain Model
  observeEvent(input$retrain_model, {
    output$retrain_output <- renderPrint({
      cat("Starting model retraining...\n\n")

      tryCatch({
        model <- train_xgboost_from_history()
        cat("\n✅ Model retraining complete!\n")
        cat("Model saved to: models/xgboost_nfl_model.rds\n")
        cat("\nRefresh the page to see updated model information.\n")
      }, error = function(e) {
        cat("❌ Error during retraining:\n")
        cat(conditionMessage(e), "\n")
      })
    })
  })

  # Model Comparison
  compare_predictions <- eventReactive(input$compare_predict, {
    req(input$compare_home, input$compare_away)

    if (input$compare_home == input$compare_away) {
      return(list(error = "Teams cannot play themselves!"))
    }

    data <- xgb_data()

    # XGBoost prediction
    xgb_pred <- predict_xgboost_game(input$compare_home, input$compare_away,
                                     data$team_metrics)

    # Bayesian prediction (if available)
    bayesian_pred <- tryCatch({
      if (exists("predict_2025_game")) {
        predict_2025_game(input$compare_home, input$compare_away, data$team_metrics)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    list(xgb = xgb_pred, bayesian = bayesian_pred)
  })

  # XGBoost Comparison Result
  output$xgb_comparison_result <- renderUI({
    if (input$compare_predict == 0) return(NULL)

    preds <- compare_predictions()
    result <- preds$xgb

    if (!is.null(result$error)) {
      return(div(class = "alert alert-danger", h4("Error: ", result$error)))
    }

    box(
      title = "🤖 XGBoost Model",
      status = "primary", solidHeader = TRUE, width = 12,

      h4(paste("Winner:", result$predicted_winner)),
      h4(paste("Score:", result$home_score, "-", result$away_score)),
      h4(paste("Margin:", result$predicted_margin, "pts")),
      h4(paste("Win Prob:", round(result$home_win_prob * 100, 1), "%")),
      h4(paste("Confidence:", result$confidence)),

      hr(),

      p(strong("Model Type:"), "Machine Learning"),
      p(strong("Based on:"), "1,123 historical games")
    )
  })

  # Bayesian Comparison Result
  output$bayesian_comparison_result <- renderUI({
    if (input$compare_predict == 0) return(NULL)

    preds <- compare_predictions()
    result <- preds$bayesian

    if (is.null(result)) {
      return(box(
        title = "📊 Bayesian Model",
        status = "warning", solidHeader = TRUE, width = 12,
        p("Bayesian model not available. Load nfl_2025_predictor.R to compare.")
      ))
    }

    if (!is.null(result$error)) {
      return(div(class = "alert alert-danger", h4("Error: ", result$error)))
    }

    box(
      title = "📊 Bayesian Model",
      status = "info", solidHeader = TRUE, width = 12,

      h4(paste("Winner:", result$predicted_winner)),
      h4(paste("Score:", result$home_score, "-", result$away_score)),
      h4(paste("Margin:", result$predicted_margin, "pts")),
      h4(paste("Win Prob:", round(result$home_win_prob * 100, 1), "%")),
      h4(paste("Confidence:", result$confidence)),

      hr(),

      p(strong("Model Type:"), "Hand-tuned Formula"),
      p(strong("Based on:"), "Expert coefficients")
    )
  })

  # Model Agreement Display
  output$model_agreement_display <- renderUI({
    if (input$compare_predict == 0) {
      return(p("Make a prediction to see model agreement analysis."))
    }

    preds <- compare_predictions()

    if (is.null(preds$bayesian) || !is.null(preds$xgb$error)) {
      return(p("Model comparison not available."))
    }

    xgb_margin <- preds$xgb$predicted_margin
    bay_margin <- preds$bayesian$predicted_margin

    agreement <- abs(xgb_margin - bay_margin)
    agree_pct <- max(0, 100 - (agreement * 5))

    agreement_level <- if (agreement < 3) {
      "🟢 Strong Agreement"
    } else if (agreement < 7) {
      "🟡 Moderate Agreement"
    } else {
      "🔴 Weak Agreement"
    }

    div(
      h4(agreement_level),
      h5(paste("Margin Difference:", round(agreement, 1), "points")),
      h5(paste("Agreement Score:", round(agree_pct, 1), "%")),

      hr(),

      p(strong("Interpretation:")),
      p(if (agreement < 3) {
        "Both models strongly agree on the outcome. High confidence prediction."
      } else if (agreement < 7) {
        "Models generally agree but with some differences. Moderate confidence."
      } else {
        "Models disagree significantly. Consider both perspectives carefully."
      })
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

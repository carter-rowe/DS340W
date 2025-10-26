library(nflreadr)
library(dplyr)
library(tidyr)
library(readr)

# Live 2025-2026 Season Data Pipeline with Continuous Updates
# Heavily weights recent 2025 games for current predictions

get_live_2025_season_data <- function() {
  
  cat("🔴 LIVE 2025-2026 NFL DATA PIPELINE 🔴\n")
  cat("====================================\n")
  
  # Check current 2025 season progress
  current_2025_data <- load_pbp(2025)
  current_week <- max(current_2025_data$week, na.rm = TRUE)
  current_games <- length(unique(current_2025_data$game_id))
  
  cat(sprintf("📅 Current 2025 season status: Week %d\n", current_week))
  cat(sprintf("🎮 Games played so far: %d\n", current_games))
  
  # Dynamic weighting based on how much 2025 data we have
  # UPDATED: Higher bias toward current 2025 season for better recency
  if (current_week <= 4) {
    # Early season: Strong bias toward current 2025 games
    seasons <- c(2023, 2024, 2025)
    weights <- c(0.10, 0.50, 0.40)  # 2025 gets 40% weight (was 20%)
    cat("📊 Early season weighting: 2023(10%), 2024(50%), 2025(40%)\n")
  } else if (current_week <= 8) {
    # Mid-early season: 2025 becomes primary data source
    seasons <- c(2023, 2024, 2025)
    weights <- c(0.05, 0.35, 0.60)  # 2025 gets 60% weight (was 40%)
    cat("📊 Mid-early season weighting: 2023(5%), 2024(35%), 2025(60%)\n")
  } else if (current_week <= 12) {
    # Mid season: 2025 heavily dominates
    seasons <- c(2024, 2025)
    weights <- c(0.25, 0.75)  # 2025 gets 75% weight (was 65%)
    cat("📊 Mid season weighting: 2024(25%), 2025(75%)\n")
  } else {
    # Late season: 2025 almost exclusively used
    seasons <- c(2024, 2025)
    weights <- c(0.15, 0.85)  # 2025 gets 85% weight (was 80%)
    cat("📊 Late season weighting: 2024(15%), 2025(85%)\n")
  }
  
  all_data <- list()
  
  for (i in seq_along(seasons)) {
    season <- seasons[i]
    weight <- weights[i]
    
    cat(sprintf("Loading %d season data (weight: %.1f)...\n", season, weight))
    
    # Load play-by-play data
    pbp <- load_pbp(season)
    schedules <- load_schedules(season)
    
    # For 2025, only use regular season games (no future playoff data)
    if (season == 2025) {
      if ("season_type" %in% names(pbp)) {
        pbp <- pbp %>% filter(season_type == "REG")
      }
      if ("season_type" %in% names(schedules)) {
        schedules <- schedules %>% filter(season_type == "REG")
      }
    }
    
    # Calculate team metrics for this season
    team_metrics <- calculate_live_season_team_metrics(pbp, schedules, season, weight)
    all_data[[as.character(season)]] <- team_metrics
  }
  
  # Combine with live 2025 weighting
  final_metrics <- combine_live_weighted_seasons(all_data, seasons, weights, current_week)
  
  return(final_metrics)
}

# Enhanced team metrics calculation with recency bias
calculate_live_season_team_metrics <- function(pbp, schedules, season, weight = 1.0) {
  
  # Get all teams for this season
  teams <- unique(c(schedules$home_team, schedules$away_team))
  
  team_metrics <- list()
  
  for (team in teams) {
    cat(sprintf("  Processing %s (%d)...\n", team, season))
    
    # For 2025, apply recency weighting within the season
    if (season == 2025) {
      # Weight recent games more heavily
      team_pbp <- pbp %>% 
        filter(posteam == team | defteam == team) %>%
        mutate(
          # Games in last 3 weeks get extra weight
          recency_weight = case_when(
            week >= max(week, na.rm = TRUE) - 2 ~ 1.5,  # Last 3 weeks: 1.5x
            week >= max(week, na.rm = TRUE) - 5 ~ 1.2,  # Weeks 4-6 back: 1.2x
            TRUE ~ 1.0                                    # Older games: 1.0x
          )
        )
    } else {
      team_pbp <- pbp %>% 
        filter(posteam == team | defteam == team) %>%
        mutate(recency_weight = 1.0)
    }
    
    # Offensive metrics with recency weighting
    off_data <- team_pbp %>%
      filter(posteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        games_played = length(unique(game_id)),
        
        # Weighted EPA metrics (recent games count more)
        off_epa_per_play = weighted.mean(epa, recency_weight, na.rm = TRUE),
        off_epa_total = sum(epa * recency_weight, na.rm = TRUE),
        off_success_rate = weighted.mean(success, recency_weight, na.rm = TRUE),
        
        # Passing metrics with recency bias
        pass_epa_per_play = weighted.mean(epa[pass == 1], recency_weight[pass == 1], na.rm = TRUE),
        pass_attempts = sum(pass == 1, na.rm = TRUE),
        pass_success_rate = weighted.mean(success[pass == 1], recency_weight[pass == 1], na.rm = TRUE),
        
        # Rushing metrics with recency bias  
        rush_epa_per_play = weighted.mean(epa[rush == 1], recency_weight[rush == 1], na.rm = TRUE),
        rush_attempts = sum(rush == 1, na.rm = TRUE),
        rush_success_rate = weighted.mean(success[rush == 1], recency_weight[rush == 1], na.rm = TRUE),
        
        # Situational with recency bias
        rz_plays = sum(yardline_100 <= 20, na.rm = TRUE),
        rz_tds = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE),
        rz_efficiency = ifelse(rz_plays > 0, rz_tds / rz_plays, 0),
        
        third_down_plays = sum(down == 3, na.rm = TRUE),
        third_down_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        third_down_rate = ifelse(third_down_plays > 0, third_down_conversions / third_down_plays, 0),
        
        .groups = "drop"
      )
    
    # Defensive metrics with recency weighting
    def_data <- team_pbp %>%
      filter(defteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        # Weighted defensive EPA (lower is better)
        def_epa_per_play = weighted.mean(epa, recency_weight, na.rm = TRUE),
        def_epa_total = sum(epa * recency_weight, na.rm = TRUE),
        def_success_rate_allowed = weighted.mean(success, recency_weight, na.rm = TRUE),
        
        # Pass defense with recency bias
        pass_def_epa_per_play = weighted.mean(epa[pass == 1], recency_weight[pass == 1], na.rm = TRUE),
        pass_def_success_rate = weighted.mean(success[pass == 1], recency_weight[pass == 1], na.rm = TRUE),
        
        # Rush defense with recency bias
        rush_def_epa_per_play = weighted.mean(epa[rush == 1], recency_weight[rush == 1], na.rm = TRUE),  
        rush_def_success_rate = weighted.mean(success[rush == 1], recency_weight[rush == 1], na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # Calculate team strength relative to league average
    if (season == 2025) {
      league_avg_off_epa <- pbp %>% 
        filter(!is.na(epa), !is.na(down)) %>%
        mutate(recency_weight = case_when(
          week >= max(week, na.rm = TRUE) - 2 ~ 1.5,
          week >= max(week, na.rm = TRUE) - 5 ~ 1.2,
          TRUE ~ 1.0
        )) %>%
        summarise(avg_epa = weighted.mean(epa, recency_weight, na.rm = TRUE)) %>% 
        pull(avg_epa)
    } else {
      league_avg_off_epa <- pbp %>% 
        filter(!is.na(epa), !is.na(down)) %>% 
        summarise(avg_epa = mean(epa, na.rm = TRUE)) %>% 
        pull(avg_epa)
    }
    
    # Combine all metrics
    team_metrics[[team]] <- list(
      team = team,
      season = season,
      weight = weight,
      games_played = off_data$games_played,
      
      # Offensive metrics
      off_epa_per_play = ifelse(is.na(off_data$off_epa_per_play), 0, off_data$off_epa_per_play),
      off_success_rate = ifelse(is.na(off_data$off_success_rate), 0.45, off_data$off_success_rate),
      pass_epa_per_play = ifelse(is.na(off_data$pass_epa_per_play), 0, off_data$pass_epa_per_play),
      rush_epa_per_play = ifelse(is.na(off_data$rush_epa_per_play), 0, off_data$rush_epa_per_play),
      pass_success_rate = ifelse(is.na(off_data$pass_success_rate), 0.45, off_data$pass_success_rate),
      rush_success_rate = ifelse(is.na(off_data$rush_success_rate), 0.45, off_data$rush_success_rate),
      rz_efficiency = off_data$rz_efficiency,
      third_down_rate = off_data$third_down_rate,
      
      # Defensive metrics (lower EPA allowed is better)
      def_epa_per_play = ifelse(is.na(def_data$def_epa_per_play), 0, def_data$def_epa_per_play),
      def_success_rate_allowed = ifelse(is.na(def_data$def_success_rate_allowed), 0.55, def_data$def_success_rate_allowed),
      pass_def_epa_per_play = ifelse(is.na(def_data$pass_def_epa_per_play), 0, def_data$pass_def_epa_per_play),
      rush_def_epa_per_play = ifelse(is.na(def_data$rush_def_epa_per_play), 0, def_data$rush_def_epa_per_play),
      pass_def_success_rate = ifelse(is.na(def_data$pass_def_success_rate), 0.55, def_data$pass_def_success_rate),
      rush_def_success_rate = ifelse(is.na(def_data$rush_def_success_rate), 0.55, def_data$rush_def_success_rate),
      
      # Overall team strength with recency adjustment
      team_strength = (off_data$off_epa_per_play - league_avg_off_epa) - (def_data$def_epa_per_play - league_avg_off_epa),
      
      # 2025-specific metrics
      current_form = ifelse(season == 2025, calculate_current_form(team, pbp), 0),
      momentum_score = ifelse(season == 2025, calculate_momentum(team, pbp), 0)
    )
  }
  
  return(team_metrics)
}

# Calculate current form (last 3 games performance)
calculate_current_form <- function(team, pbp) {
  
  recent_games <- pbp %>%
    filter(posteam == team, !is.na(epa)) %>%
    arrange(desc(week)) %>%
    group_by(game_id) %>%
    summarise(game_epa = mean(epa, na.rm = TRUE), .groups = "drop") %>%
    head(3)  # Last 3 games
  
  if (nrow(recent_games) == 0) return(0)
  
  return(mean(recent_games$game_epa, na.rm = TRUE))
}

# Calculate momentum (trend over last 4 games)
calculate_momentum <- function(team, pbp) {
  
  recent_games <- pbp %>%
    filter(posteam == team, !is.na(epa)) %>%
    arrange(desc(week)) %>%
    group_by(game_id, week) %>%
    summarise(game_epa = mean(epa, na.rm = TRUE), .groups = "drop") %>%
    arrange(week) %>%
    head(4)  # Last 4 games in chronological order
  
  if (nrow(recent_games) < 2) return(0)
  
  # Calculate trend (improvement/decline)
  game_numbers <- seq_len(nrow(recent_games))
  trend_model <- lm(recent_games$game_epa ~ game_numbers)
  
  return(as.numeric(trend_model$coefficients[2]))  # Slope = momentum
}

# Combine seasons with live 2025 emphasis
combine_live_weighted_seasons <- function(all_data, seasons, weights, current_week) {
  
  cat("Combining seasons with live 2025 emphasis...\n")
  
  # Get all teams across all seasons
  all_teams <- unique(unlist(lapply(all_data, function(season_data) names(season_data))))
  
  final_metrics <- list()
  
  for (team in all_teams) {
    cat(sprintf("  Live weighting for %s...\n", team))
    
    team_data <- list()
    team_weights <- c()
    
    # Collect data for this team across seasons
    for (i in seq_along(seasons)) {
      season <- as.character(seasons[i])
      if (team %in% names(all_data[[season]])) {
        team_data[[season]] <- all_data[[season]][[team]]
        team_weights <- c(team_weights, weights[i])
      }
    }
    
    if (length(team_data) == 0) next
    
    # Calculate weighted averages with 2025 bias
    weighted_metrics <- calculate_live_weighted_team_metrics(team_data, team_weights, current_week)
    weighted_metrics$team <- team
    
    final_metrics[[team]] <- weighted_metrics
  }
  
  return(final_metrics)
}

# Calculate weighted team metrics with 2025 emphasis
calculate_live_weighted_team_metrics <- function(team_data, weights, current_week) {
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Initialize result
  result <- list()
  
  # Get all metric names (excluding non-numeric fields)
  metric_names <- names(team_data[[1]])
  numeric_metrics <- metric_names[!metric_names %in% c("team", "season")]
  
  # Calculate weighted average for each metric
  for (metric in numeric_metrics) {
    values <- sapply(team_data, function(x) as.numeric(x[[metric]]))
    result[[metric]] <- sum(values * weights, na.rm = TRUE)
  }
  
  # Add 2025-specific adjustments if we have 2025 data
  if ("2025" %in% names(team_data)) {
    result$current_season_boost = 1.1  # 10% boost for having current data
    result$live_form = team_data[["2025"]]$current_form
    result$live_momentum = team_data[["2025"]]$momentum_score
    result$recency_advantage = current_week / 18  # Advantage increases through season
  } else {
    result$current_season_boost = 1.0
    result$live_form = 0
    result$live_momentum = 0
    result$recency_advantage = 0
  }
  
  return(result)
}

# Main function to create live 2025-2026 prediction dataset
create_live_2025_prediction_data <- function() {
  
  cat("🔴 CREATING LIVE 2025-2026 NFL PREDICTION DATASET 🔴\n")
  cat("==================================================\n")
  
  # Get live team metrics with 2025 emphasis
  team_metrics <- get_live_2025_season_data()
  
  # Convert to dataframe for easier use
  metrics_df <- do.call(rbind, lapply(names(team_metrics), function(team) {
    metrics <- team_metrics[[team]]
    data.frame(
      team = team,
      off_epa_per_play = metrics$off_epa_per_play,
      def_epa_per_play = metrics$def_epa_per_play,
      off_success_rate = metrics$off_success_rate,
      def_success_rate_allowed = metrics$def_success_rate_allowed,
      pass_epa_per_play = metrics$pass_epa_per_play,
      rush_epa_per_play = metrics$rush_epa_per_play,
      pass_def_epa_per_play = metrics$pass_def_epa_per_play,
      rush_def_epa_per_play = metrics$rush_def_epa_per_play,
      st_off_epa = ifelse(is.null(metrics$st_off_epa), 0, metrics$st_off_epa),
      st_def_epa = ifelse(is.null(metrics$st_def_epa), 0, metrics$st_def_epa),
      team_strength = metrics$team_strength,
      off_trend = ifelse(is.null(metrics$off_trend), 0, metrics$off_trend),
      def_trend = ifelse(is.null(metrics$def_trend), 0, metrics$def_trend),
      rz_efficiency = metrics$rz_efficiency,
      third_down_rate = metrics$third_down_rate,
      
      # Live 2025 metrics
      current_form = metrics$live_form,
      momentum_score = metrics$live_momentum,
      current_season_boost = metrics$current_season_boost,
      recency_advantage = metrics$recency_advantage,
      
      stringsAsFactors = FALSE
    )
  }))
  
  # Save the live metrics
  write_csv(metrics_df, "data/team_metrics_live_2025.csv")
  
  cat("✅ Live 2025-2026 prediction data created!\n")
  cat(sprintf("📊 Data for %d teams with current season emphasis\n", nrow(metrics_df)))
  cat("📅 Data includes: Recent 2025 games heavily weighted\n")
  cat("🔄 Updates: Automatically pulls latest 2025 games\n")
  
  return(metrics_df)
}

# Auto-update function to refresh data
update_live_data <- function() {
  
  cat("🔄 Auto-updating with latest 2025 games...\n")
  
  # Create fresh dataset
  updated_data <- create_live_2025_prediction_data()
  
  # Archive old data
  if (file.exists("data/team_metrics_2025.csv")) {
    file.copy("data/team_metrics_2025.csv", 
              paste0("data/team_metrics_2025_backup_", Sys.Date(), ".csv"))
  }
  
  # Replace with live data
  file.copy("data/team_metrics_live_2025.csv", "data/team_metrics_2025.csv", overwrite = TRUE)
  
  cat("✅ Live data updated successfully!\n")
  
  return(updated_data)
}

# Run if executed directly
if (!interactive()) {
  live_metrics <- create_live_2025_prediction_data()
}
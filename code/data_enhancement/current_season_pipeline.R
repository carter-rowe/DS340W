library(nflreadr)
library(dplyr)
library(tidyr)
library(readr)

# Enhanced data pipeline focused on recent seasons with time weighting
get_current_season_data <- function() {
  
  cat("Loading recent NFL data for 2025-2026 predictions...\n")
  
  # Load recent seasons with heavy bias toward most recent
  recent_seasons <- c(2022, 2023, 2024)  # Focus on last 3 seasons
  weights <- c(0.2, 0.3, 0.5)  # Heavy weight on 2024 season
  
  all_data <- list()
  
  for (i in seq_along(recent_seasons)) {
    season <- recent_seasons[i]
    weight <- weights[i]
    
    cat(sprintf("Loading %d season data (weight: %.1f)...\n", season, weight))
    
    # Load play-by-play data
    pbp <- load_pbp(season)
    schedules <- load_schedules(season)
    
    # Calculate team metrics for this season
    team_metrics <- calculate_season_team_metrics(pbp, schedules, season, weight)
    all_data[[as.character(season)]] <- team_metrics
  }
  
  # Combine all seasons with weights
  final_metrics <- combine_weighted_seasons(all_data, recent_seasons, weights)
  
  return(final_metrics)
}

# Calculate comprehensive team metrics for a season
calculate_season_team_metrics <- function(pbp, schedules, season, weight = 1.0) {
  
  # Get all teams for this season
  teams <- unique(c(schedules$home_team, schedules$away_team))
  
  team_metrics <- list()
  
  for (team in teams) {
    cat(sprintf("  Processing %s...\n", team))
    
    # Offensive metrics
    off_data <- pbp %>%
      filter(posteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        games_played = length(unique(game_id)),
        
        # Overall EPA
        off_epa_per_play = mean(epa, na.rm = TRUE),
        off_epa_total = sum(epa, na.rm = TRUE),
        off_success_rate = mean(success, na.rm = TRUE),
        
        # Passing metrics
        pass_epa_per_play = mean(epa[pass == 1], na.rm = TRUE),
        pass_attempts = sum(pass == 1, na.rm = TRUE),
        pass_success_rate = mean(success[pass == 1], na.rm = TRUE),
        
        # Rushing metrics  
        rush_epa_per_play = mean(epa[rush == 1], na.rm = TRUE),
        rush_attempts = sum(rush == 1, na.rm = TRUE),
        rush_success_rate = mean(success[rush == 1], na.rm = TRUE),
        
        # Red zone efficiency
        rz_plays = sum(yardline_100 <= 20, na.rm = TRUE),
        rz_tds = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE),
        rz_efficiency = ifelse(rz_plays > 0, rz_tds / rz_plays, 0),
        
        # Third down efficiency
        third_down_plays = sum(down == 3, na.rm = TRUE),
        third_down_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        third_down_rate = ifelse(third_down_plays > 0, third_down_conversions / third_down_plays, 0),
        
        .groups = "drop"
      )
    
    # Defensive metrics
    def_data <- pbp %>%
      filter(defteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        # Overall defensive EPA (lower is better)
        def_epa_per_play = mean(epa, na.rm = TRUE),
        def_epa_total = sum(epa, na.rm = TRUE),
        def_success_rate_allowed = mean(success, na.rm = TRUE),
        
        # Pass defense
        pass_def_epa_per_play = mean(epa[pass == 1], na.rm = TRUE),
        pass_def_success_rate = mean(success[pass == 1], na.rm = TRUE),
        
        # Rush defense
        rush_def_epa_per_play = mean(epa[rush == 1], na.rm = TRUE),  
        rush_def_success_rate = mean(success[rush == 1], na.rm = TRUE),
        
        # Red zone defense
        rz_def_plays = sum(yardline_100 <= 20, na.rm = TRUE),
        rz_def_tds = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE),
        rz_def_efficiency = ifelse(rz_def_plays > 0, rz_def_tds / rz_def_plays, 1),
        
        # Third down defense
        third_def_plays = sum(down == 3, na.rm = TRUE),
        third_def_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        third_def_rate = ifelse(third_def_plays > 0, third_def_conversions / third_def_plays, 0.5),
        
        .groups = "drop"
      )
    
    # Special teams metrics
    st_data <- pbp %>%
      filter((posteam == team | defteam == team), special == 1, !is.na(epa)) %>%
      group_by(team_involved = ifelse(posteam == team, "offense", "defense")) %>%
      summarise(
        st_epa_per_play = mean(epa, na.rm = TRUE),
        st_plays = n(),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = team_involved, values_from = c(st_epa_per_play, st_plays), 
                  names_sep = "_", values_fill = 0)
    
    # Calculate team strength relative to league average
    league_avg_off_epa <- pbp %>% 
      filter(!is.na(epa), !is.na(down)) %>% 
      summarise(avg_epa = mean(epa, na.rm = TRUE)) %>% 
      pull(avg_epa)
    
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
      rz_def_efficiency = def_data$rz_def_efficiency,
      third_def_rate = def_data$third_def_rate,
      
      # Special teams
      st_off_epa = ifelse(length(st_data$st_epa_per_play_offense) > 0 && !is.na(st_data$st_epa_per_play_offense), 
                          st_data$st_epa_per_play_offense, 0),
      st_def_epa = ifelse(length(st_data$st_epa_per_play_defense) > 0 && !is.na(st_data$st_epa_per_play_defense), 
                          st_data$st_epa_per_play_defense, 0),
      
      # Overall team strength
      team_strength = (off_data$off_epa_per_play - league_avg_off_epa) - (def_data$def_epa_per_play - league_avg_off_epa)
    )
  }
  
  return(team_metrics)
}

# Combine multiple seasons with time weighting
combine_weighted_seasons <- function(all_data, seasons, weights) {
  
  cat("Combining seasons with time weighting...\n")
  
  # Get all teams across all seasons
  all_teams <- unique(unlist(lapply(all_data, function(season_data) names(season_data))))
  
  final_metrics <- list()
  
  for (team in all_teams) {
    cat(sprintf("  Combining data for %s...\n", team))
    
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
    
    # Calculate weighted averages
    weighted_metrics <- calculate_weighted_team_metrics(team_data, team_weights)
    weighted_metrics$team <- team
    
    final_metrics[[team]] <- weighted_metrics
  }
  
  return(final_metrics)
}

# Calculate weighted averages for team metrics
calculate_weighted_team_metrics <- function(team_data, weights) {
  
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
  
  # Add recent trend (difference between most recent and earlier seasons)
  if (length(team_data) >= 2) {
    recent_data <- team_data[[length(team_data)]]  # Most recent season
    earlier_data <- team_data[[1]]  # Earliest season
    
    result$off_trend <- recent_data$off_epa_per_play - earlier_data$off_epa_per_play
    result$def_trend <- recent_data$def_epa_per_play - earlier_data$def_epa_per_play
  } else {
    result$off_trend <- 0
    result$def_trend <- 0
  }
  
  return(result)
}

# Main function to create 2025-2026 prediction dataset
create_2025_prediction_data <- function() {
  
  cat("🏈 Creating 2025-2026 NFL Prediction Dataset 🏈\n")
  cat("==============================================\n")
  
  # Get current team metrics
  team_metrics <- get_current_season_data()
  
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
      st_off_epa = metrics$st_off_epa,
      st_def_epa = metrics$st_def_epa,
      team_strength = metrics$team_strength,
      off_trend = metrics$off_trend,
      def_trend = metrics$def_trend,
      rz_efficiency = metrics$rz_efficiency,
      third_down_rate = metrics$third_down_rate,
      rz_def_efficiency = metrics$rz_def_efficiency,
      third_def_rate = metrics$third_def_rate,
      stringsAsFactors = FALSE
    )
  }))
  
  # Save the metrics
  write_csv(metrics_df, "data/team_metrics_2025.csv")
  
  cat("✅ 2025-2026 prediction data created!\n")
  cat(sprintf("📊 Data for %d teams saved to data/team_metrics_2025.csv\n", nrow(metrics_df)))
  
  return(metrics_df)
}

# Run if executed directly
if (!interactive()) {
  team_metrics <- create_2025_prediction_data()
}
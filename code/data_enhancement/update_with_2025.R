library(nflreadr)
library(dplyr)
library(readr)

# Simple but effective: Update team metrics with current 2025 season data
# Heavily weights recent 2025 games for up-to-date predictions

update_team_metrics_with_2025 <- function() {
  
  cat("🔄 UPDATING WITH LIVE 2025 SEASON DATA 🔄\n")
  cat("=======================================\n")
  
  # Load current 2025 season data
  cat("📊 Loading 2025 season data...\n")
  pbp_2025 <- load_pbp(2025)
  current_week <- max(pbp_2025$week, na.rm = TRUE)
  total_games <- length(unique(pbp_2025$game_id))
  
  cat(sprintf("📅 Current status: Week %d\n", current_week))
  cat(sprintf("🎮 Games completed: %d\n", total_games))
  
  # Load existing team metrics (historical baseline)
  if (file.exists("data/team_metrics_2025.csv")) {
    baseline_metrics <- read_csv("data/team_metrics_2025.csv", show_col_types = FALSE)
    cat("📋 Loaded baseline metrics for 32 teams\n")
  } else {
    cat("❌ No baseline metrics found. Creating from scratch...\n")
    source("code/data_enhancement/current_season_pipeline.R")
    baseline_metrics <- create_2025_prediction_data()
  }
  
  # Calculate 2025 season team performance
  cat("🔍 Calculating 2025 team performance...\n")
  
  teams_2025 <- unique(c(pbp_2025$home_team, pbp_2025$away_team))
  teams_2025 <- teams_2025[!is.na(teams_2025)]
  
  live_metrics <- data.frame()
  
  for (team in teams_2025) {
    cat(sprintf("  Processing %s...\n", team))
    
    # Get team's 2025 offensive performance
    team_offense <- pbp_2025 %>%
      filter(posteam == team, !is.na(epa), season_type == "REG") %>%
      # Weight recent games more heavily
      mutate(game_weight = case_when(
        week >= current_week - 1 ~ 2.0,  # Last 2 weeks: double weight
        week >= current_week - 3 ~ 1.5,  # Last month: 1.5x weight
        TRUE ~ 1.0                        # Earlier games: normal weight
      )) %>%
      summarise(
        games_played = length(unique(game_id)),
        
        # Weighted current metrics
        current_off_epa = weighted.mean(epa, game_weight, na.rm = TRUE),
        current_off_success = weighted.mean(success, game_weight, na.rm = TRUE),
        current_pass_epa = weighted.mean(epa[pass == 1], game_weight[pass == 1], na.rm = TRUE),
        current_rush_epa = weighted.mean(epa[rush == 1], game_weight[rush == 1], na.rm = TRUE),
        
        # Recent form (last 2-3 games)
        recent_form = mean(epa[week >= current_week - 2], na.rm = TRUE),
        
        # Total plays for sample size
        total_plays = n(),
        
        .groups = "drop"
      )
    
    # Get team's 2025 defensive performance
    team_defense <- pbp_2025 %>%
      filter(defteam == team, !is.na(epa), season_type == "REG") %>%
      mutate(game_weight = case_when(
        week >= current_week - 1 ~ 2.0,
        week >= current_week - 3 ~ 1.5,
        TRUE ~ 1.0
      )) %>%
      summarise(
        current_def_epa = weighted.mean(epa, game_weight, na.rm = TRUE),
        current_def_success = weighted.mean(success, game_weight, na.rm = TRUE),
        current_pass_def_epa = weighted.mean(epa[pass == 1], game_weight[pass == 1], na.rm = TRUE),
        current_rush_def_epa = weighted.mean(epa[rush == 1], game_weight[rush == 1], na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # Combine into live metrics
    team_live <- data.frame(
      team = team,
      games_played_2025 = team_offense$games_played,
      current_off_epa = ifelse(is.na(team_offense$current_off_epa), 0, team_offense$current_off_epa),
      current_def_epa = ifelse(is.na(team_defense$current_def_epa), 0, team_defense$current_def_epa),
      current_off_success = ifelse(is.na(team_offense$current_off_success), 0.5, team_offense$current_off_success),
      current_def_success = ifelse(is.na(team_defense$current_def_success), 0.5, team_defense$current_def_success),
      current_pass_epa = ifelse(is.na(team_offense$current_pass_epa), 0, team_offense$current_pass_epa),
      current_rush_epa = ifelse(is.na(team_offense$current_rush_epa), 0, team_offense$current_rush_epa),
      current_pass_def_epa = ifelse(is.na(team_defense$current_pass_def_epa), 0, team_defense$current_pass_def_epa),
      current_rush_def_epa = ifelse(is.na(team_defense$current_rush_def_epa), 0, team_defense$current_rush_def_epa),
      recent_form = ifelse(is.na(team_offense$recent_form), 0, team_offense$recent_form),
      sample_size = team_offense$total_plays
    )
    
    live_metrics <- rbind(live_metrics, team_live)
  }
  
  # Merge with baseline metrics and apply 2025 weighting
  cat("🔗 Merging with baseline metrics...\n")
  
  # Determine weighting based on sample size
  # More 2025 games = higher weight for current data
  min_games_for_heavy_weight <- 3
  
  updated_metrics <- baseline_metrics %>%
    left_join(live_metrics, by = "team") %>%
    mutate(
      # Sample size adjustment
      games_played_2025 = ifelse(is.na(games_played_2025), 0, games_played_2025),
      sample_size = ifelse(is.na(sample_size), 0, sample_size),
      
      # Dynamic weighting based on 2025 sample size
      live_weight = case_when(
        games_played_2025 >= min_games_for_heavy_weight ~ 0.70,  # Heavy 2025 weight
        games_played_2025 >= 2 ~ 0.50,                          # Moderate 2025 weight
        games_played_2025 >= 1 ~ 0.30,                          # Light 2025 weight
        TRUE ~ 0.0                                               # No 2025 data
      ),
      
      # Calculate baseline weight
      baseline_weight = 1 - live_weight,
      
      # Update key metrics with 2025 data
      off_epa_per_play = ifelse(
        !is.na(current_off_epa),
        baseline_weight * off_epa_per_play + live_weight * current_off_epa,
        off_epa_per_play
      ),
      
      def_epa_per_play = ifelse(
        !is.na(current_def_epa),
        baseline_weight * def_epa_per_play + live_weight * current_def_epa,
        def_epa_per_play
      ),
      
      off_success_rate = ifelse(
        !is.na(current_off_success),
        baseline_weight * off_success_rate + live_weight * current_off_success,
        off_success_rate
      ),
      
      def_success_rate_allowed = ifelse(
        !is.na(current_def_success),
        baseline_weight * def_success_rate_allowed + live_weight * current_def_success,
        def_success_rate_allowed
      ),
      
      pass_epa_per_play = ifelse(
        !is.na(current_pass_epa),
        baseline_weight * pass_epa_per_play + live_weight * current_pass_epa,
        pass_epa_per_play
      ),
      
      rush_epa_per_play = ifelse(
        !is.na(current_rush_epa),
        baseline_weight * rush_epa_per_play + live_weight * current_rush_epa,
        rush_epa_per_play
      ),
      
      pass_def_epa_per_play = ifelse(
        !is.na(current_pass_def_epa),
        baseline_weight * pass_def_epa_per_play + live_weight * current_pass_def_epa,
        pass_def_epa_per_play
      ),
      
      rush_def_epa_per_play = ifelse(
        !is.na(current_rush_def_epa),
        baseline_weight * rush_def_epa_per_play + live_weight * current_rush_def_epa,
        rush_def_epa_per_play
      ),
      
      # Add 2025-specific metrics
      current_form_2025 = ifelse(is.na(recent_form), 0, recent_form),
      live_data_quality = live_weight,
      last_updated = as.character(Sys.Date())
    ) %>%
    # Recalculate team strength with updated metrics
    mutate(
      team_strength = (off_epa_per_play * 10) + 
                     (def_epa_per_play * -8) +  # Defense: lower EPA is better
                     (current_form_2025 * 5)    # Recent form boost
    )
  
  # Clean up columns
  final_metrics <- updated_metrics %>%
    select(-current_off_epa, -current_def_epa, -current_off_success, -current_def_success,
           -current_pass_epa, -current_rush_epa, -current_pass_def_epa, -current_rush_def_epa,
           -recent_form, -sample_size, -baseline_weight)
  
  # Save updated metrics
  write_csv(final_metrics, "data/team_metrics_2025_updated.csv")
  
  # Backup old file and replace
  if (file.exists("data/team_metrics_2025.csv")) {
    file.copy("data/team_metrics_2025.csv", 
              paste0("data/team_metrics_2025_backup_", Sys.Date(), ".csv"))
  }
  file.copy("data/team_metrics_2025_updated.csv", "data/team_metrics_2025.csv", overwrite = TRUE)
  
  cat("✅ Team metrics updated with 2025 season data!\n")
  cat(sprintf("📊 Updated metrics for %d teams\n", nrow(final_metrics)))
  cat(sprintf("🔄 Teams with 3+ 2025 games: %d\n", 
              sum(final_metrics$games_played_2025 >= 3, na.rm = TRUE)))
  cat(sprintf("📈 Average live data weight: %.1f%%\n", 
              mean(final_metrics$live_data_quality * 100, na.rm = TRUE)))
  cat("💾 Saved to: data/team_metrics_2025.csv\n")
  
  return(final_metrics)
}

# Quick update function for regular use
quick_update_2025_data <- function() {
  
  cat("⚡ Quick 2025 season update...\n")
  
  updated_metrics <- update_team_metrics_with_2025()
  
  cat("✅ Update complete! Your predictions now use current 2025 data.\n")
  
  return(invisible(updated_metrics))
}

# Run if executed directly
if (!interactive()) {
  updated_metrics <- update_team_metrics_with_2025()
}
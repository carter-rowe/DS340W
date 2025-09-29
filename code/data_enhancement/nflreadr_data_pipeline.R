library(nflreadr)
library(dplyr)
library(tidyr)
library(readr)

# Function to get comprehensive team metrics from nflreadr
get_team_metrics <- function(seasons = 2002:2023) {
  
  # Load play-by-play data
  cat("Loading play-by-play data...\n")
  pbp <- load_pbp(seasons)
  
  # Load schedules for game info
  cat("Loading schedules...\n")
  schedules <- load_schedules(seasons)
  
  # Calculate offensive EPA metrics per team per game
  offensive_metrics <- pbp %>%
    filter(!is.na(epa), !is.na(posteam)) %>%
    group_by(game_id, posteam) %>%
    summarise(
      # Overall EPA
      off_epa_total = sum(epa, na.rm = TRUE),
      off_epa_avg = mean(epa, na.rm = TRUE),
      off_success_rate = mean(success, na.rm = TRUE),
      
      # Passing EPA
      off_pass_epa_total = sum(epa[pass == 1], na.rm = TRUE),
      off_pass_epa_avg = mean(epa[pass == 1], na.rm = TRUE),
      off_pass_success_rate = mean(success[pass == 1], na.rm = TRUE),
      
      # Rushing EPA
      off_rush_epa_total = sum(epa[rush == 1], na.rm = TRUE),
      off_rush_epa_avg = mean(epa[rush == 1], na.rm = TRUE),
      off_rush_success_rate = mean(success[rush == 1], na.rm = TRUE),
      
      # Play counts
      total_plays = n(),
      pass_plays = sum(pass == 1, na.rm = TRUE),
      rush_plays = sum(rush == 1, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Calculate defensive EPA metrics per team per game
  defensive_metrics <- pbp %>%
    filter(!is.na(epa), !is.na(defteam)) %>%
    group_by(game_id, defteam) %>%
    summarise(
      # Overall defensive EPA allowed
      def_epa_allowed_total = sum(epa, na.rm = TRUE),
      def_epa_allowed_avg = mean(epa, na.rm = TRUE),
      def_success_rate_allowed = mean(success, na.rm = TRUE),
      
      # Passing defense
      def_pass_epa_allowed_total = sum(epa[pass == 1], na.rm = TRUE),
      def_pass_epa_allowed_avg = mean(epa[pass == 1], na.rm = TRUE),
      def_pass_success_rate_allowed = mean(success[pass == 1], na.rm = TRUE),
      
      # Rushing defense
      def_rush_epa_allowed_total = sum(epa[rush == 1], na.rm = TRUE),
      def_rush_epa_allowed_avg = mean(epa[rush == 1], na.rm = TRUE),
      def_rush_success_rate_allowed = mean(success[rush == 1], na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Calculate special teams EPA
  special_teams_metrics <- pbp %>%
    filter(!is.na(epa), special == 1) %>%
    group_by(game_id, posteam) %>%
    summarise(
      st_epa_total = sum(epa, na.rm = TRUE),
      st_epa_avg = mean(epa, na.rm = TRUE),
      st_plays = n(),
      .groups = "drop"
    ) %>%
    # Add defensive special teams
    bind_rows(
      pbp %>%
        filter(!is.na(epa), special == 1) %>%
        group_by(game_id, defteam) %>%
        summarise(
          st_epa_allowed_total = sum(epa, na.rm = TRUE),
          st_epa_allowed_avg = mean(epa, na.rm = TRUE),
          st_plays_allowed = n(),
          .groups = "drop"
        ) %>%
        rename(posteam = defteam)
    )
  
  # Combine all metrics
  game_metrics <- schedules %>%
    select(game_id, season, week, home_team, away_team, home_score, away_score) %>%
    # Add home team metrics
    left_join(
      offensive_metrics %>% rename_with(~paste0("home_", .), -c(game_id, posteam)),
      by = c("game_id", "home_team" = "posteam")
    ) %>%
    left_join(
      defensive_metrics %>% rename_with(~paste0("home_", .), -c(game_id, defteam)),
      by = c("game_id", "home_team" = "defteam")
    ) %>%
    # Add away team metrics
    left_join(
      offensive_metrics %>% rename_with(~paste0("away_", .), -c(game_id, posteam)),
      by = c("game_id", "away_team" = "posteam")
    ) %>%
    left_join(
      defensive_metrics %>% rename_with(~paste0("away_", .), -c(game_id, defteam)),
      by = c("game_id", "away_team" = "defteam")
    )
  
  return(game_metrics)
}

# Function to calculate team directionality (momentum/trends)
calculate_directionality <- function(team_metrics, window_size = 4) {
  
  team_metrics %>%
    arrange(season, week) %>%
    group_by(home_team) %>%
    mutate(
      # Home team rolling averages
      home_epa_trend = zoo::rollmean(home_off_epa_avg, window_size, fill = NA, align = "right"),
      home_def_trend = zoo::rollmean(home_def_epa_allowed_avg, window_size, fill = NA, align = "right")
    ) %>%
    group_by(away_team) %>%
    mutate(
      # Away team rolling averages  
      away_epa_trend = zoo::rollmean(away_off_epa_avg, window_size, fill = NA, align = "right"),
      away_def_trend = zoo::rollmean(away_def_epa_allowed_avg, window_size, fill = NA, align = "right")
    ) %>%
    ungroup()
}

# Main function to create enhanced dataset
create_enhanced_dataset <- function(seasons = 2002:2023) {
  
  cat("Creating enhanced NFL dataset with EPA metrics...\n")
  
  # Get team metrics
  team_metrics <- get_team_metrics(seasons)
  
  # Calculate directionality
  enhanced_data <- calculate_directionality(team_metrics)
  
  # Read existing rest data to merge
  existing_data <- read_csv("data/games_clean.csv")
  
  # Merge with existing rest analysis (fix game_id type mismatch)
  final_dataset <- enhanced_data %>%
    mutate(game_id = as.numeric(game_id)) %>%
    left_join(existing_data, by = c("game_id"))
  
  # Save enhanced dataset
  write_csv(final_dataset, "data/games_enhanced.csv")
  
  cat("Enhanced dataset saved to data/games_enhanced.csv\n")
  
  return(final_dataset)
}

# Run the pipeline
if (!interactive()) {
  enhanced_data <- create_enhanced_dataset()
}
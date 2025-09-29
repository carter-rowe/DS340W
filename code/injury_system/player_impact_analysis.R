library(nflreadr)
library(dplyr)
library(readr)
library(tidyr)

# Player Impact Analysis for Injury System
# Calculates individual player contributions to team EPA

# Function to calculate quarterback impact
calculate_qb_impact <- function(seasons = 2022:2024) {
  
  cat("Calculating QB impact from", min(seasons), "to", max(seasons), "...\n")
  
  qb_impact <- data.frame()
  
  for (season in seasons) {
    cat("Processing", season, "season...\n")
    
    # Load play-by-play data
    pbp <- load_pbp(season)
    
    # Calculate QB stats
    season_qb <- pbp %>%
      filter(!is.na(passer_player_name), !is.na(epa), season_type == "REG") %>%
      group_by(passer_player_name, posteam, season) %>%
      summarise(
        # Basic EPA metrics
        pass_epa_per_play = mean(epa, na.rm = TRUE),
        total_pass_epa = sum(epa, na.rm = TRUE),
        pass_attempts = n(),
        success_rate = mean(success, na.rm = TRUE),
        
        # Situational EPA
        third_down_epa = mean(epa[down == 3], na.rm = TRUE),
        red_zone_epa = mean(epa[yardline_100 <= 20], na.rm = TRUE),
        fourth_quarter_epa = mean(epa[qtr == 4], na.rm = TRUE),
        
        # Advanced metrics
        air_yards_per_att = mean(air_yards, na.rm = TRUE),
        yards_after_catch_per_att = mean(yards_after_catch, na.rm = TRUE),
        
        # Game context
        games_played = length(unique(game_id)),
        
        .groups = "drop"
      ) %>%
      filter(pass_attempts >= 100) %>%  # Minimum threshold for relevance
      mutate(
        position = "QB",
        season = season,
        player_name = passer_player_name,
        team = posteam
      )
    
    qb_impact <- bind_rows(qb_impact, season_qb)
  }
  
  return(qb_impact)
}

# Function to calculate running back impact
calculate_rb_impact <- function(seasons = 2022:2024) {
  
  cat("Calculating RB impact from", min(seasons), "to", max(seasons), "...\n")
  
  rb_impact <- data.frame()
  
  for (season in seasons) {
    
    pbp <- load_pbp(season)
    
    # Calculate RB stats
    season_rb <- pbp %>%
      filter(!is.na(rusher_player_name), !is.na(epa), season_type == "REG") %>%
      group_by(rusher_player_name, posteam, season) %>%
      summarise(
        # Basic EPA metrics
        rush_epa_per_play = mean(epa, na.rm = TRUE),
        total_rush_epa = sum(epa, na.rm = TRUE),
        carries = n(),
        success_rate = mean(success, na.rm = TRUE),
        
        # Situational
        goal_line_epa = mean(epa[yardline_100 <= 5], na.rm = TRUE),
        short_yardage_epa = mean(epa[ydstogo <= 2], na.rm = TRUE),
        
        # Context
        avg_yards_per_carry = mean(rushing_yards, na.rm = TRUE),
        games_played = length(unique(game_id)),
        
        .groups = "drop"
      ) %>%
      filter(carries >= 50) %>%  # Minimum carries for relevance
      mutate(
        position = "RB",
        season = season,
        player_name = rusher_player_name,
        team = posteam
      )
    
    rb_impact <- bind_rows(rb_impact, season_rb)
  }
  
  return(rb_impact)
}

# Function to calculate receiver impact (WR/TE)
calculate_receiver_impact <- function(seasons = 2022:2024) {
  
  cat("Calculating WR/TE impact from", min(seasons), "to", max(seasons), "...\n")
  
  rec_impact <- data.frame()
  
  for (season in seasons) {
    
    pbp <- load_pbp(season)
    
    # Load rosters to get positions
    rosters <- load_rosters(season)
    
    # Calculate receiver stats
    season_rec <- pbp %>%
      filter(!is.na(receiver_player_name), !is.na(epa), season_type == "REG") %>%
      group_by(receiver_player_name, posteam, season) %>%
      summarise(
        # Basic EPA metrics
        rec_epa_per_play = mean(epa, na.rm = TRUE),
        total_rec_epa = sum(epa, na.rm = TRUE),
        targets = n(),
        receptions = sum(complete_pass == 1, na.rm = TRUE),
        
        # Efficiency
        catch_rate = mean(complete_pass, na.rm = TRUE),
        success_rate = mean(success, na.rm = TRUE),
        
        # Situational
        red_zone_epa = mean(epa[yardline_100 <= 20], na.rm = TRUE),
        third_down_epa = mean(epa[down == 3], na.rm = TRUE),
        
        # Yards
        avg_air_yards = mean(air_yards, na.rm = TRUE),
        avg_yac = mean(yards_after_catch, na.rm = TRUE),
        
        games_played = length(unique(game_id)),
        
        .groups = "drop"
      ) %>%
      filter(targets >= 30) %>%  # Minimum targets for relevance
      left_join(
        rosters %>% select(full_name, position) %>% distinct(),
        by = c("receiver_player_name" = "full_name")
      ) %>%
      mutate(
        position = case_when(
          position %in% c("WR") ~ "WR",
          position %in% c("TE") ~ "TE",
          TRUE ~ "WR"  # Default assumption
        ),
        season = season,
        player_name = receiver_player_name,
        team = posteam
      )
    
    rec_impact <- bind_rows(rec_impact, season_rec)
  }
  
  return(rec_impact)
}

# Function to calculate team EPA baselines
calculate_team_epa_baselines <- function(seasons = 2022:2024) {
  
  cat("Calculating team EPA baselines...\n")
  
  team_baselines <- data.frame()
  
  for (season in seasons) {
    
    pbp <- load_pbp(season)
    
    # Calculate team total EPA
    season_teams <- pbp %>%
      filter(!is.na(epa), season_type == "REG") %>%
      group_by(posteam, season) %>%
      summarise(
        total_team_epa = sum(epa, na.rm = TRUE),
        total_plays = n(),
        team_epa_per_play = mean(epa, na.rm = TRUE),
        
        # Split by play type
        pass_plays = sum(pass == 1, na.rm = TRUE),
        rush_plays = sum(rush == 1, na.rm = TRUE),
        pass_epa_total = sum(epa[pass == 1], na.rm = TRUE),
        rush_epa_total = sum(epa[rush == 1], na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      mutate(
        season = season,
        team = posteam
      )
    
    team_baselines <- bind_rows(team_baselines, season_teams)
  }
  
  return(team_baselines)
}

# Function to calculate player importance weights
calculate_player_weights <- function(player_impact, team_baselines) {
  
  cat("Calculating player importance weights...\n")
  
  # Join player impact with team baselines
  player_weights <- player_impact %>%
    left_join(team_baselines, by = c("team", "season")) %>%
    mutate(
      # Calculate what percentage of team EPA this player contributes
      team_epa_contribution = case_when(
        position == "QB" ~ total_epa / pass_epa_total,
        position == "RB" ~ total_epa / rush_epa_total,
        position %in% c("WR", "TE") ~ total_epa / pass_epa_total,
        TRUE ~ 0
      ),
      
      # Overall team impact (more conservative)
      overall_team_impact = case_when(
        position == "QB" ~ team_epa_contribution * 0.6,  # QBs get 60% of pass EPA credit
        position == "RB" ~ team_epa_contribution * 0.7,  # RBs get 70% of rush EPA credit  
        position %in% c("WR", "TE") ~ team_epa_contribution * 0.8,  # Receivers get 80% credit
        TRUE ~ 0
      ),
      
      # Cap contributions at reasonable levels
      team_epa_contribution = pmin(team_epa_contribution, 0.8),
      overall_team_impact = pmin(overall_team_impact, 0.5)
    ) %>%
    select(player_name, position, team, season, 
           ends_with("_epa_per_play"), ends_with("_epa_total"),
           team_epa_contribution, overall_team_impact,
           games_played, success_rate)
  
  return(player_weights)
}

# Main function to create comprehensive player impact database
create_player_impact_database <- function(seasons = 2022:2024) {
  
  cat("🏈 Creating Player Impact Database 🏈\n")
  cat("===================================\n")
  
  # Calculate individual position impacts
  qb_impact <- calculate_qb_impact(seasons)
  rb_impact <- calculate_rb_impact(seasons)
  rec_impact <- calculate_receiver_impact(seasons)
  
  # Get team baselines
  team_baselines <- calculate_team_epa_baselines(seasons)
  
  # Combine all player data
  cat("Combining player data...\n")
  
  # Standardize column names for combination
  qb_standard <- qb_impact %>%
    select(player_name, position, team, season, 
           epa_per_play = pass_epa_per_play,
           total_epa = total_pass_epa,
           plays = pass_attempts,
           success_rate, games_played,
           third_down_epa, red_zone_epa)
  
  rb_standard <- rb_impact %>%
    select(player_name, position, team, season,
           epa_per_play = rush_epa_per_play,
           total_epa = total_rush_epa,
           plays = carries,
           success_rate, games_played,
           goal_line_epa, short_yardage_epa) %>%
    mutate(third_down_epa = NA, red_zone_epa = goal_line_epa)
  
  rec_standard <- rec_impact %>%
    select(player_name, position, team, season,
           epa_per_play = rec_epa_per_play,
           total_epa = total_rec_epa,
           plays = targets,
           success_rate, games_played,
           third_down_epa, red_zone_epa)
  
  # Combine all players
  all_players <- bind_rows(qb_standard, rb_standard, rec_standard)
  
  # Calculate weights
  player_weights <- calculate_player_weights(all_players, team_baselines)
  
  # Save the database
  write_csv(player_weights, "data/player_impact_database.csv")
  
  cat("✅ Player impact database created!\n")
  cat(sprintf("📊 Analyzed %d players across %d seasons\n", 
              length(unique(player_weights$player_name)), length(seasons)))
  cat("💾 Saved to: data/player_impact_database.csv\n")
  
  return(player_weights)
}

# Example usage for specific players
analyze_specific_player <- function(player_name, player_db) {
  
  player_data <- player_db %>%
    filter(player_name == !!player_name) %>%
    arrange(desc(season))
  
  if (nrow(player_data) == 0) {
    cat("Player not found in database\n")
    return(NULL)
  }
  
  cat(sprintf("\n🏈 %s (%s) Analysis 🏈\n", player_name, player_data$position[1]))
  cat("================================\n")
  
  for (i in 1:nrow(player_data)) {
    season_data <- player_data[i, ]
    cat(sprintf("%d Season (%s):\n", season_data$season, season_data$team))
    cat(sprintf("  EPA per play: %+.3f\n", season_data$epa_per_play))
    cat(sprintf("  Team EPA contribution: %.1f%%\n", season_data$team_epa_contribution * 100))
    cat(sprintf("  Overall team impact: %.1f%%\n", season_data$overall_team_impact * 100))
    cat(sprintf("  Games played: %d\n", season_data$games_played))
    cat("\n")
  }
  
  return(player_data)
}

# Run if executed directly
if (!interactive()) {
  player_impact_db <- create_player_impact_database(seasons = 2022:2024)
}
library(nflreadr)
library(dplyr)
library(readr)
library(stringr)

# Injury Integration System for NFL Predictions
# Integrates real-time injury data with player impact analysis

# Function to load and process current injury reports
get_current_injuries <- function(weeks = 1:18, season = 2024) {
  
  cat("Loading current injury reports...\n")
  
  # Load injury data from nflreadr
  injuries_raw <- load_injuries(seasons = season)
  
  # Process and standardize injury data
  injuries_processed <- injuries_raw %>%
    mutate(
      # Standardize team names
      team = case_when(
        team == "LA" ~ "LAR",  # Rams
        TRUE ~ team
      ),
      
      # Calculate participation probability based on status
      participation_probability = case_when(
        str_detect(tolower(report_status), "out") ~ 0.0,
        str_detect(tolower(report_status), "doubtful") ~ 0.25,
        str_detect(tolower(report_status), "questionable") ~ 0.50,
        str_detect(tolower(report_status), "probable") ~ 0.85,
        str_detect(tolower(report_status), "ir|injured reserve") ~ 0.0,
        TRUE ~ 1.0
      ),
      
      # Estimate effectiveness when playing (if not 100% healthy)
      effectiveness_when_playing = case_when(
        str_detect(tolower(report_status), "questionable") ~ 0.75,
        str_detect(tolower(report_status), "doubtful") ~ 0.60,
        str_detect(tolower(report_status), "probable") ~ 0.90,
        TRUE ~ 1.0
      ),
      
      # Classify injury severity based on body part and type
      injury_severity = case_when(
        str_detect(tolower(report_primary), "concussion|head|brain") ~ "High",
        str_detect(tolower(report_primary), "knee|acl|mcl|meniscus") ~ "High",
        str_detect(tolower(report_primary), "ankle|foot|achilles") ~ "Medium",
        str_detect(tolower(report_primary), "shoulder|collarbone|clavicle") ~ "Medium",
        str_detect(tolower(report_primary), "back|spine") ~ "Medium",
        str_detect(tolower(report_primary), "hamstring|quad|groin") ~ "Medium",
        str_detect(tolower(report_primary), "wrist|hand|finger") ~ "Low",
        str_detect(tolower(report_primary), "hip|ribs") ~ "Low",
        TRUE ~ "Low"
      ),
      
      # Position-specific impact multipliers
      position_vulnerability = case_when(
        position == "QB" & str_detect(tolower(report_primary), "shoulder|arm|hand|wrist") ~ 1.5,
        position == "RB" & str_detect(tolower(report_primary), "knee|ankle|hamstring") ~ 1.3,
        position %in% c("WR", "TE") & str_detect(tolower(report_primary), "hamstring|ankle") ~ 1.2,
        position %in% c("OT", "OG", "C") & str_detect(tolower(report_primary), "ankle|knee") ~ 1.4,
        TRUE ~ 1.0
      )
    ) %>%
    filter(!is.na(full_name), !is.na(team)) %>%
    select(full_name, team, position, week, season,
           report_status, report_primary, report_secondary,
           participation_probability, effectiveness_when_playing,
           injury_severity, position_vulnerability)
  
  cat(sprintf("✅ Processed %d injury reports\n", nrow(injuries_processed)))
  
  return(injuries_processed)
}

# Function to calculate team injury impact
calculate_team_injury_impact <- function(team_code, injuries, player_impact_db) {
  
  # Get injuries for this team
  team_injuries <- injuries %>% 
    filter(team == team_code, participation_probability < 1.0)
  
  if (nrow(team_injuries) == 0) {
    return(list(
      total_epa_impact = 0,
      position_impacts = list(),
      injured_players = list(),
      severity_score = 0
    ))
  }
  
  cat(sprintf("Analyzing %d injuries for %s...\n", nrow(team_injuries), team_code))
  
  total_impact <- 0
  position_impacts <- list()
  injured_players <- list()
  
  for (i in 1:nrow(team_injuries)) {
    injury <- team_injuries[i, ]
    player_name <- injury$full_name
    
    # Find player in impact database (most recent season)
    player_impact <- player_impact_db %>%
      filter(player_name == !!player_name) %>%
      arrange(desc(season)) %>%
      slice_head(n = 1)
    
    if (nrow(player_impact) == 0) {
      # If player not in database, estimate based on position
      player_impact <- estimate_position_impact(injury$position, team_code, player_impact_db)
    }
    
    if (nrow(player_impact) == 0) next  # Skip if we can't estimate impact
    
    # Calculate expected impact loss
    participation_loss <- 1 - injury$participation_probability
    effectiveness_loss <- injury$participation_probability * (1 - injury$effectiveness_when_playing)
    total_availability_loss <- participation_loss + effectiveness_loss
    
    # Position-specific impact calculation
    base_impact <- player_impact$overall_team_impact * player_impact$team_epa_per_play
    
    # Apply injury severity and position vulnerability multipliers
    severity_multiplier <- case_when(
      injury$injury_severity == "High" ~ 1.3,
      injury$injury_severity == "Medium" ~ 1.1,
      TRUE ~ 1.0
    )
    
    # Calculate final impact
    player_epa_impact <- base_impact * total_availability_loss * 
                        severity_multiplier * injury$position_vulnerability
    
    total_impact <- total_impact + player_epa_impact
    
    # Store position-specific impacts
    if (!(injury$position %in% names(position_impacts))) {
      position_impacts[[injury$position]] <- 0
    }
    position_impacts[[injury$position]] <- position_impacts[[injury$position]] + player_epa_impact
    
    # Store player details
    injured_players[[player_name]] <- list(
      position = injury$position,
      status = injury$report_status,
      epa_impact = player_epa_impact,
      participation_prob = injury$participation_probability,
      effectiveness = injury$effectiveness_when_playing
    )
  }
  
  # Calculate overall severity score
  severity_score <- sum(sapply(injured_players, function(p) {
    case_when(
      p$position == "QB" ~ p$epa_impact * 3,  # QBs weighted heavily
      p$position %in% c("WR", "RB", "TE") ~ p$epa_impact * 2,
      TRUE ~ p$epa_impact
    )
  }))
  
  return(list(
    total_epa_impact = total_impact,
    position_impacts = position_impacts,
    injured_players = injured_players,
    severity_score = severity_score
  ))
}

# Function to estimate impact for players not in database
estimate_position_impact <- function(position, team_code, player_impact_db) {
  
  # Get average impact for this position from similar players
  position_avg <- player_impact_db %>%
    filter(position == !!position, season >= 2023) %>%  # Recent seasons only
    summarise(
      avg_epa_per_play = mean(team_epa_per_play, na.rm = TRUE),
      avg_team_impact = mean(overall_team_impact, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(position_avg) == 0) {
    # Default estimates if no data
    default_impacts <- list(
      "QB" = list(team_epa_per_play = 0.05, overall_team_impact = 0.35),
      "RB" = list(team_epa_per_play = 0.02, overall_team_impact = 0.15),
      "WR" = list(team_epa_per_play = 0.08, overall_team_impact = 0.12),
      "TE" = list(team_epa_per_play = 0.06, overall_team_impact = 0.08),
      "OT" = list(team_epa_per_play = 0.01, overall_team_impact = 0.10),
      "OG" = list(team_epa_per_play = 0.01, overall_team_impact = 0.08),
      "C" = list(team_epa_per_play = 0.01, overall_team_impact = 0.08)
    )
    
    if (position %in% names(default_impacts)) {
      return(data.frame(
        player_name = "Unknown",
        position = position,
        team_epa_per_play = default_impacts[[position]]$team_epa_per_play,
        overall_team_impact = default_impacts[[position]]$overall_team_impact
      ))
    } else {
      return(data.frame())
    }
  }
  
  return(data.frame(
    player_name = "Estimated",
    position = position,
    team_epa_per_play = position_avg$avg_epa_per_play,
    overall_team_impact = position_avg$avg_team_impact
  ))
}

# Enhanced prediction function with injury adjustments
predict_game_with_injury_impact <- function(home_team, away_team, base_team_metrics, 
                                           current_injuries, player_impact_db) {
  
  cat(sprintf("🏥 Analyzing injuries for %s vs %s...\n", home_team, away_team))
  
  # Calculate injury impacts for both teams
  home_injury_impact <- calculate_team_injury_impact(home_team, current_injuries, player_impact_db)
  away_injury_impact <- calculate_team_injury_impact(away_team, current_injuries, player_impact_db)
  
  # Load the base prediction function
  source("code/prediction_engine/nfl_2025_predictor.R")
  
  # Get base team metrics
  home_metrics <- base_team_metrics %>% filter(team == home_team)
  away_metrics <- base_team_metrics %>% filter(team == away_team)
  
  # Adjust team metrics based on injuries
  home_metrics_adjusted <- home_metrics %>%
    mutate(
      off_epa_per_play = off_epa_per_play + home_injury_impact$total_epa_impact,
      # Defensive injuries would affect def_epa_per_play, but focusing on offense for now
      team_strength = team_strength + (home_injury_impact$total_epa_impact * 10)  # Scale for team strength
    )
  
  away_metrics_adjusted <- away_metrics %>%
    mutate(
      off_epa_per_play = off_epa_per_play + away_injury_impact$total_epa_impact,
      team_strength = team_strength + (away_injury_impact$total_epa_impact * 10)
    )
  
  # Make base prediction with adjusted metrics
  base_prediction <- predict_2025_game(home_team, away_team, 
                                      rbind(home_metrics_adjusted, away_metrics_adjusted))
  
  # Calculate injury-adjusted impacts
  injury_point_impact <- (home_injury_impact$total_epa_impact - away_injury_impact$total_epa_impact) * 25
  
  # Adjust prediction
  adjusted_prediction <- base_prediction
  adjusted_prediction$predicted_margin <- base_prediction$predicted_margin + injury_point_impact
  adjusted_prediction$home_score <- base_prediction$home_score + (injury_point_impact / 2)
  adjusted_prediction$away_score <- base_prediction$away_score - (injury_point_impact / 2)
  
  # Recalculate win probabilities
  game_variance <- 14.0
  adjusted_prediction$home_win_prob <- pnorm(adjusted_prediction$predicted_margin / game_variance)
  adjusted_prediction$away_win_prob <- 1 - adjusted_prediction$home_win_prob
  adjusted_prediction$predicted_winner <- ifelse(adjusted_prediction$predicted_margin > 0, home_team, away_team)
  
  # Add injury context
  adjusted_prediction$injury_analysis <- list(
    home_team_injuries = home_injury_impact,
    away_team_injuries = away_injury_impact,
    net_injury_advantage = injury_point_impact,
    injury_adjusted = TRUE
  )
  
  cat(sprintf("🔍 Injury impact: %+.1f points toward %s\n", 
              abs(injury_point_impact), 
              ifelse(injury_point_impact > 0, home_team, away_team)))
  
  return(adjusted_prediction)
}

# Function to create injury report summary
create_injury_summary <- function(team, injury_impact) {
  
  if (length(injury_impact$injured_players) == 0) {
    return(sprintf("✅ %s: No significant injuries reported", team))
  }
  
  summary_lines <- c(sprintf("🏥 %s Injury Report:", team))
  
  for (player_name in names(injury_impact$injured_players)) {
    player <- injury_impact$injured_players[[player_name]]
    impact_description <- case_when(
      abs(player$epa_impact) > 0.02 ~ "Major impact",
      abs(player$epa_impact) > 0.01 ~ "Moderate impact", 
      TRUE ~ "Minor impact"
    )
    
    summary_lines <- c(summary_lines,
      sprintf("  • %s (%s): %s - %s (%.0f%% to play)", 
              player_name, player$position, player$status, 
              impact_description, player$participation_prob * 100))
  }
  
  summary_lines <- c(summary_lines,
    sprintf("  📊 Total EPA Impact: %+.3f", injury_impact$total_epa_impact))
  
  return(paste(summary_lines, collapse = "\n"))
}

# Example: Joe Burrow injury analysis
analyze_burrow_injury_example <- function() {
  
  cat("🏈 Joe Burrow Injury Impact Example 🏈\n")
  cat("====================================\n")
  
  # Simulated Burrow injury scenario
  burrow_injury <- data.frame(
    full_name = "Joe Burrow",
    team = "CIN", 
    position = "QB",
    week = 1,
    season = 2024,
    report_status = "Questionable",
    report_primary = "Wrist",
    participation_probability = 0.50,
    effectiveness_when_playing = 0.75,
    injury_severity = "Medium",
    position_vulnerability = 1.5  # QB wrist injury
  )
  
  # Load player impact database
  if (file.exists("data/player_impact_database.csv")) {
    player_db <- read_csv("data/player_impact_database.csv", show_col_types = FALSE)
  } else {
    cat("⚠️ Player impact database not found. Run player_impact_analysis.R first.\n")
    return(NULL)
  }
  
  # Calculate Burrow's impact
  cin_injury_impact <- calculate_team_injury_impact("CIN", burrow_injury, player_db)
  
  cat("📊 Analysis Results:\n")
  cat(sprintf("Burrow's typical EPA contribution: High (QB position)\n"))
  cat(sprintf("Injury status: %s (%.0f%% to play)\n", 
              burrow_injury$report_status, burrow_injury$participation_probability * 100))
  cat(sprintf("Expected effectiveness if playing: %.0f%%\n", 
              burrow_injury$effectiveness_when_playing * 100))
  cat(sprintf("Estimated team EPA impact: %+.3f\n", cin_injury_impact$total_epa_impact))
  cat(sprintf("Point spread impact: ~%+.1f points\n", cin_injury_impact$total_epa_impact * 25))
  
  return(cin_injury_impact)
}

# Run example if executed directly
if (!interactive()) {
  # Run Burrow example
  burrow_example <- analyze_burrow_injury_example()
}
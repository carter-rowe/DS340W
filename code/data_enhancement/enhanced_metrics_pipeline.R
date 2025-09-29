library(nflreadr)
library(dplyr)
library(tidyr)
library(readr)

# Enhanced metrics pipeline with CPOE and advanced EPA metrics
# Phase 1 improvements for NFL prediction model

# Main function to calculate enhanced team metrics
calculate_enhanced_team_metrics <- function(pbp, schedules, season, weight = 1.0) {

  # Get all teams for this season
  teams <- unique(c(schedules$home_team, schedules$away_team))

  team_metrics <- list()

  for (team in teams) {
    cat(sprintf("  Processing enhanced metrics for %s...\n", team))

    # Enhanced offensive metrics with CPOE and advanced EPA
    off_data <- pbp %>%
      filter(posteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        games_played = length(unique(game_id)),

        # Core EPA metrics
        off_epa_per_play = mean(epa, na.rm = TRUE),
        off_epa_total = sum(epa, na.rm = TRUE),
        off_success_rate = mean(success, na.rm = TRUE),

        # Enhanced passing metrics with CPOE
        pass_epa_per_play = mean(epa[pass == 1], na.rm = TRUE),
        pass_epa_per_dropback = mean(epa[pass == 1 | sack == 1], na.rm = TRUE),
        pass_attempts = sum(pass == 1, na.rm = TRUE),
        pass_success_rate = mean(success[pass == 1], na.rm = TRUE),

        # CPOE - Completion Percentage Over Expectation
        cpoe = ifelse(sum(complete_pass == 1 | incomplete_pass == 1, na.rm = TRUE) > 0,
                     mean(complete_pass[pass == 1], na.rm = TRUE) - mean(cp[pass == 1], na.rm = TRUE),
                     0),

        # EPA + CPOE composite metric
        epa_cpoe_composite = ifelse(sum(pass == 1, na.rm = TRUE) > 0,
                                  mean(epa[pass == 1], na.rm = TRUE) +
                                  (mean(complete_pass[pass == 1], na.rm = TRUE) - mean(cp[pass == 1], na.rm = TRUE)) * 10,
                                  0),

        # Advanced passing depth metrics
        avg_depth_of_target = mean(air_yards[pass == 1], na.rm = TRUE),
        deep_pass_rate = mean(air_yards[pass == 1] >= 20, na.rm = TRUE),
        short_pass_efficiency = mean(epa[pass == 1 & air_yards <= 10], na.rm = TRUE),
        deep_pass_efficiency = mean(epa[pass == 1 & air_yards >= 20], na.rm = TRUE),

        # Time-based metrics
        avg_time_to_throw = mean(time_to_throw[pass == 1], na.rm = TRUE),
        quick_pass_rate = mean(time_to_throw[pass == 1] <= 2.5, na.rm = TRUE),

        # Enhanced rushing metrics
        rush_epa_per_play = mean(epa[rush == 1], na.rm = TRUE),
        rush_attempts = sum(rush == 1, na.rm = TRUE),
        rush_success_rate = mean(success[rush == 1], na.rm = TRUE),
        explosive_rush_rate = mean(rushing_yards[rush == 1] >= 10, na.rm = TRUE),

        # Situational efficiency - Red zone (enhanced)
        rz_plays = sum(yardline_100 <= 20, na.rm = TRUE),
        rz_tds = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE),
        rz_efficiency = ifelse(rz_plays > 0, rz_tds / rz_plays, 0),
        rz_epa_per_play = mean(epa[yardline_100 <= 20], na.rm = TRUE),

        # Third down efficiency (enhanced)
        third_down_plays = sum(down == 3, na.rm = TRUE),
        third_down_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        third_down_rate = ifelse(third_down_plays > 0, third_down_conversions / third_down_plays, 0),
        third_down_epa = mean(epa[down == 3], na.rm = TRUE),

        # Late-game performance (clutch factor)
        late_game_plays = sum(quarter >= 4 & score_differential <= 7, na.rm = TRUE),
        late_game_epa = mean(epa[quarter >= 4 & score_differential <= 7], na.rm = TRUE),

        # Two-minute drill efficiency
        two_min_plays = sum(two_minute_warning == 1, na.rm = TRUE),
        two_min_epa = mean(epa[two_minute_warning == 1], na.rm = TRUE),

        # Goal-to-go efficiency
        goal_to_go_plays = sum(goal_to_go == 1, na.rm = TRUE),
        goal_to_go_td_rate = ifelse(goal_to_go_plays > 0,
                                   sum(touchdown == 1 & goal_to_go == 1, na.rm = TRUE) / goal_to_go_plays,
                                   0),

        .groups = "drop"
      )

    # Enhanced defensive metrics
    def_data <- pbp %>%
      filter(defteam == team, !is.na(epa), !is.na(down)) %>%
      summarise(
        # Core defensive EPA (lower is better)
        def_epa_per_play = mean(epa, na.rm = TRUE),
        def_epa_total = sum(epa, na.rm = TRUE),
        def_success_rate_allowed = mean(success, na.rm = TRUE),

        # Enhanced pass defense
        pass_def_epa_per_play = mean(epa[pass == 1], na.rm = TRUE),
        pass_def_epa_per_dropback = mean(epa[pass == 1 | sack == 1], na.rm = TRUE),
        pass_def_success_rate = mean(success[pass == 1], na.rm = TRUE),

        # Defensive CPOE (opponent CPOE allowed)
        def_cpoe_allowed = ifelse(sum(complete_pass == 1 | incomplete_pass == 1, na.rm = TRUE) > 0,
                                 mean(complete_pass[pass == 1], na.rm = TRUE) - mean(cp[pass == 1], na.rm = TRUE),
                                 0),

        # Pressure metrics
        sack_rate = ifelse(sum(pass == 1 | sack == 1, na.rm = TRUE) > 0,
                          sum(sack == 1, na.rm = TRUE) / sum(pass == 1 | sack == 1, na.rm = TRUE),
                          0),
        qb_hit_rate = mean(qb_hit[pass == 1 | sack == 1], na.rm = TRUE),

        # Coverage metrics
        deep_pass_def_epa = mean(epa[pass == 1 & air_yards >= 20], na.rm = TRUE),
        short_pass_def_epa = mean(epa[pass == 1 & air_yards <= 10], na.rm = TRUE),

        # Enhanced rush defense
        rush_def_epa_per_play = mean(epa[rush == 1], na.rm = TRUE),
        rush_def_success_rate = mean(success[rush == 1], na.rm = TRUE),
        explosive_rush_allowed_rate = mean(rushing_yards[rush == 1] >= 10, na.rm = TRUE),

        # Situational defense - Red zone
        rz_def_plays = sum(yardline_100 <= 20, na.rm = TRUE),
        rz_def_tds = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE),
        rz_def_efficiency = ifelse(rz_def_plays > 0, rz_def_tds / rz_def_plays, 1),
        rz_def_epa_per_play = mean(epa[yardline_100 <= 20], na.rm = TRUE),

        # Third down defense
        third_def_plays = sum(down == 3, na.rm = TRUE),
        third_def_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        third_def_rate = ifelse(third_def_plays > 0, third_def_conversions / third_def_plays, 0.5),
        third_def_epa = mean(epa[down == 3], na.rm = TRUE),

        # Late-game defense
        late_game_def_plays = sum(quarter >= 4 & score_differential >= -7, na.rm = TRUE),
        late_game_def_epa = mean(epa[quarter >= 4 & score_differential >= -7], na.rm = TRUE),

        # Two-minute defense
        two_min_def_plays = sum(two_minute_warning == 1, na.rm = TRUE),
        two_min_def_epa = mean(epa[two_minute_warning == 1], na.rm = TRUE),

        .groups = "drop"
      )

    # Enhanced special teams metrics
    st_data <- pbp %>%
      filter((posteam == team | defteam == team), special == 1, !is.na(epa)) %>%
      group_by(team_involved = ifelse(posteam == team, "offense", "defense")) %>%
      summarise(
        st_epa_per_play = mean(epa, na.rm = TRUE),
        st_plays = n(),

        # Field goal metrics
        fg_epa = mean(epa[play_type == "field_goal"], na.rm = TRUE),
        fg_attempts = sum(play_type == "field_goal", na.rm = TRUE),

        # Punt metrics
        punt_epa = mean(epa[play_type == "punt"], na.rm = TRUE),
        punt_attempts = sum(play_type == "punt", na.rm = TRUE),

        # Kickoff metrics
        kickoff_epa = mean(epa[play_type == "kickoff"], na.rm = TRUE),
        kickoff_attempts = sum(play_type == "kickoff", na.rm = TRUE),

        .groups = "drop"
      ) %>%
      pivot_wider(names_from = team_involved, values_from = everything(),
                  names_sep = "_", values_fill = 0)

    # Calculate enhanced team strength with new metrics
    league_avg_off_epa <- pbp %>%
      filter(!is.na(epa), !is.na(down)) %>%
      summarise(avg_epa = mean(epa, na.rm = TRUE)) %>%
      pull(avg_epa)

    # Combine all enhanced metrics
    team_metrics[[team]] <- list(
      team = team,
      season = season,
      weight = weight,
      games_played = off_data$games_played,

      # Core offensive metrics
      off_epa_per_play = ifelse(is.na(off_data$off_epa_per_play), 0, off_data$off_epa_per_play),
      off_success_rate = ifelse(is.na(off_data$off_success_rate), 0.45, off_data$off_success_rate),

      # Enhanced passing metrics
      pass_epa_per_play = ifelse(is.na(off_data$pass_epa_per_play), 0, off_data$pass_epa_per_play),
      pass_epa_per_dropback = ifelse(is.na(off_data$pass_epa_per_dropback), 0, off_data$pass_epa_per_dropback),
      cpoe = ifelse(is.na(off_data$cpoe), 0, off_data$cpoe),
      epa_cpoe_composite = ifelse(is.na(off_data$epa_cpoe_composite), 0, off_data$epa_cpoe_composite),
      avg_depth_of_target = ifelse(is.na(off_data$avg_depth_of_target), 7.0, off_data$avg_depth_of_target),
      deep_pass_rate = ifelse(is.na(off_data$deep_pass_rate), 0.15, off_data$deep_pass_rate),
      short_pass_efficiency = ifelse(is.na(off_data$short_pass_efficiency), 0, off_data$short_pass_efficiency),
      deep_pass_efficiency = ifelse(is.na(off_data$deep_pass_efficiency), 0, off_data$deep_pass_efficiency),
      avg_time_to_throw = ifelse(is.na(off_data$avg_time_to_throw), 2.7, off_data$avg_time_to_throw),
      quick_pass_rate = ifelse(is.na(off_data$quick_pass_rate), 0.6, off_data$quick_pass_rate),

      # Enhanced rushing metrics
      rush_epa_per_play = ifelse(is.na(off_data$rush_epa_per_play), 0, off_data$rush_epa_per_play),
      rush_success_rate = ifelse(is.na(off_data$rush_success_rate), 0.45, off_data$rush_success_rate),
      explosive_rush_rate = ifelse(is.na(off_data$explosive_rush_rate), 0.1, off_data$explosive_rush_rate),

      # Enhanced situational metrics
      rz_efficiency = ifelse(is.na(off_data$rz_efficiency), 0.6, off_data$rz_efficiency),
      rz_epa_per_play = ifelse(is.na(off_data$rz_epa_per_play), 0, off_data$rz_epa_per_play),
      third_down_rate = ifelse(is.na(off_data$third_down_rate), 0.4, off_data$third_down_rate),
      third_down_epa = ifelse(is.na(off_data$third_down_epa), 0, off_data$third_down_epa),
      late_game_epa = ifelse(is.na(off_data$late_game_epa), 0, off_data$late_game_epa),
      two_min_epa = ifelse(is.na(off_data$two_min_epa), 0, off_data$two_min_epa),
      goal_to_go_td_rate = ifelse(is.na(off_data$goal_to_go_td_rate), 0.5, off_data$goal_to_go_td_rate),

      # Core defensive metrics
      def_epa_per_play = ifelse(is.na(def_data$def_epa_per_play), 0, def_data$def_epa_per_play),
      def_success_rate_allowed = ifelse(is.na(def_data$def_success_rate_allowed), 0.55, def_data$def_success_rate_allowed),

      # Enhanced pass defense
      pass_def_epa_per_play = ifelse(is.na(def_data$pass_def_epa_per_play), 0, def_data$pass_def_epa_per_play),
      pass_def_epa_per_dropback = ifelse(is.na(def_data$pass_def_epa_per_dropback), 0, def_data$pass_def_epa_per_dropback),
      def_cpoe_allowed = ifelse(is.na(def_data$def_cpoe_allowed), 0, def_data$def_cpoe_allowed),
      sack_rate = ifelse(is.na(def_data$sack_rate), 0.07, def_data$sack_rate),
      qb_hit_rate = ifelse(is.na(def_data$qb_hit_rate), 0.2, def_data$qb_hit_rate),
      deep_pass_def_epa = ifelse(is.na(def_data$deep_pass_def_epa), 0, def_data$deep_pass_def_epa),
      short_pass_def_epa = ifelse(is.na(def_data$short_pass_def_epa), 0, def_data$short_pass_def_epa),

      # Enhanced rush defense
      rush_def_epa_per_play = ifelse(is.na(def_data$rush_def_epa_per_play), 0, def_data$rush_def_epa_per_play),
      rush_def_success_rate = ifelse(is.na(def_data$rush_def_success_rate), 0.55, def_data$rush_def_success_rate),
      explosive_rush_allowed_rate = ifelse(is.na(def_data$explosive_rush_allowed_rate), 0.1, def_data$explosive_rush_allowed_rate),

      # Enhanced defensive situational metrics
      rz_def_efficiency = ifelse(is.na(def_data$rz_def_efficiency), 0.6, def_data$rz_def_efficiency),
      rz_def_epa_per_play = ifelse(is.na(def_data$rz_def_epa_per_play), 0, def_data$rz_def_epa_per_play),
      third_def_rate = ifelse(is.na(def_data$third_def_rate), 0.4, def_data$third_def_rate),
      third_def_epa = ifelse(is.na(def_data$third_def_epa), 0, def_data$third_def_epa),
      late_game_def_epa = ifelse(is.na(def_data$late_game_def_epa), 0, def_data$late_game_def_epa),
      two_min_def_epa = ifelse(is.na(def_data$two_min_def_epa), 0, def_data$two_min_def_epa),

      # Enhanced special teams
      st_off_epa = ifelse(length(st_data$st_epa_per_play_offense) > 0 && !is.na(st_data$st_epa_per_play_offense),
                          st_data$st_epa_per_play_offense, 0),
      st_def_epa = ifelse(length(st_data$st_epa_per_play_defense) > 0 && !is.na(st_data$st_epa_per_play_defense),
                          st_data$st_epa_per_play_defense, 0),
      fg_epa_offense = ifelse(length(st_data$fg_epa_offense) > 0 && !is.na(st_data$fg_epa_offense),
                             st_data$fg_epa_offense, 0),
      punt_epa_offense = ifelse(length(st_data$punt_epa_offense) > 0 && !is.na(st_data$punt_epa_offense),
                               st_data$punt_epa_offense, 0),

      # Enhanced team strength calculation
      team_strength = (off_data$off_epa_per_play - league_avg_off_epa) - (def_data$def_epa_per_play - league_avg_off_epa) +
                     (ifelse(is.na(off_data$cpoe), 0, off_data$cpoe) * 5) +  # CPOE factor
                     (ifelse(is.na(off_data$late_game_epa), 0, off_data$late_game_epa) * 2)  # Clutch factor
    )
  }

  return(team_metrics)
}

# Enhanced data pipeline with new metrics
get_enhanced_season_data <- function() {

  cat("Loading enhanced NFL data with CPOE and advanced EPA metrics...\n")

  # Load recent seasons with heavy bias toward most recent
  recent_seasons <- c(2022, 2023, 2024)
  weights <- c(0.2, 0.3, 0.5)

  all_data <- list()

  for (i in seq_along(recent_seasons)) {
    season <- recent_seasons[i]
    weight <- weights[i]

    cat(sprintf("Loading %d season data with enhanced metrics (weight: %.1f)...\n", season, weight))

    # Load play-by-play data
    pbp <- load_pbp(season)
    schedules <- load_schedules(season)

    # Calculate enhanced team metrics for this season
    team_metrics <- calculate_enhanced_team_metrics(pbp, schedules, season, weight)
    all_data[[as.character(season)]] <- team_metrics
  }

  # Combine all seasons with weights
  final_metrics <- combine_weighted_seasons_enhanced(all_data, recent_seasons, weights)

  return(final_metrics)
}

# Enhanced weighted combination function
combine_weighted_seasons_enhanced <- function(all_data, seasons, weights) {

  cat("Combining seasons with enhanced time weighting...\n")

  # Get all teams across all seasons
  all_teams <- unique(unlist(lapply(all_data, function(season_data) names(season_data))))

  final_metrics <- list()

  for (team in all_teams) {
    cat(sprintf("  Combining enhanced data for %s...\n", team))

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

    # Calculate enhanced weighted averages
    weighted_metrics <- calculate_weighted_team_metrics_enhanced(team_data, team_weights)
    weighted_metrics$team <- team

    final_metrics[[team]] <- weighted_metrics
  }

  return(final_metrics)
}

# Enhanced weighted averaging with new metrics
calculate_weighted_team_metrics_enhanced <- function(team_data, weights) {

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

  # Enhanced trend calculations with multiple factors
  if (length(team_data) >= 2) {
    recent_data <- team_data[[length(team_data)]]  # Most recent season
    earlier_data <- team_data[[1]]  # Earliest season

    # Core trends
    result$off_trend <- recent_data$off_epa_per_play - earlier_data$off_epa_per_play
    result$def_trend <- recent_data$def_epa_per_play - earlier_data$def_epa_per_play

    # Enhanced trends
    result$cpoe_trend <- recent_data$cpoe - earlier_data$cpoe
    result$late_game_trend <- recent_data$late_game_epa - earlier_data$late_game_epa
    result$rz_trend <- recent_data$rz_efficiency - earlier_data$rz_efficiency
  } else {
    result$off_trend <- 0
    result$def_trend <- 0
    result$cpoe_trend <- 0
    result$late_game_trend <- 0
    result$rz_trend <- 0
  }

  return(result)
}

# Main function to create enhanced 2025 prediction dataset
create_enhanced_2025_prediction_data <- function() {

  cat("🏈 Creating ENHANCED 2025-2026 NFL Prediction Dataset 🏈\n")
  cat("=====================================================\n")
  cat("📈 Including CPOE, Advanced EPA, and Situational Metrics\n\n")

  # Get enhanced team metrics
  team_metrics <- get_enhanced_season_data()

  # Convert to dataframe with all enhanced metrics
  metrics_df <- do.call(rbind, lapply(names(team_metrics), function(team) {
    metrics <- team_metrics[[team]]
    data.frame(
      team = team,

      # Core metrics
      off_epa_per_play = metrics$off_epa_per_play,
      def_epa_per_play = metrics$def_epa_per_play,
      off_success_rate = metrics$off_success_rate,
      def_success_rate_allowed = metrics$def_success_rate_allowed,

      # Enhanced passing metrics
      pass_epa_per_play = metrics$pass_epa_per_play,
      pass_epa_per_dropback = metrics$pass_epa_per_dropback,
      cpoe = metrics$cpoe,
      epa_cpoe_composite = metrics$epa_cpoe_composite,
      avg_depth_of_target = metrics$avg_depth_of_target,
      deep_pass_rate = metrics$deep_pass_rate,
      short_pass_efficiency = metrics$short_pass_efficiency,
      deep_pass_efficiency = metrics$deep_pass_efficiency,
      avg_time_to_throw = metrics$avg_time_to_throw,
      quick_pass_rate = metrics$quick_pass_rate,

      # Enhanced rushing metrics
      rush_epa_per_play = metrics$rush_epa_per_play,
      rush_success_rate = metrics$rush_success_rate,
      explosive_rush_rate = metrics$explosive_rush_rate,

      # Enhanced defensive metrics
      pass_def_epa_per_play = metrics$pass_def_epa_per_play,
      pass_def_epa_per_dropback = metrics$pass_def_epa_per_dropback,
      def_cpoe_allowed = metrics$def_cpoe_allowed,
      sack_rate = metrics$sack_rate,
      qb_hit_rate = metrics$qb_hit_rate,
      deep_pass_def_epa = metrics$deep_pass_def_epa,
      short_pass_def_epa = metrics$short_pass_def_epa,
      rush_def_epa_per_play = metrics$rush_def_epa_per_play,
      rush_def_success_rate = metrics$rush_def_success_rate,
      explosive_rush_allowed_rate = metrics$explosive_rush_allowed_rate,

      # Enhanced situational metrics
      rz_efficiency = metrics$rz_efficiency,
      rz_epa_per_play = metrics$rz_epa_per_play,
      third_down_rate = metrics$third_down_rate,
      third_down_epa = metrics$third_down_epa,
      late_game_epa = metrics$late_game_epa,
      two_min_epa = metrics$two_min_epa,
      goal_to_go_td_rate = metrics$goal_to_go_td_rate,
      rz_def_efficiency = metrics$rz_def_efficiency,
      rz_def_epa_per_play = metrics$rz_def_epa_per_play,
      third_def_rate = metrics$third_def_rate,
      third_def_epa = metrics$third_def_epa,
      late_game_def_epa = metrics$late_game_def_epa,
      two_min_def_epa = metrics$two_min_def_epa,

      # Special teams
      st_off_epa = metrics$st_off_epa,
      st_def_epa = metrics$st_def_epa,
      fg_epa_offense = metrics$fg_epa_offense,
      punt_epa_offense = metrics$punt_epa_offense,

      # Enhanced team strength and trends
      team_strength = metrics$team_strength,
      off_trend = metrics$off_trend,
      def_trend = metrics$def_trend,
      cpoe_trend = metrics$cpoe_trend,
      late_game_trend = metrics$late_game_trend,
      rz_trend = metrics$rz_trend,

      stringsAsFactors = FALSE
    )
  }))

  # Save the enhanced metrics
  write_csv(metrics_df, "data/team_metrics_2025_enhanced.csv")

  cat("✅ ENHANCED 2025-2026 prediction data created!\n")
  cat(sprintf("📊 Enhanced data for %d teams saved to data/team_metrics_2025_enhanced.csv\n", nrow(metrics_df)))
  cat(sprintf("🎯 Added %d new advanced metrics including CPOE, situational EPA, and clutch factors\n",
              ncol(metrics_df) - 18))  # Subtract original metrics count

  return(metrics_df)
}

# Run if executed directly
if (!interactive()) {
  enhanced_metrics <- create_enhanced_2025_prediction_data()
}
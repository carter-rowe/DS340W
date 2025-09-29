library(nflreadr)
library(dplyr)
library(readr)
library(stringr)

# Enhanced Injury Impact Analysis System
# Phase 1 improvements with position-specific weights and advanced calculations

# Enhanced position impact weights based on research and EPA impact
get_enhanced_position_weights <- function() {
  list(
    # Offense - High Impact
    "QB" = list(
      base_weight = 12.0,
      injury_multipliers = list(
        "shoulder" = 1.8, "arm" = 1.7, "hand" = 1.6, "wrist" = 1.5,
        "knee" = 1.4, "ankle" = 1.3, "back" = 1.4, "concussion" = 2.0
      ),
      replacement_dropoff = 0.65  # 65% dropoff to backup QB
    ),

    # Running Backs - Medium-High Impact
    "RB" = list(
      base_weight = 4.5,
      injury_multipliers = list(
        "knee" = 1.6, "ankle" = 1.5, "hamstring" = 1.4, "groin" = 1.3,
        "shoulder" = 1.2, "concussion" = 1.5, "ribs" = 1.1
      ),
      replacement_dropoff = 0.35
    ),

    # Wide Receivers - Medium Impact
    "WR" = list(
      base_weight = 3.5,
      injury_multipliers = list(
        "hamstring" = 1.5, "ankle" = 1.4, "knee" = 1.3, "groin" = 1.3,
        "shoulder" = 1.2, "hand" = 1.3, "concussion" = 1.4
      ),
      replacement_dropoff = 0.25
    ),

    # Tight Ends - Medium Impact
    "TE" = list(
      base_weight = 3.0,
      injury_multipliers = list(
        "ankle" = 1.4, "knee" = 1.3, "shoulder" = 1.2, "back" = 1.2,
        "hamstring" = 1.3, "concussion" = 1.3
      ),
      replacement_dropoff = 0.30
    ),

    # Offensive Line - High Impact (Pass Protection)
    "OT" = list(
      base_weight = 5.5,
      injury_multipliers = list(
        "ankle" = 1.5, "knee" = 1.4, "back" = 1.3, "shoulder" = 1.2,
        "hand" = 1.4, "concussion" = 1.2
      ),
      replacement_dropoff = 0.45
    ),

    "OG" = list(
      base_weight = 4.0,
      injury_multipliers = list(
        "ankle" = 1.4, "knee" = 1.3, "back" = 1.2, "shoulder" = 1.1,
        "hand" = 1.3
      ),
      replacement_dropoff = 0.35
    ),

    "C" = list(
      base_weight = 4.5,
      injury_multipliers = list(
        "ankle" = 1.4, "knee" = 1.3, "back" = 1.3, "shoulder" = 1.2,
        "hand" = 1.4, "concussion" = 1.3
      ),
      replacement_dropoff = 0.40
    ),

    # Defense - Variable Impact
    "DE" = list(
      base_weight = 4.0,
      injury_multipliers = list(
        "ankle" = 1.3, "knee" = 1.4, "shoulder" = 1.2, "back" = 1.2,
        "hand" = 1.2, "concussion" = 1.2
      ),
      replacement_dropoff = 0.30
    ),

    "DT" = list(
      base_weight = 3.5,
      injury_multipliers = list(
        "ankle" = 1.3, "knee" = 1.3, "shoulder" = 1.1, "back" = 1.2,
        "hand" = 1.1
      ),
      replacement_dropoff = 0.25
    ),

    "LB" = list(
      base_weight = 3.5,
      injury_multipliers = list(
        "ankle" = 1.3, "knee" = 1.4, "shoulder" = 1.2, "concussion" = 1.3,
        "hamstring" = 1.2
      ),
      replacement_dropoff = 0.30
    ),

    "CB" = list(
      base_weight = 3.8,
      injury_multipliers = list(
        "ankle" = 1.4, "hamstring" = 1.5, "knee" = 1.3, "groin" = 1.3,
        "concussion" = 1.4, "shoulder" = 1.1
      ),
      replacement_dropoff = 0.35
    ),

    "S" = list(
      base_weight = 3.2,
      injury_multipliers = list(
        "ankle" = 1.3, "hamstring" = 1.3, "knee" = 1.3, "concussion" = 1.4,
        "shoulder" = 1.1
      ),
      replacement_dropoff = 0.30
    ),

    # Special Teams
    "K" = list(
      base_weight = 1.5,
      injury_multipliers = list(
        "ankle" = 1.8, "knee" = 1.6, "groin" = 1.4, "hip" = 1.3
      ),
      replacement_dropoff = 0.60
    ),

    "P" = list(
      base_weight = 1.0,
      injury_multipliers = list(
        "ankle" = 1.5, "knee" = 1.4, "groin" = 1.3
      ),
      replacement_dropoff = 0.40
    ),

    "LS" = list(
      base_weight = 0.5,
      injury_multipliers = list(
        "back" = 1.2, "shoulder" = 1.1
      ),
      replacement_dropoff = 0.20
    )
  )
}

# Enhanced injury severity classification
classify_injury_severity_enhanced <- function(injury_description, position) {

  injury_lower <- tolower(injury_description)

  # High severity injuries (major impact on performance)
  high_severity_patterns <- c(
    "concussion", "head", "brain", "acl", "mcl", "lcl", "pcl", "meniscus",
    "torn", "fracture", "break", "surgery", "ir", "injured.reserve",
    "achilles", "pectoral", "bicep.tear", "tricep.tear"
  )

  # Medium severity injuries (moderate impact)
  medium_severity_patterns <- c(
    "knee", "ankle", "shoulder", "back", "spine", "hamstring", "quad",
    "groin", "hip", "calf", "strain", "sprain"
  )

  # Position-specific severity adjustments
  position_specific_high <- list(
    "QB" = c("shoulder", "arm", "elbow", "hand", "wrist", "thumb"),
    "K" = c("ankle", "knee", "groin", "hip"),
    "P" = c("ankle", "knee", "groin"),
    "RB" = c("knee", "ankle", "hamstring"),
    "WR" = c("hamstring", "ankle", "groin"),
    "CB" = c("hamstring", "ankle", "groin")
  )

  # Check for high severity
  if (any(str_detect(injury_lower, high_severity_patterns))) {
    return("High")
  }

  # Check position-specific high severity
  if (position %in% names(position_specific_high)) {
    pos_patterns <- position_specific_high[[position]]
    if (any(str_detect(injury_lower, pos_patterns))) {
      return("High")
    }
  }

  # Check for medium severity
  if (any(str_detect(injury_lower, medium_severity_patterns))) {
    return("Medium")
  }

  # Default to low severity
  return("Low")
}

# Enhanced participation probability calculation
calculate_enhanced_participation_probability <- function(report_status, injury_severity, position, week_of_injury) {

  # Base probabilities by status
  base_prob <- case_when(
    str_detect(tolower(report_status), "out|ir|injured.reserve") ~ 0.0,
    str_detect(tolower(report_status), "doubtful") ~ 0.20,
    str_detect(tolower(report_status), "questionable") ~ 0.55,
    str_detect(tolower(report_status), "probable|likely") ~ 0.85,
    str_detect(tolower(report_status), "full|cleared") ~ 1.0,
    TRUE ~ 0.75  # Default for unclear status
  )

  # Adjust for injury severity
  severity_adjustment <- case_when(
    injury_severity == "High" ~ -0.15,
    injury_severity == "Medium" ~ -0.05,
    TRUE ~ 0.0
  )

  # Position-specific adjustments (some positions play through pain better)
  position_adjustment <- case_when(
    position %in% c("QB", "K", "P") ~ -0.10,  # Skill positions affected more
    position %in% c("OT", "OG", "C") ~ 0.05,  # Linemen tend to play through pain
    position %in% c("CB", "WR") ~ -0.05,  # Speed positions affected by lower body
    TRUE ~ 0.0
  )

  # Time factor (injuries linger differently)
  weeks_since_injury <- max(1, week_of_injury)
  time_adjustment <- ifelse(weeks_since_injury > 2 && injury_severity != "High", 0.10, 0.0)

  final_prob <- base_prob + severity_adjustment + position_adjustment + time_adjustment

  return(max(0.0, min(1.0, final_prob)))  # Clamp between 0 and 1
}

# Enhanced effectiveness calculation when playing injured
calculate_enhanced_effectiveness <- function(participation_probability, injury_severity, position, injury_description) {

  # Base effectiveness by participation probability
  base_effectiveness <- ifelse(participation_probability < 0.3, 0.50,
                              ifelse(participation_probability < 0.6, 0.70,
                                    ifelse(participation_probability < 0.9, 0.85, 0.95)))

  # Injury-specific effectiveness impacts
  injury_lower <- tolower(injury_description)

  injury_effectiveness_impact <- case_when(
    # Mobility-affecting injuries
    str_detect(injury_lower, "hamstring|groin|ankle|knee") ~ -0.20,
    # Upper body injuries affecting throwing/catching
    str_detect(injury_lower, "shoulder|arm|hand|wrist|finger") ~ -0.15,
    # Concussions affect decision-making
    str_detect(injury_lower, "concussion|head") ~ -0.25,
    # Back injuries affect everything
    str_detect(injury_lower, "back|spine") ~ -0.18,
    # Less impactful injuries
    str_detect(injury_lower, "ribs|hip|calf") ~ -0.10,
    TRUE ~ -0.05
  )

  # Position-specific effectiveness adjustments
  position_adjustment <- case_when(
    position == "QB" && str_detect(injury_lower, "shoulder|arm|hand") ~ -0.15,
    position %in% c("RB", "WR", "CB") && str_detect(injury_lower, "hamstring|ankle|knee") ~ -0.15,
    position %in% c("K", "P") && str_detect(injury_lower, "ankle|knee|groin") ~ -0.20,
    TRUE ~ 0.0
  )

  final_effectiveness <- base_effectiveness + injury_effectiveness_impact + position_adjustment

  return(max(0.20, min(1.0, final_effectiveness)))  # Minimum 20% effectiveness
}

# Enhanced team injury impact calculation
calculate_enhanced_team_injury_impact <- function(team_code, injuries, player_impact_db = NULL) {

  position_weights <- get_enhanced_position_weights()

  # Get injuries for this team
  team_injuries <- injuries %>%
    filter(team == team_code)

  if (nrow(team_injuries) == 0) {
    return(list(
      total_epa_impact = 0,
      position_impacts = list(),
      injured_players = list(),
      severity_score = 0,
      total_players_affected = 0,
      key_player_impacts = list()
    ))
  }

  cat(sprintf("Analyzing %d enhanced injuries for %s...\n", nrow(team_injuries), team_code))

  total_impact <- 0
  position_impacts <- list()
  injured_players <- list()
  key_player_impacts <- list()

  for (i in 1:nrow(team_injuries)) {
    injury <- team_injuries[i, ]
    player_name <- injury$full_name
    position <- injury$position

    # Get position weights
    if (!(position %in% names(position_weights))) {
      # Default weights for unknown positions
      pos_weight_info <- list(
        base_weight = 2.0,
        injury_multipliers = list(),
        replacement_dropoff = 0.25
      )
    } else {
      pos_weight_info <- position_weights[[position]]
    }

    # Enhanced injury severity
    enhanced_severity <- classify_injury_severity_enhanced(injury$report_primary, position)

    # Enhanced participation probability
    enhanced_participation <- calculate_enhanced_participation_probability(
      injury$report_status, enhanced_severity, position, injury$week
    )

    # Enhanced effectiveness when playing
    enhanced_effectiveness <- calculate_enhanced_effectiveness(
      enhanced_participation, enhanced_severity, position, injury$report_primary
    )

    # Calculate injury-specific multiplier
    injury_multiplier <- 1.0
    injury_lower <- tolower(injury$report_primary)

    for (injury_type in names(pos_weight_info$injury_multipliers)) {
      if (str_detect(injury_lower, injury_type)) {
        injury_multiplier <- max(injury_multiplier, pos_weight_info$injury_multipliers[[injury_type]])
      }
    }

    # Calculate total impact
    participation_loss <- 1 - enhanced_participation
    effectiveness_loss <- enhanced_participation * (1 - enhanced_effectiveness)
    total_availability_loss <- participation_loss + (effectiveness_loss * 0.7)  # Effectiveness loss weighted at 70%

    # Base EPA impact calculation
    base_epa_impact <- pos_weight_info$base_weight * total_availability_loss * injury_multiplier

    # Replacement quality factor
    replacement_factor <- pos_weight_info$replacement_dropoff
    adjusted_impact <- base_epa_impact * (1 + replacement_factor)

    total_impact <- total_impact + adjusted_impact

    # Store position-specific impacts
    if (!(position %in% names(position_impacts))) {
      position_impacts[[position]] <- 0
    }
    position_impacts[[position]] <- position_impacts[[position]] + adjusted_impact

    # Store detailed player information
    injured_players[[player_name]] <- list(
      position = position,
      status = injury$report_status,
      injury_type = injury$report_primary,
      severity = enhanced_severity,
      epa_impact = adjusted_impact,
      participation_prob = enhanced_participation,
      effectiveness = enhanced_effectiveness,
      base_weight = pos_weight_info$base_weight,
      injury_multiplier = injury_multiplier
    )

    # Identify key player impacts (high-impact positions)
    if (pos_weight_info$base_weight >= 4.0 && adjusted_impact >= 2.0) {
      key_player_impacts[[player_name]] <- list(
        position = position,
        impact = adjusted_impact,
        severity = enhanced_severity,
        participation_prob = enhanced_participation
      )
    }
  }

  # Calculate overall severity score
  severity_score <- sum(sapply(injured_players, function(p) {
    case_when(
      p$severity == "High" ~ 3,
      p$severity == "Medium" ~ 2,
      TRUE ~ 1
    ) * p$base_weight
  }))

  return(list(
    total_epa_impact = round(total_impact, 2),
    position_impacts = position_impacts,
    injured_players = injured_players,
    severity_score = round(severity_score, 1),
    total_players_affected = length(injured_players),
    key_player_impacts = key_player_impacts,
    impact_breakdown = list(
      high_impact_players = length(key_player_impacts),
      offensive_impact = sum(sapply(names(position_impacts), function(pos) {
        if (pos %in% c("QB", "RB", "WR", "TE", "OT", "OG", "C")) position_impacts[[pos]] else 0
      })),
      defensive_impact = sum(sapply(names(position_impacts), function(pos) {
        if (pos %in% c("DE", "DT", "LB", "CB", "S")) position_impacts[[pos]] else 0
      })),
      special_teams_impact = sum(sapply(names(position_impacts), function(pos) {
        if (pos %in% c("K", "P", "LS")) position_impacts[[pos]] else 0
      }))
    )
  ))
}

# Enhanced game prediction with injury impact
predict_game_with_enhanced_injury_impact <- function(home_team, away_team, team_metrics,
                                                    current_injuries, player_impact_db = NULL) {

  cat(sprintf("Making enhanced injury-aware prediction: %s vs %s\n", home_team, away_team))

  # Calculate enhanced injury impacts for both teams
  home_injury_impact <- calculate_enhanced_team_injury_impact(home_team, current_injuries, player_impact_db)
  away_injury_impact <- calculate_enhanced_team_injury_impact(away_team, current_injuries, player_impact_db)

  # Base prediction (using existing function)
  if (exists("predict_2025_game")) {
    base_prediction <- predict_2025_game(home_team, away_team, team_metrics)
  } else {
    stop("Base prediction function not found")
  }

  if (!is.null(base_prediction$error)) {
    return(base_prediction)
  }

  # Calculate net injury advantage (positive favors home team)
  net_injury_advantage <- away_injury_impact$total_epa_impact - home_injury_impact$total_epa_impact

  # Apply injury adjustment to prediction
  adjusted_margin <- base_prediction$predicted_margin + net_injury_advantage

  # Recalculate scores and probabilities
  league_avg_score <- 23.5
  adjusted_home_score <- league_avg_score + (adjusted_margin / 2)
  adjusted_away_score <- league_avg_score - (adjusted_margin / 2)

  # Calculate adjusted win probability
  game_variance <- 14.0
  adjusted_home_win_prob <- pnorm(adjusted_margin / game_variance)

  # Ensure reasonable scores
  adjusted_home_score <- max(7, min(45, adjusted_home_score))
  adjusted_away_score <- max(7, min(45, adjusted_away_score))

  # Determine confidence (injury uncertainty reduces confidence)
  injury_uncertainty <- (home_injury_impact$total_epa_impact + away_injury_impact$total_epa_impact) * 0.1
  base_confidence_score <- abs(adjusted_margin) - injury_uncertainty

  adjusted_confidence <- ifelse(base_confidence_score > 10, "High",
                               ifelse(base_confidence_score > 5, "Medium", "Low"))

  return(list(
    predicted_margin = round(adjusted_margin, 1),
    home_score = round(adjusted_home_score, 0),
    away_score = round(adjusted_away_score, 0),
    home_win_prob = round(adjusted_home_win_prob, 3),
    away_win_prob = round(1 - adjusted_home_win_prob, 3),
    predicted_winner = ifelse(adjusted_margin > 0, home_team, away_team),
    confidence = adjusted_confidence,
    model_type = "Enhanced Injury-Aware",

    # Enhanced injury analysis
    injury_analysis = list(
      home_team_injuries = home_injury_impact,
      away_team_injuries = away_injury_impact,
      net_injury_advantage = round(net_injury_advantage, 2),
      injury_impact_on_line = round(net_injury_advantage, 1),
      total_injury_uncertainty = round(injury_uncertainty, 2)
    ),

    # Base prediction for comparison
    base_prediction = list(
      base_margin = base_prediction$predicted_margin,
      injury_adjustment = round(net_injury_advantage, 1),
      margin_change = round(adjusted_margin - base_prediction$predicted_margin, 1)
    ),

    key_factors = list(
      injury_impact = ifelse(abs(net_injury_advantage) > 1.0, "Significant", "Minimal"),
      home_key_injuries = names(home_injury_impact$key_player_impacts),
      away_key_injuries = names(away_injury_impact$key_player_impacts),
      most_impactful_injury = determine_most_impactful_injury(home_injury_impact, away_injury_impact)
    )
  ))
}

# Helper function to determine most impactful injury
determine_most_impactful_injury <- function(home_injuries, away_injuries) {

  all_injuries <- c(home_injuries$injured_players, away_injuries$injured_players)

  if (length(all_injuries) == 0) {
    return("None")
  }

  # Find injury with highest impact
  max_impact <- 0
  most_impactful <- "None"

  for (player_name in names(all_injuries)) {
    if (all_injuries[[player_name]]$epa_impact > max_impact) {
      max_impact <- all_injuries[[player_name]]$epa_impact
      most_impactful <- paste0(player_name, " (", all_injuries[[player_name]]$position, ")")
    }
  }

  return(most_impactful)
}

cat("🏥 Enhanced Injury Analysis System Loaded!\n")
cat("📊 Features:\n")
cat("  - Position-specific impact weights (12 positions)\n")
cat("  - Injury-specific severity multipliers\n")
cat("  - Enhanced participation/effectiveness calculations\n")
cat("  - Replacement quality factors\n")
cat("  - Key player identification\n")
cat("  - Detailed impact breakdowns\n")
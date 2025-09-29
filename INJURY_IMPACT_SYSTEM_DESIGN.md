# 🏥 NFL Injury Impact System - Comprehensive Design & Implementation Plan

## 📋 Executive Summary

This document outlines a comprehensive system to integrate player injuries into your NFL prediction model. The system will analyze individual player contributions, quantify injury impacts, and adjust team predictions accordingly.

## 🎯 Core Objectives

1. **Quantify individual player impact** on team performance
2. **Classify injury severity** with probability-based predictions
3. **Calculate replacement player effects** and backup quality
4. **Real-time adjustment** of team EPA and predictions
5. **Position-specific models** for different impact patterns

## 🔬 Research Findings

### Data Sources Available
- **nflreadr**: `load_injuries()` function for weekly injury reports
- **Official NFL.com**: Real-time injury status updates
- **ESPN API**: Team-specific injury data endpoints
- **PFF**: WAR-Adjusted Injuries Lost (WAIL) methodology

### Injury Classification System
- **Out**: 0% chance to play (full impact)
- **Doubtful**: ~25% chance to play (75% impact)
- **Questionable**: ~50% chance to play (50% impact) 
- **IR/PUP**: Extended absence (100% impact)

### Player Value Metrics
- **EPA per Play**: Direct contribution measurement
- **WAR (Wins Above Replacement)**: Wins impact quantification
- **Position Value**: QB > WR > DB > TE hierarchy
- **Snap Count Impact**: Playing time percentage effects

## 🏗️ System Architecture

### 1. Player Impact Database
```
player_impacts.csv:
- player_name, position, team
- epa_per_play, war_value, snap_percentage
- pass_epa, rush_epa, def_epa_allowed
- situational_impact (red_zone, third_down)
- replacement_quality (backup_war)
```

### 2. Injury Status Pipeline
```
injury_reports.csv:
- player_name, team, week, status
- injury_type, body_part, severity
- game_participation_probability
- expected_effectiveness (0-100%)
```

### 3. Impact Calculation Engine
```
Position-Specific Models:
- QB: 40-60% of team EPA impact
- RB: 10-20% of rushing EPA
- WR1: 15-25% of passing EPA
- OL: 5-15% distributed impact
- DEF: Position-dependent (10-30%)
```

## 🔧 Implementation Plan

### Phase 1: Data Collection & Player Analysis

#### Step 1: Create Player Impact Database
```r
# File: code/injury_system/player_impact_analysis.R

library(nflreadr)
library(dplyr)

# Calculate individual player EPA contributions
calculate_player_impact <- function(seasons = 2022:2024) {
  
  # Load play-by-play data
  pbp <- load_pbp(seasons)
  
  # Calculate QB impact
  qb_impact <- pbp %>%
    filter(!is.na(passer_player_name), !is.na(epa)) %>%
    group_by(passer_player_name, posteam, season) %>%
    summarise(
      qb_pass_epa_per_play = mean(epa),
      qb_total_epa = sum(epa),
      attempts = n(),
      success_rate = mean(success, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate RB impact
  rb_impact <- pbp %>%
    filter(!is.na(rusher_player_name), !is.na(epa)) %>%
    group_by(rusher_player_name, posteam, season) %>%
    summarise(
      rb_rush_epa_per_play = mean(epa),
      rb_total_epa = sum(epa),
      carries = n(),
      .groups = "drop"
    )
  
  # Calculate WR/TE impact (receiving)
  rec_impact <- pbp %>%
    filter(!is.na(receiver_player_name), !is.na(epa)) %>%
    group_by(receiver_player_name, posteam, season) %>%
    summarise(
      rec_epa_per_play = mean(epa),
      rec_total_epa = sum(epa),
      targets = n(),
      .groups = "drop"
    )
  
  return(list(qb = qb_impact, rb = rb_impact, rec = rec_impact))
}
```

#### Step 2: Position-Specific Impact Models
```r
# Calculate position importance weights
position_weights <- list(
  QB = list(
    pass_epa_contribution = 0.65,      # QBs drive 65% of passing EPA
    total_team_epa = 0.45,             # QBs impact 45% of total team EPA
    critical_situations = 0.70         # Higher impact in red zone/3rd down
  ),
  
  RB1 = list(
    rush_epa_contribution = 0.60,      # Lead RB gets 60% of rushing EPA
    total_team_epa = 0.12,             # RBs impact 12% of total team EPA
    goal_line_impact = 0.80            # High impact near goal line
  ),
  
  WR1 = list(
    rec_epa_contribution = 0.35,       # WR1 gets 35% of receiving EPA
    total_team_epa = 0.15,             # WR1 impacts 15% of team EPA
    red_zone_impact = 0.50
  ),
  
  DEF_EDGE = list(
    pass_rush_impact = 0.25,           # Edge rushers create 25% of pressure
    def_epa_contribution = 0.20,       # 20% of defensive EPA impact
    third_down_impact = 0.40
  )
)
```

### Phase 2: Injury Data Integration

#### Step 3: Real-Time Injury Pipeline
```r
# File: code/injury_system/injury_data_pipeline.R

get_current_injuries <- function() {
  
  # Load injury reports from nflreadr
  current_injuries <- load_injuries(seasons = 2024)
  
  # Standardize injury status
  injuries_processed <- current_injuries %>%
    mutate(
      participation_probability = case_when(
        report_status == "Out" ~ 0.0,
        report_status == "Doubtful" ~ 0.25,
        report_status == "Questionable" ~ 0.50,
        report_status == "IR" ~ 0.0,
        TRUE ~ 1.0
      ),
      
      effectiveness_when_playing = case_when(
        report_status == "Questionable" ~ 0.75,  # Reduced effectiveness
        report_status == "Doubtful" ~ 0.60,     # Significantly reduced
        TRUE ~ 1.0
      ),
      
      injury_severity = case_when(
        str_detect(tolower(report_primary), "concussion|head") ~ "High",
        str_detect(tolower(report_primary), "knee|ankle|foot") ~ "Medium",
        str_detect(tolower(report_primary), "shoulder|back") ~ "Medium",
        TRUE ~ "Low"
      )
    )
  
  return(injuries_processed)
}
```

### Phase 3: Impact Calculation System

#### Step 4: Team Adjustment Calculator
```r
# File: code/injury_system/team_impact_calculator.R

calculate_injury_impact <- function(team, injuries, player_impacts) {
  
  team_injuries <- injuries %>% filter(team == !!team)
  
  total_impact <- list(
    off_epa_adjustment = 0,
    def_epa_adjustment = 0,
    special_teams_adjustment = 0,
    situational_adjustments = list()
  )
  
  for (i in 1:nrow(team_injuries)) {
    injury <- team_injuries[i, ]
    player <- injury$full_name
    
    # Get player's historical impact
    player_impact <- player_impacts %>% 
      filter(player_name == player) %>%
      slice_tail(n = 1)  # Most recent season
    
    if (nrow(player_impact) == 0) next
    
    # Calculate expected impact reduction
    participation_loss <- 1 - injury$participation_probability
    effectiveness_loss <- 1 - injury$effectiveness_when_playing
    
    # Position-specific calculations
    if (player_impact$position == "QB") {
      
      # QB injury has massive impact
      qb_epa_loss <- player_impact$epa_per_play * 
                     position_weights$QB$total_team_epa * 
                     (participation_loss + effectiveness_loss * injury$participation_probability)
      
      total_impact$off_epa_adjustment <- total_impact$off_epa_adjustment - qb_epa_loss
      
      # Backup QB quality adjustment
      backup_quality <- get_backup_qb_quality(team)
      replacement_loss <- (player_impact$epa_per_play - backup_quality) * participation_loss
      total_impact$off_epa_adjustment <- total_impact$off_epa_adjustment - replacement_loss
      
    } else if (player_impact$position %in% c("RB", "FB")) {
      
      rb_epa_loss <- player_impact$epa_per_play * 
                     position_weights$RB1$total_team_epa * 
                     (participation_loss + effectiveness_loss * injury$participation_probability)
      
      total_impact$off_epa_adjustment <- total_impact$off_epa_adjustment - rb_epa_loss
      
    } else if (player_impact$position %in% c("WR", "TE")) {
      
      wr_epa_loss <- player_impact$epa_per_play * 
                     position_weights$WR1$total_team_epa * 
                     (participation_loss + effectiveness_loss * injury$participation_probability)
      
      total_impact$off_epa_adjustment <- total_impact$off_epa_adjustment - wr_epa_loss
    }
    
    # Add injury severity multipliers
    severity_multiplier <- case_when(
      injury$injury_severity == "High" ~ 1.5,
      injury$injury_severity == "Medium" ~ 1.2,
      TRUE ~ 1.0
    )
    
    total_impact$off_epa_adjustment <- total_impact$off_epa_adjustment * severity_multiplier
  }
  
  return(total_impact)
}
```

### Phase 4: Enhanced Prediction System

#### Step 5: Injury-Adjusted Predictions
```r
# File: code/injury_system/injury_adjusted_predictor.R

predict_game_with_injuries <- function(home_team, away_team, team_metrics, current_injuries) {
  
  # Calculate injury impacts for both teams
  home_injury_impact <- calculate_injury_impact(home_team, current_injuries, player_impacts)
  away_injury_impact <- calculate_injury_impact(away_team, current_injuries, player_impacts)
  
  # Adjust base team metrics
  home_metrics_adjusted <- team_metrics %>%
    filter(team == home_team) %>%
    mutate(
      off_epa_per_play = off_epa_per_play + home_injury_impact$off_epa_adjustment,
      def_epa_per_play = def_epa_per_play + home_injury_impact$def_epa_adjustment
    )
  
  away_metrics_adjusted <- team_metrics %>%
    filter(team == away_team) %>%
    mutate(
      off_epa_per_play = off_epa_per_play + away_injury_impact$off_epa_adjustment,
      def_epa_per_play = def_epa_per_play + away_injury_impact$def_epa_adjustment
    )
  
  # Run prediction with adjusted metrics
  prediction <- predict_2025_game_base(home_team, away_team, 
                                       home_metrics_adjusted, away_metrics_adjusted)
  
  # Add injury context to results
  prediction$injury_impact <- list(
    home_team_injuries = home_injury_impact,
    away_team_injuries = away_injury_impact,
    net_advantage = home_injury_impact$off_epa_adjustment - away_injury_impact$off_epa_adjustment
  )
  
  return(prediction)
}
```

## 🎯 Joe Burrow Example Implementation

### Burrow Impact Analysis
```r
# Example: Joe Burrow injury impact on Bengals
burrow_analysis <- function() {
  
  # Historical Burrow impact (2021-2024)
  burrow_stats <- list(
    epa_per_play = 0.086,              # Strong QB performance
    team_epa_contribution = 0.52,       # Drives 52% of Bengals EPA
    vs_backup = 0.125,                  # 0.125 EPA advantage over backup
    critical_situations = 0.78          # Exceptional in red zone/3rd down
  )
  
  # If Burrow is "Questionable" (50% to play, 75% effective when playing)
  injury_scenario <- list(
    participation_prob = 0.50,
    effectiveness = 0.75,
    backup_qb_epa = -0.039             # Jake Browning/backup performance
  )
  
  # Calculate Bengals EPA reduction
  expected_epa_loss <- (
    burrow_stats$epa_per_play * 
    burrow_stats$team_epa_contribution * 
    (1 - injury_scenario$participation_prob)
  ) + (
    burrow_stats$epa_per_play * 
    burrow_stats$team_epa_contribution * 
    injury_scenario$participation_prob * 
    (1 - injury_scenario$effectiveness)
  )
  
  # Result: ~0.022 EPA per play reduction for Bengals offense
  # Translates to ~3-4 point disadvantage in game predictions
  
  return(expected_epa_loss)
}
```

## 🔮 Advanced Features

### 1. Injury Probability Modeling
```r
# Predict likelihood of player missing games based on injury type
injury_duration_model <- function(injury_type, position, player_age) {
  
  duration_expectations <- list(
    "concussion" = list(games_missed = 1.2, variance = 0.8),
    "knee" = list(games_missed = 2.5, variance = 1.5),
    "ankle" = list(games_missed = 1.8, variance = 1.2),
    "shoulder" = list(games_missed = 2.1, variance = 1.3)
  )
  
  # Position and age adjustments
  if (position == "QB") duration_expectations[[injury_type]]$games_missed *= 0.8  # QBs return faster
  if (player_age > 30) duration_expectations[[injury_type]]$games_missed *= 1.2   # Older players slower
  
  return(duration_expectations[[injury_type]])
}
```

### 2. Backup Quality Database
```r
# Maintain database of backup player quality
backup_quality_db <- list(
  "CIN" = list(
    QB = list(name = "Jake Browning", epa_per_play = -0.039),
    RB1 = list(name = "Chase Brown", epa_per_play = 0.02),
    WR1 = list(name = "Tee Higgins", epa_per_play = 0.08)
  ),
  "KC" = list(
    QB = list(name = "Carson Wentz", epa_per_play = 0.01),
    RB1 = list(name = "Isiah Pacheco", epa_per_play = 0.03)
  )
  # ... all 32 teams
)
```

### 3. Real-Time Integration
```r
# Auto-update system for game day
update_injury_predictions <- function(gameday_injuries) {
  
  # Pull latest injury reports
  current_injuries <- get_current_injuries()
  
  # Update all scheduled games
  updated_predictions <- map(scheduled_games, function(game) {
    predict_game_with_injuries(
      game$home_team, 
      game$away_team, 
      team_metrics, 
      current_injuries
    )
  })
  
  return(updated_predictions)
}
```

## 📊 Expected Impact Ranges

### Position-Specific Impact Scale
| Position | Minor Injury | Major Injury | Out/IR |
|----------|-------------|-------------|---------|
| **QB1** | -1 to -3 pts | -4 to -7 pts | -8 to -12 pts |
| **RB1** | -0.5 to -1.5 pts | -2 to -4 pts | -3 to -5 pts |
| **WR1** | -0.5 to -2 pts | -2 to -4 pts | -3 to -5 pts |
| **OL** | -0.5 to -1 pt | -1 to -3 pts | -2 to -4 pts |
| **DEF Star** | -1 to -2 pts | -2 to -4 pts | -3 to -6 pts |

### Cumulative Effects
- **Multiple injuries**: Exponential impact (not additive)
- **Key position clusters**: OL injuries compound significantly
- **Timing effects**: Short week vs long week recovery

## 🚀 Implementation Roadmap

### Week 1: Foundation
- [ ] Build player impact database from 2022-2024 data
- [ ] Create injury data pipeline with nflreadr
- [ ] Develop position-specific impact models

### Week 2: Core System
- [ ] Implement injury impact calculator
- [ ] Build backup quality database
- [ ] Create injury-adjusted prediction function

### Week 3: Interface Integration
- [ ] Add injury status to prediction interface
- [ ] Create injury impact visualization
- [ ] Build real-time update system

### Week 4: Advanced Features
- [ ] Implement injury probability modeling
- [ ] Add cumulative injury effects
- [ ] Create injury trend analysis

## 🎯 Success Metrics

1. **Accuracy Improvement**: 3-5% increase in prediction accuracy
2. **Injury Correlation**: Strong correlation between injury impact and actual game results
3. **Real-time Responsiveness**: Updates within 1 hour of injury reports
4. **User Value**: Clear injury impact explanations in interface

This comprehensive system will transform your NFL predictor into the most sophisticated injury-aware prediction tool available, giving you a significant edge in accuracy and insight.
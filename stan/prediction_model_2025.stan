data {
  int<lower=1> N_teams;                    // number of teams
  
  // Team metrics (current season weighted averages)
  vector[N_teams] off_epa;                 // offensive EPA per play
  vector[N_teams] def_epa;                 // defensive EPA per play (lower is better)
  vector[N_teams] off_success_rate;        // offensive success rate
  vector[N_teams] def_success_rate;        // defensive success rate allowed
  vector[N_teams] pass_epa;                // passing EPA per play
  vector[N_teams] rush_epa;                // rushing EPA per play
  vector[N_teams] pass_def_epa;            // pass defense EPA allowed
  vector[N_teams] rush_def_epa;            // rush defense EPA allowed
  vector[N_teams] st_epa;                  // special teams EPA
  vector[N_teams] team_strength;           // overall team strength
  vector[N_teams] off_trend;               // offensive trend (recent improvement)
  vector[N_teams] def_trend;               // defensive trend
  vector[N_teams] rz_efficiency;           // red zone efficiency
  vector[N_teams] third_down_rate;         // third down conversion rate
  vector[N_teams] rz_def_efficiency;       // red zone defense efficiency
  vector[N_teams] third_def_rate;          // third down defense rate
}

parameters {
  // Model coefficients
  real alpha_home;                         // home field advantage
  real beta_off_epa;                       // offensive EPA coefficient
  real beta_def_epa;                       // defensive EPA coefficient
  real beta_success_off;                   // offensive success rate coefficient
  real beta_success_def;                   // defensive success rate coefficient
  real beta_pass_epa;                      // passing EPA coefficient
  real beta_rush_epa;                      // rushing EPA coefficient
  real beta_pass_def;                      // pass defense coefficient
  real beta_rush_def;                      // rush defense coefficient
  real beta_st;                            // special teams coefficient
  real beta_trend_off;                     // offensive trend coefficient
  real beta_trend_def;                     // defensive trend coefficient
  real beta_rz;                            // red zone coefficient
  real beta_third;                         // third down coefficient
  real beta_rz_def;                        // red zone defense coefficient
  real beta_third_def;                     // third down defense coefficient
  
  // Model variance parameters
  real<lower=0> sigma_game;                // game-to-game variance
  real<lower=0> sigma_team;                // team strength variance
}

transformed parameters {
  // Team strength adjustments based on all factors
  vector[N_teams] adjusted_team_strength;
  
  for (i in 1:N_teams) {
    adjusted_team_strength[i] = team_strength[i] + 
                               beta_trend_off * off_trend[i] + 
                               beta_trend_def * def_trend[i] +
                               beta_rz * rz_efficiency[i] +
                               beta_third * third_down_rate[i] +
                               beta_rz_def * (1 - rz_def_efficiency[i]) +  // Lower def efficiency is better
                               beta_third_def * (1 - third_def_rate[i]);   // Lower def rate is better
  }
}

model {
  // Priors
  alpha_home ~ normal(2.5, 1);            // Home field advantage ~2.5 points
  beta_off_epa ~ normal(0, 15);           // EPA coefficients
  beta_def_epa ~ normal(0, 15);
  beta_success_off ~ normal(0, 10);       // Success rate coefficients
  beta_success_def ~ normal(0, 10);
  beta_pass_epa ~ normal(0, 12);          // Play-type coefficients
  beta_rush_epa ~ normal(0, 12);
  beta_pass_def ~ normal(0, 12);
  beta_rush_def ~ normal(0, 12);
  beta_st ~ normal(0, 8);                 // Special teams coefficient
  beta_trend_off ~ normal(0, 5);          // Trend coefficients (recent bias)
  beta_trend_def ~ normal(0, 5);
  beta_rz ~ normal(0, 6);                 // Situational coefficients
  beta_third ~ normal(0, 6);
  beta_rz_def ~ normal(0, 6);
  beta_third_def ~ normal(0, 6);
  
  // Variance priors
  sigma_game ~ normal(0, 8);
  sigma_team ~ normal(0, 3);
  
  // Team strength priors (regularization)
  adjusted_team_strength ~ normal(0, sigma_team);
}

generated quantities {
  // Function to predict game outcome between any two teams
  real predict_game(int home_team_idx, int away_team_idx) {
    real home_strength = adjusted_team_strength[home_team_idx];
    real away_strength = adjusted_team_strength[away_team_idx];
    
    // EPA differentials
    real epa_diff = off_epa[home_team_idx] - off_epa[away_team_idx];
    real def_epa_diff = def_epa[away_team_idx] - def_epa[home_team_idx];  // Lower def EPA is better
    
    // Success rate differentials
    real success_off_diff = off_success_rate[home_team_idx] - off_success_rate[away_team_idx];
    real success_def_diff = def_success_rate[away_team_idx] - def_success_rate[home_team_idx];
    
    // Play-type differentials
    real pass_diff = pass_epa[home_team_idx] - pass_epa[away_team_idx];
    real rush_diff = rush_epa[home_team_idx] - rush_epa[away_team_idx];
    real pass_def_diff = pass_def_epa[away_team_idx] - pass_def_epa[home_team_idx];
    real rush_def_diff = rush_def_epa[away_team_idx] - rush_def_epa[home_team_idx];
    
    // Special teams differential
    real st_diff = st_epa[home_team_idx] - st_epa[away_team_idx];
    
    // Predicted margin
    real predicted_margin = alpha_home + 
                           (home_strength - away_strength) +
                           beta_off_epa * epa_diff +
                           beta_def_epa * def_epa_diff +
                           beta_success_off * success_off_diff +
                           beta_success_def * success_def_diff +
                           beta_pass_epa * pass_diff +
                           beta_rush_epa * rush_diff +
                           beta_pass_def * pass_def_diff +
                           beta_rush_def * rush_def_diff +
                           beta_st * st_diff;
    
    return predicted_margin;
  }
  
  // Predict scores for any matchup
  vector[2] predict_scores(int home_team_idx, int away_team_idx, real league_avg_score) {
    real predicted_margin = predict_game(home_team_idx, away_team_idx);
    real home_score = league_avg_score + predicted_margin / 2;
    real away_score = league_avg_score - predicted_margin / 2;
    
    return [home_score, away_score]';
  }
}
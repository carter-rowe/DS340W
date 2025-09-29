data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  int<lower=1> num_seasons;                                   // number of seasons
  int<lower=1, upper=num_seasons> season[num_games];          // seasons
  
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  real outcome[num_games];                                    // outcome for game g (point differential)
  
  // Rest advantages (existing)
  int<lower=0,upper=1> h_adv[num_games];                      // home field advantage
  int<lower=-1,upper=1> bye[num_games];                       // bye week advantage
  int<lower=-1,upper=1> mnf[num_games];                       // monday night advantage
  int<lower=-1,upper=1> mini[num_games];                      // mini bye advantage
  
  // EPA metrics
  real home_off_epa_avg[num_games];                          // home team offensive EPA per play
  real away_off_epa_avg[num_games];                          // away team offensive EPA per play
  real home_def_epa_allowed_avg[num_games];                  // home team defensive EPA allowed per play
  real away_def_epa_allowed_avg[num_games];                  // away team defensive EPA allowed per play
  
  // Success rates
  real home_off_success_rate[num_games];                     // home team offensive success rate
  real away_off_success_rate[num_games];                     // away team offensive success rate
  real home_def_success_rate_allowed[num_games];             // home team defensive success rate allowed
  real away_def_success_rate_allowed[num_games];             // away team defensive success rate allowed
  
  // Passing/Rushing EPA
  real home_pass_epa_avg[num_games];                         // home team passing EPA per play
  real away_pass_epa_avg[num_games];                         // away team passing EPA per play
  real home_rush_epa_avg[num_games];                         // home team rushing EPA per play
  real away_rush_epa_avg[num_games];                         // away team rushing EPA per play
  
  real home_def_pass_epa_allowed_avg[num_games];             // home team pass defense EPA allowed
  real away_def_pass_epa_allowed_avg[num_games];             // away team pass defense EPA allowed
  real home_def_rush_epa_allowed_avg[num_games];             // home team rush defense EPA allowed
  real away_def_rush_epa_allowed_avg[num_games];             // away team rush defense EPA allowed
  
  // Special teams
  real home_st_epa_avg[num_games];                           // home team special teams EPA
  real away_st_epa_avg[num_games];                           // away team special teams EPA
  
  // Directionality/momentum (rolling averages)
  real home_epa_trend[num_games];                            // home team EPA trend
  real away_epa_trend[num_games];                            // away team EPA trend
  real home_def_trend[num_games];                            // home team defensive trend
  real away_def_trend[num_games];                            // away team defensive trend
}

parameters {
  // Team strength parameters
  matrix[num_clubs, num_seasons] theta;                       // base team strength
  real<lower=0> sigma_team_strength;                         // team strength sd
  real<lower=0> sigma_season;                                // season variation
  
  // Rest advantage parameters (existing)
  real alpha_home;                                           // home field advantage
  real alpha_bye;                                            // bye week advantage
  real alpha_mnf;                                            // monday night advantage
  real alpha_mini;                                           // mini bye advantage
  
  // EPA coefficients
  real beta_off_epa;                                         // offensive EPA coefficient
  real beta_def_epa;                                         // defensive EPA coefficient
  real beta_success_off;                                     // offensive success rate coefficient
  real beta_success_def;                                     // defensive success rate coefficient
  
  // Passing/Rushing coefficients
  real beta_pass_epa;                                        // passing EPA coefficient
  real beta_rush_epa;                                        // rushing EPA coefficient
  real beta_def_pass_epa;                                    // pass defense coefficient
  real beta_def_rush_epa;                                    // rush defense coefficient
  
  // Special teams coefficient
  real beta_st_epa;                                          // special teams EPA coefficient
  
  // Momentum/directionality coefficients
  real beta_momentum_off;                                    // offensive momentum coefficient
  real beta_momentum_def;                                    // defensive momentum coefficient
  
  // Model variance
  real<lower=0> sigma;                                       // residual standard deviation
}

transformed parameters {
  vector[num_games] mu;                                      // expected outcome for each game
  
  for (g in 1:num_games) {
    // Base team strengths
    real home_strength = theta[home_team_code[g], season[g]];
    real away_strength = theta[away_team_code[g], season[g]];
    
    // EPA differentials
    real epa_off_diff = home_off_epa_avg[g] - away_off_epa_avg[g];
    real epa_def_diff = away_def_epa_allowed_avg[g] - home_def_epa_allowed_avg[g]; // Lower allowed is better
    
    // Success rate differentials
    real success_off_diff = home_off_success_rate[g] - away_off_success_rate[g];
    real success_def_diff = away_def_success_rate_allowed[g] - home_def_success_rate_allowed[g];
    
    // Passing/Rushing differentials
    real pass_epa_diff = home_pass_epa_avg[g] - away_pass_epa_avg[g];
    real rush_epa_diff = home_rush_epa_avg[g] - away_rush_epa_avg[g];
    real def_pass_epa_diff = away_def_pass_epa_allowed_avg[g] - home_def_pass_epa_allowed_avg[g];
    real def_rush_epa_diff = away_def_rush_epa_allowed_avg[g] - home_def_rush_epa_allowed_avg[g];
    
    // Special teams differential
    real st_epa_diff = home_st_epa_avg[g] - away_st_epa_avg[g];
    
    // Momentum differentials
    real momentum_off_diff = home_epa_trend[g] - away_epa_trend[g];
    real momentum_def_diff = away_def_trend[g] - home_def_trend[g];
    
    // Combine all factors
    mu[g] = home_strength - away_strength +
            alpha_home * h_adv[g] +
            alpha_bye * bye[g] +
            alpha_mnf * mnf[g] +
            alpha_mini * mini[g] +
            beta_off_epa * epa_off_diff +
            beta_def_epa * epa_def_diff +
            beta_success_off * success_off_diff +
            beta_success_def * success_def_diff +
            beta_pass_epa * pass_epa_diff +
            beta_rush_epa * rush_epa_diff +
            beta_def_pass_epa * def_pass_epa_diff +
            beta_def_rush_epa * def_rush_epa_diff +
            beta_st_epa * st_epa_diff +
            beta_momentum_off * momentum_off_diff +
            beta_momentum_def * momentum_def_diff;
  }
}

model {
  // Priors
  sigma_team_strength ~ normal(0, 1);
  sigma_season ~ normal(0, 0.5);
  sigma ~ normal(0, 10);
  
  // Rest advantage priors
  alpha_home ~ normal(2.5, 1);        // Home field advantage ~2.5 points
  alpha_bye ~ normal(0, 2);           // Bye week advantage
  alpha_mnf ~ normal(0, 1);           // MNF advantage
  alpha_mini ~ normal(0, 1);          // Mini bye advantage
  
  // EPA coefficient priors
  beta_off_epa ~ normal(0, 10);       // Offensive EPA impact
  beta_def_epa ~ normal(0, 10);       // Defensive EPA impact
  beta_success_off ~ normal(0, 5);    // Success rate impact
  beta_success_def ~ normal(0, 5);
  
  // Passing/Rushing priors
  beta_pass_epa ~ normal(0, 8);
  beta_rush_epa ~ normal(0, 8);
  beta_def_pass_epa ~ normal(0, 8);
  beta_def_rush_epa ~ normal(0, 8);
  
  // Special teams prior
  beta_st_epa ~ normal(0, 5);
  
  // Momentum priors
  beta_momentum_off ~ normal(0, 3);
  beta_momentum_def ~ normal(0, 3);
  
  // Team strength priors
  for (j in 1:num_clubs) {
    theta[j, 1] ~ normal(0, sigma_team_strength);
    for (s in 2:num_seasons) {
      theta[j, s] ~ normal(theta[j, s-1], sigma_season);
    }
  }
  
  // Likelihood
  outcome ~ normal(mu, sigma);
}

generated quantities {
  vector[num_games] log_lik;           // For LOO-CV
  vector[num_games] y_rep;             // Posterior predictive checks
  
  for (g in 1:num_games) {
    log_lik[g] = normal_lpdf(outcome[g] | mu[g], sigma);
    y_rep[g] = normal_rng(mu[g], sigma);
  }
}
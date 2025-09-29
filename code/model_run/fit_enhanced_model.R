library(rstan)
library(dplyr)
library(readr)
library(purrr)

# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Model parameters
params <- list(
  seed = 73097,
  chains = 4,
  iter = 3000,
  warmup = 1000,
  adapt_delta = 0.95,
  outcomes = c("point_diff", "spread_line")
)

# Function to prepare data for Stan model
prepare_stan_data <- function(df_enhanced) {
  
  # Handle missing values by filling with reasonable defaults
  df_enhanced <- df_enhanced %>%
    mutate(
      # EPA metrics - fill with 0 if missing
      across(contains("epa"), ~ifelse(is.na(.), 0, .)),
      # Success rates - fill with 0.5 if missing  
      across(contains("success"), ~ifelse(is.na(.), 0.5, .)),
      # Trends - fill with 0 if missing
      across(contains("trend"), ~ifelse(is.na(.), 0, .))
    )
  
  # Create team ID mapping
  teams <- unique(c(df_enhanced$home_team, df_enhanced$away_team))
  team_mapping <- setNames(seq_along(teams), teams)
  
  # Prepare Stan data
  stan_data <- list(
    num_clubs = length(teams),
    num_games = nrow(df_enhanced),
    num_seasons = length(unique(df_enhanced$season)),
    
    # Team codes
    home_team_code = team_mapping[df_enhanced$home_team],
    away_team_code = team_mapping[df_enhanced$away_team],
    season = df_enhanced$season - min(df_enhanced$season, na.rm = TRUE) + 1,
    
    # Rest advantages (from original model)
    h_adv = ifelse(is.na(df_enhanced$true_home), 1, df_enhanced$true_home),
    bye = ifelse(is.na(df_enhanced$bye), 0, df_enhanced$bye),
    mnf = ifelse(is.na(df_enhanced$mnf), 0, df_enhanced$mnf),
    mini = ifelse(is.na(df_enhanced$mini), 0, df_enhanced$mini),
    
    # EPA metrics
    home_off_epa_avg = df_enhanced$home_off_epa_avg,
    away_off_epa_avg = df_enhanced$away_off_epa_avg,
    home_def_epa_allowed_avg = df_enhanced$home_def_epa_allowed_avg,
    away_def_epa_allowed_avg = df_enhanced$away_def_epa_allowed_avg,
    
    # Success rates
    home_off_success_rate = df_enhanced$home_off_success_rate,
    away_off_success_rate = df_enhanced$away_off_success_rate,
    home_def_success_rate_allowed = df_enhanced$home_def_success_rate_allowed,
    away_def_success_rate_allowed = df_enhanced$away_def_success_rate_allowed,
    
    # Passing/Rushing EPA
    home_pass_epa_avg = df_enhanced$home_off_pass_epa_avg,
    away_pass_epa_avg = df_enhanced$away_off_pass_epa_avg,
    home_rush_epa_avg = df_enhanced$home_off_rush_epa_avg,
    away_rush_epa_avg = df_enhanced$away_off_rush_epa_avg,
    
    home_def_pass_epa_allowed_avg = df_enhanced$home_def_pass_epa_allowed_avg,
    away_def_pass_epa_allowed_avg = df_enhanced$away_def_pass_epa_allowed_avg,
    home_def_rush_epa_allowed_avg = df_enhanced$home_def_rush_epa_allowed_avg,
    away_def_rush_epa_allowed_avg = df_enhanced$away_def_rush_epa_allowed_avg,
    
    # Special teams (use 0 if not available)
    home_st_epa_avg = ifelse("home_st_epa_avg" %in% names(df_enhanced), 
                             df_enhanced$home_st_epa_avg, 
                             rep(0, nrow(df_enhanced))),
    away_st_epa_avg = ifelse("away_st_epa_avg" %in% names(df_enhanced), 
                             df_enhanced$away_st_epa_avg, 
                             rep(0, nrow(df_enhanced))),
    
    # Directionality/momentum
    home_epa_trend = ifelse("home_epa_trend" %in% names(df_enhanced), 
                            df_enhanced$home_epa_trend, 
                            rep(0, nrow(df_enhanced))),
    away_epa_trend = ifelse("away_epa_trend" %in% names(df_enhanced), 
                            df_enhanced$away_epa_trend, 
                            rep(0, nrow(df_enhanced))),
    home_def_trend = ifelse("home_def_trend" %in% names(df_enhanced), 
                            df_enhanced$home_def_trend, 
                            rep(0, nrow(df_enhanced))),
    away_def_trend = ifelse("away_def_trend" %in% names(df_enhanced), 
                            df_enhanced$away_def_trend, 
                            rep(0, nrow(df_enhanced)))
  )
  
  return(list(stan_data = stan_data, team_mapping = team_mapping))
}

# Function to fit the enhanced model
fit_enhanced_model <- function(outcome_var = "point_diff") {
  
  cat("Loading enhanced dataset...\n")
  
  # Try to load enhanced data, fall back to creating it if it doesn't exist
  if (file.exists("data/games_enhanced.csv")) {
    df_enhanced <- read_csv("data/games_enhanced.csv")
  } else {
    cat("Enhanced dataset not found. Creating it...\n")
    source("code/data_enhancement/nflreadr_data_pipeline.R")
    df_enhanced <- create_enhanced_dataset()
  }
  
  # Prepare outcome variable
  if (outcome_var == "point_diff") {
    df_enhanced$outcome <- df_enhanced$home_score - df_enhanced$away_score
  } else if (outcome_var == "spread_line") {
    df_enhanced$outcome <- df_enhanced$spread_line
  } else {
    stop("outcome_var must be 'point_diff' or 'spread_line'")
  }
  
  # Remove games with missing outcome
  df_enhanced <- df_enhanced %>% filter(!is.na(outcome))
  
  cat(paste("Fitting model for", outcome_var, "with", nrow(df_enhanced), "games\n"))
  
  # Prepare Stan data
  prepared_data <- prepare_stan_data(df_enhanced)
  stan_data <- prepared_data$stan_data
  stan_data$outcome <- df_enhanced$outcome
  
  cat("Fitting enhanced Stan model...\n")
  
  # Fit the model
  model <- stan(
    file = "stan/enhanced_prediction_model.stan",
    data = stan_data,
    seed = params$seed,
    chains = params$chains,
    iter = params$iter,
    warmup = params$warmup,
    control = list(adapt_delta = params$adapt_delta),
    pars = c("mu"),  # Exclude mu from output to save space
    include = FALSE
  )
  
  # Save the model
  output_file <- paste0("stan_results/enhanced_model_", outcome_var, ".rds")
  write_rds(model, output_file)
  cat(paste("Model saved to", output_file, "\n"))
  
  # Save team mapping for predictions
  write_rds(prepared_data$team_mapping, "stan_results/team_mapping.rds")
  
  return(model)
}

# Function to validate model performance
validate_model <- function(model, outcome_var = "point_diff") {
  
  # Extract posterior samples
  posterior <- extract(model)
  
  # Calculate model summary statistics
  cat("\n=== Model Summary ===\n")
  
  # Print key coefficients
  cat("\nKey Coefficients (mean ± sd):\n")
  cat(sprintf("Home Field Advantage: %.2f ± %.2f\n", 
              mean(posterior$alpha_home), sd(posterior$alpha_home)))
  cat(sprintf("Bye Week Advantage: %.2f ± %.2f\n", 
              mean(posterior$alpha_bye), sd(posterior$alpha_bye)))
  cat(sprintf("Offensive EPA Coefficient: %.2f ± %.2f\n", 
              mean(posterior$beta_off_epa), sd(posterior$beta_off_epa)))
  cat(sprintf("Defensive EPA Coefficient: %.2f ± %.2f\n", 
              mean(posterior$beta_def_epa), sd(posterior$beta_def_epa)))
  cat(sprintf("Momentum Coefficient: %.2f ± %.2f\n", 
              mean(posterior$beta_momentum_off), sd(posterior$beta_momentum_off)))
  
  # Model fit statistics
  cat(sprintf("\nModel Standard Deviation: %.2f ± %.2f\n", 
              mean(posterior$sigma), sd(posterior$sigma)))
  
  return(invisible(model))
}

# Main execution
main <- function() {
  
  # Create results directory if it doesn't exist
  if (!dir.exists("stan_results")) {
    dir.create("stan_results")
  }
  
  # Fit models for both outcomes
  for (outcome in params$outcomes) {
    cat(paste("\n", paste(rep("=", 50), collapse=""), "\n"))
    cat(paste("Fitting model for outcome:", outcome, "\n"))
    cat(paste(paste(rep("=", 50), collapse=""), "\n"))
    
    model <- fit_enhanced_model(outcome)
    validate_model(model, outcome)
    
    # Clean up memory
    rm(model)
    gc()
  }
  
  cat("\n✓ All models fitted successfully!\n")
  cat("Models saved in stan_results/ directory\n")
  cat("Run the prediction interface with: shiny::runApp('code/prediction_engine/nfl_predictor.R')\n")
}

# Run if script is executed directly
if (!interactive()) {
  main()
}
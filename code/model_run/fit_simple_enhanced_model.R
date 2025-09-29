library(rstan)
library(dplyr)
library(readr)

# Simplified model fitting for demonstration
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load the enhanced data
cat("Loading enhanced dataset...\n")
enhanced_data <- read_csv("data/games_enhanced.csv", show_col_types = FALSE)

# Create a simplified version for demonstration
simplified_data <- enhanced_data %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    point_diff = home_score - away_score,
    # Fill missing EPA values with 0
    home_off_epa_avg = ifelse(is.na(home_off_epa_avg), 0, home_off_epa_avg),
    away_off_epa_avg = ifelse(is.na(away_off_epa_avg), 0, away_off_epa_avg),
    home_def_epa_allowed_avg = ifelse(is.na(home_def_epa_allowed_avg), 0, home_def_epa_allowed_avg),
    away_def_epa_allowed_avg = ifelse(is.na(away_def_epa_allowed_avg), 0, away_def_epa_allowed_avg),
    home_off_success_rate = ifelse(is.na(home_off_success_rate), 0.5, home_off_success_rate),
    away_off_success_rate = ifelse(is.na(away_off_success_rate), 0.5, away_off_success_rate)
  ) %>%
  head(100)  # Use first 100 games for quick demonstration

cat("Using", nrow(simplified_data), "games for demonstration\n")

# Create simple Stan model for demonstration
simple_model_code <- "
data {
  int<lower=1> N;
  real point_diff[N];
  real home_epa_diff[N];
  real def_epa_diff[N];
}
parameters {
  real alpha;
  real beta_epa;
  real beta_def;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(2.5, 1);
  beta_epa ~ normal(0, 10);
  beta_def ~ normal(0, 10);
  sigma ~ normal(0, 10);
  
  for (i in 1:N) {
    point_diff[i] ~ normal(alpha + beta_epa * home_epa_diff[i] + beta_def * def_epa_diff[i], sigma);
  }
}
"

# Prepare data
stan_data <- list(
  N = nrow(simplified_data),
  point_diff = simplified_data$point_diff,
  home_epa_diff = simplified_data$home_off_epa_avg - simplified_data$away_off_epa_avg,
  def_epa_diff = simplified_data$away_def_epa_allowed_avg - simplified_data$home_def_epa_allowed_avg
)

cat("Fitting simplified model...\n")

# Fit model
model <- stan(
  model_code = simple_model_code,
  data = stan_data,
  chains = 2,
  iter = 1000,
  warmup = 500
)

# Print results
print(model)

# Save model
write_rds(model, "stan_results/simple_enhanced_model.rds")

cat("✓ Simple enhanced model fitted successfully!\n")
cat("Model saved to stan_results/simple_enhanced_model.rds\n")

# Extract and print key results
posterior <- extract(model)
cat("\nKey Results:\n")
cat(sprintf("Home Field + EPA Advantage: %.2f ± %.2f points\n", 
            mean(posterior$alpha), sd(posterior$alpha)))
cat(sprintf("Offensive EPA Impact: %.2f ± %.2f points per EPA\n", 
            mean(posterior$beta_epa), sd(posterior$beta_epa)))
cat(sprintf("Defensive EPA Impact: %.2f ± %.2f points per EPA\n", 
            mean(posterior$beta_def), sd(posterior$beta_def)))
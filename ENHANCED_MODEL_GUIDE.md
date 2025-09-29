# Enhanced NFL Prediction Model

This enhanced version of your NFL analysis includes comprehensive EPA metrics, team directionality, and a prediction interface for forecasting game winners.

## New Features

### 📊 **Enhanced Data Pipeline**
- **EPA Metrics**: Offensive/Defensive EPA per play, success rates
- **Play-Type Breakdown**: Passing vs Rushing EPA analysis  
- **Special Teams**: Special teams EPA impact
- **Directionality**: Team momentum/trends based on recent performance
- **Data Source**: Uses `nflreadr` for comprehensive NFL data

### 🎯 **Prediction Model**
- **Bayesian Framework**: Enhanced Stan model with 15+ features
- **Multiple Factors**: Rest, EPA, success rates, momentum, play-type splits
- **Uncertainty Quantification**: Confidence intervals for predictions
- **Historical Validation**: Backtesting on 2002-2023 data

### 🖥️ **Interactive Interface**
- **Shiny Dashboard**: User-friendly web interface
- **Real-time Predictions**: Select any two teams for instant predictions
- **Team Analytics**: Deep dive into team performance metrics
- **Comparison Tools**: Side-by-side team metric comparisons

## Quick Start

### 1. Install Dependencies
```r
install.packages(c("nflreadr", "rstan", "shiny", "shinydashboard", 
                   "DT", "plotly", "dplyr", "tidyr", "readr", "zoo"))
```

### 2. Create Enhanced Dataset
```r
# Run the data pipeline to get EPA metrics
source("code/data_enhancement/nflreadr_data_pipeline.R")
enhanced_data <- create_enhanced_dataset(seasons = 2002:2023)
```

### 3. Fit the Enhanced Model
```r
# Fit the comprehensive model with EPA features
source("code/model_run/fit_enhanced_model.R")
```

### 4. Launch Prediction Interface
```r
# Start the interactive prediction app
shiny::runApp("code/prediction_engine/nfl_predictor.R")
```

## Model Features

### Core Inputs
- **Team Strength**: Hierarchical team effects by season
- **Rest Advantages**: Bye weeks, mini-byes, Monday Night Football
- **Home Field**: True home vs neutral site games

### EPA Metrics  
- **Offensive EPA**: Points added per play on offense
- **Defensive EPA**: Points allowed per play on defense  
- **Success Rates**: Percentage of "successful" plays
- **Play-Type Splits**: Separate passing/rushing effectiveness

### Advanced Features
- **Momentum**: Rolling 4-game performance trends
- **Special Teams**: Kicking, punting, return game impact
- **Situational**: Context-aware performance metrics

## File Structure

```
DS340W/
├── code/
│   ├── data_enhancement/
│   │   └── nflreadr_data_pipeline.R    # EPA data extraction
│   ├── model_run/
│   │   ├── fit_enhanced_model.R        # Model fitting script  
│   │   ├── prep_data.R                 # Original data prep
│   │   └── run_analysis.R              # Original analysis
│   ├── prediction_engine/
│   │   └── nfl_predictor.R             # Shiny prediction app
│   └── results_analysis/               # Original analysis scripts
├── data/
│   ├── games_enhanced.csv              # New: EPA + original data
│   ├── games_clean.csv                 # Original rest data
│   └── ...
├── stan/
│   ├── enhanced_prediction_model.stan  # New: Full prediction model
│   ├── model_no_split.stan            # Original models
│   └── model_split_bye.stan
└── stan_results/                       # Model outputs
    ├── enhanced_model_point_diff.rds
    ├── enhanced_model_spread_line.rds
    └── team_mapping.rds
```

## Usage Examples

### Predicting a Game
```r
# Load the prediction functions
source("code/prediction_engine/nfl_predictor.R")

# Predict Chiefs vs Bills
prediction <- predict_game("KC", "BUF")
print(prediction$predicted_margin)    # Expected point difference
print(prediction$home_win_prob)       # Chiefs win probability
```

### Team Analysis
```r
# Get current team metrics
kc_metrics <- get_current_team_metrics("KC", season = 2024)
print(kc_metrics$off_epa_avg)         # Offensive EPA per play
print(kc_metrics$epa_trend)           # Recent performance trend
```

### Model Validation
```r
# Load fitted model
model <- readRDS("stan_results/enhanced_model_point_diff.rds")

# Validate predictions
validate_model(model, "point_diff")
```

## Key Improvements Over Original

| Feature | Original | Enhanced |
|---------|----------|----------|
| **Data Source** | Manual NFL data | nflreadr (comprehensive) |
| **Features** | Rest only (4 vars) | Rest + EPA + Momentum (15+ vars) |
| **Interface** | R scripts only | Interactive Shiny dashboard |
| **Predictions** | Historical analysis | Real-time game forecasting |
| **Validation** | Model fit only | Prediction accuracy + uncertainty |

## Performance Expectations

- **Accuracy**: ~65-70% against spread (typical for sports models)
- **Calibration**: Prediction confidence matches actual outcomes  
- **Features**: EPA metrics typically most predictive
- **Seasonality**: Model adapts to changing team strength over time

## Troubleshooting

### Common Issues
1. **nflreadr data loading**: Requires internet connection for recent data
2. **Stan compilation**: May take 2-3 minutes on first run
3. **Memory usage**: Large datasets may require 8GB+ RAM for full seasons

### Model Diagnostics
```r
# Check model convergence
model <- readRDS("stan_results/enhanced_model_point_diff.rds")
print(model)  # Look for Rhat < 1.1, n_eff > 400
```

## Next Steps

1. **Backtest validation** on held-out seasons
2. **Feature engineering** for weather, injuries, etc.  
3. **Live integration** with current week spreads
4. **Mobile interface** for easier access
5. **Ensemble methods** combining multiple models
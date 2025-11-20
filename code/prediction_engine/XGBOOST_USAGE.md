# XGBoost NFL Predictor - Usage Guide

## Overview
The gradient_boosting_predictor now uses XGBoost machine learning and 2024-2025 season data, similar to nfl_2025_predictor.

## Quick Start

### 1. Train the Model (One-time setup)
```r
source("code/prediction_engine/gradient_boosting_predictor.R")

# Train on historical games (2020-2024)
model <- train_xgboost_from_history()

# Model is automatically saved to: models/xgboost_nfl_model.rds
```

### 2. Make Predictions
```r
source("code/prediction_engine/gradient_boosting_predictor.R")

# Predict a game (automatically loads 2025 data and trained model)
result <- predict_xgboost_game("KC", "BUF")

# View results
cat("Winner:", result$predicted_winner, "\n")
cat("Score:", result$home_score, "-", result$away_score, "\n")
cat("Margin:", result$predicted_margin, "points\n")
cat("Win Probability:", result$home_win_prob, "\n")
```

## Features

### Data Sources
- **Training Data**: 1,123 games from 2020-2024 seasons (games_enhanced.csv)
- **Prediction Data**: 2024-2025 team metrics (team_metrics_2025.csv)
- Uses same EPA-based metrics as nfl_2025_predictor

### Model Features (13 total)
1. Offensive EPA differential
2. Defensive EPA differential
3. Pass EPA differential
4. Rush EPA differential
5. Pass defense differential
6. Rush defense differential
7. Offensive success rate differential
8. Defensive success rate differential
9. Pass success differential
10. Rush success differential
11. Offensive trend differential
12. Defensive trend differential
13. Home field advantage

### Most Important Features (by XGBoost)
1. **Offensive EPA diff** (85.7% importance)
2. **Defensive EPA diff** (12.8% importance)
3. Success rates and trends (remaining ~1.5%)

## Example Predictions (Nov 2025)

| Home | Away | Predicted Winner | Score | Margin | Confidence |
|------|------|------------------|-------|--------|------------|
| KC | BUF | BUF | 20-27 | -6.0 | Medium |
| DET | SF | DET | 26-21 | +5.1 | Medium |
| CHI | GB | GB | 19-28 | -8.4 | Medium |

## Differences from nfl_2025_predictor

| Feature | nfl_2025_predictor | gradient_boosting_predictor |
|---------|-------------------|----------------------------|
| Method | Hand-tuned formula | Machine learning (XGBoost) |
| Coefficients | Hardcoded | Learned from data |
| Training | None | 1,123 historical games |
| Adaptability | Manual updates | Retrains automatically |
| Interpretability | High | Medium (feature importance) |

## Functions

### Main Functions
- `train_xgboost_from_history()` - Train model on historical data
- `predict_xgboost_game(home, away)` - Predict using 2025 data
- `predict_ensemble_game(home, away)` - Combine Bayesian + XGBoost

### Data Functions
- `load_xgboost_prediction_data()` - Load 2024-2025 metrics
- `load_training_data()` - Load historical games

### Model Management
- `save_xgboost_model(model)` - Save trained model
- `load_xgboost_model()` - Load saved model

## Model Performance

- **Training RMSE**: 1.25 points (after 200 rounds)
- **Features**: 13 differentials + home advantage
- **Regularization**: L1 (0.1) + L2 (1.5)
- **Overfitting Prevention**: Early stopping, max depth 5, subsampling

## Tips

1. **Retrain periodically**: As more 2025 games are played, retrain with updated data
2. **Compare models**: Use both nfl_2025_predictor and XGBoost to validate predictions
3. **Ensemble predictions**: Use `predict_ensemble_game()` to combine both approaches
4. **Check confidence**: Higher confidence predictions are more reliable

## Troubleshooting

**Model not found**: Run `train_xgboost_from_history()` first

**Team not found**: Verify team code (e.g., "KC", "BUF", "SF")

**Poor predictions**: Model may need retraining with updated data

**NA values**: Ensure team_metrics_2025.csv has all required columns

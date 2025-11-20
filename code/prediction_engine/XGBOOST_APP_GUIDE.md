# XGBoost Predictor Interface - User Guide

## 🚀 Quick Start

### Option 1: Using the Launch Script (Recommended)
```r
# From the prediction_engine directory
Rscript launch_xgboost_app.R
```

### Option 2: Using Shiny directly
```r
library(shiny)
runApp("xgboost_predictor_app.R")
```

### Option 3: From R Console
```r
source("xgboost_predictor_app.R")
# App will launch automatically
```

## 📱 Interface Overview

The XGBoost Predictor interface has 5 main tabs:

### 1. 🎯 Game Prediction
- **Select teams**: Choose home and away teams from dropdowns
- **Get prediction**: Click "Predict with XGBoost" button
- **View results**: See predicted winner, score, margin, and win probability
- **Feature impacts**: See which metrics drove the prediction
- **Team comparison**: Compare full stats between teams

**Example:**
- Select KC (home) vs BUF (away)
- Click Predict
- See: BUF predicted to win 27-20 (6 pt margin, 66.7% win probability)

### 2. 🧠 Model Insights
- **Model architecture**: See XGBoost configuration and parameters
- **Feature importance chart**: Interactive visualization of which features matter most
- **Feature explanations**: Detailed explanations of top predictive features

**Key Insights:**
- Offensive EPA differential = 85.7% of prediction power
- Defensive EPA differential = 12.8%
- All other features = 1.5% combined

### 3. 🏆 Team Rankings
- **Full team rankings**: All 32 teams ranked by Offensive EPA
- **Team metrics**: See EPA stats for all teams
- **Color coding**: Green (top 8), Yellow (9-16), Orange (17-24), Red (25-32)

### 4. 📊 Model Performance
- **Training statistics**: See model training info
- **Performance metrics**: RMSE, training samples, features used
- **Retrain button**: Retrain the model with latest data
- **Model parameters**: View XGBoost hyperparameters

**When to Retrain:**
- After updating team_metrics_2025.csv with new data
- When historical games are updated
- If predictions seem off

### 5. ⚖️ Compare Models
- **Side-by-side comparison**: XGBoost vs Bayesian model
- **Agreement analysis**: See how much models agree
- **Interpretation**: Guidance on which prediction to trust

**Agreement Levels:**
- 🟢 Strong Agreement (<3 pt diff): High confidence
- 🟡 Moderate Agreement (3-7 pt diff): Consider both
- 🔴 Weak Agreement (>7 pt diff): Mixed signals

## 📊 Understanding the Predictions

### Score Display
```
🏆 WINNER: BUF
📊 SCORE: 20 - 27
🎯 WIN PROBABILITY: 66.7%
Margin: 6 points
Confidence: Medium
```

### Confidence Levels
- **High**: Predicted margin > 10 points (blowout expected)
- **Medium**: Predicted margin 5-10 points (clear favorite)
- **Low**: Predicted margin < 5 points (toss-up game)

### Feature Impact Example
```
OFF EPA DIFF
Impact: -0.126 - favors Away
```
This means the away team has 0.126 higher offensive EPA per play, giving them an advantage.

## 🎨 Visual Features

### Color Coding
- **Purple gradient boxes**: XGBoost predictions
- **Blue boxes**: General information
- **Yellow boxes**: Warnings or comparisons
- **Green checkmarks**: Strong agreement
- **Red warnings**: Disagreements

### Interactive Charts
- **Feature importance**: Hover to see exact values
- **Rankings table**: Sortable and searchable
- **Team comparison**: Side-by-side metrics

## 💡 Tips for Best Results

### 1. Check Model Status
Before making predictions, verify:
- ✅ Model is trained (check Model Performance tab)
- ✅ Team metrics are loaded (should show 32 teams)
- ✅ Training date is recent

### 2. Compare with Bayesian Model
For important predictions:
1. Use Compare Models tab
2. Run both XGBoost and Bayesian predictions
3. If they agree strongly → high confidence
4. If they disagree → investigate team metrics

### 3. Understand Feature Impacts
Look at which features drove the prediction:
- Large off_epa_diff → Offense mismatch
- Large def_epa_diff → Defense mismatch
- Trend differences → Recent form matters

### 4. Context Matters
The model doesn't know about:
- ❌ Injuries (unless manually integrated)
- ❌ Weather conditions
- ❌ Rivalry factors
- ❌ Coaching changes
- ❌ Player trades

Use your judgment to adjust predictions based on these factors.

## 🔧 Troubleshooting

### "No model loaded" Error
**Solution**: Go to Model Performance tab → Click "Retrain XGBoost Model"

### App won't launch
**Check:**
```r
# Verify required packages
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "readr", "xgboost"))

# Verify files exist
file.exists("xgboost_predictor_app.R")
file.exists("gradient_boosting_predictor.R")
file.exists("data/team_metrics_2025.csv")
```

### Predictions seem wrong
**Try:**
1. Retrain the model
2. Check team_metrics_2025.csv for correct data
3. Compare with Bayesian model
4. Verify team codes (e.g., "LA" not "LAR" for Rams)

### Teams not found
**Common team code errors:**
- ✅ LA (correct) ❌ LAR (wrong) - Los Angeles Rams
- ✅ LAC (correct) - Los Angeles Chargers
- ✅ WAS (correct) - Washington Commanders

## 📈 Advanced Usage

### Batch Predictions
Want to predict multiple games at once? Use the console:
```r
source("gradient_boosting_predictor.R")

games <- data.frame(
  home = c("KC", "DET", "PHI"),
  away = c("BUF", "SF", "DAL")
)

results <- lapply(1:nrow(games), function(i) {
  predict_xgboost_game(games$home[i], games$away[i])
})
```

### Custom Analysis
Export feature importance:
```r
model <- load_xgboost_model()
importance <- model$importance
write.csv(importance, "feature_importance.csv")
```

### Integration with Other Tools
The app can be integrated with:
- nfl_2025_predictor for comparison
- injury_system for injury-adjusted predictions
- Custom data pipelines for live updates

## 🎯 Best Practices

1. **Train weekly**: Retrain model as new data becomes available
2. **Compare models**: Use both XGBoost and Bayesian for important games
3. **Check feature impacts**: Understand WHY the model predicts what it does
4. **Monitor agreement**: High agreement = more confidence
5. **Update data**: Keep team_metrics_2025.csv current

## 📞 Quick Reference

| Task | Action |
|------|--------|
| Make prediction | Game Prediction tab → Select teams → Predict |
| See why | Feature Impact section below prediction |
| Compare teams | Team Metrics Comparison table |
| View rankings | Team Rankings tab |
| Retrain model | Model Performance tab → Retrain button |
| Compare models | Compare Models tab → Select teams → Compare |

## 🌟 Pro Tips

- **Monday morning**: Good time to retrain after weekend games
- **Playoff predictions**: Compare models for validation
- **Toss-up games**: Look at feature impacts to understand thin margins
- **Blowouts**: High confidence predictions (>10 pt margin) are reliable
- **Divisional games**: Historical models may underpredict competitiveness

---

**Enjoy using the XGBoost NFL Predictor! 🏈🤖**

NEXT-WEEK GASOLINE FORECASTS

Open this folder in RStudio and run:
source("run_forecasts.R")

Only base R is required. Outputs and the standalone HTML are regenerated.
There are no API keys or network requests. Input CSVs are copied from the
existing supplied project and stock EDA; original files are not modified.

Target: next Monday's Binghamton regional gasoline price change. Add the
forecast change to the origin Monday's price to obtain a price forecast.

Validation targets: 2024. Choose settings by MAE, with RMSE as tie-break.
Test targets: common complete weeks from 2025 onward (actual start 2025-02-10
after the local missing week and common 4-lag history requirements).
Coefficients refit weekly with observed outcomes only; settings stay frozen.

IMPORTANT ASSUMPTIONS
Current Monday local price is assumed available. Spot weekly inputs are
delayed until 10 days after their Friday period end. This is a conservative
assumption, not a verified release timestamp for every historical row.
Stock returns use the preceding week's adjusted close. Historical adjustments
and data revisions are not reconstructed as of each forecast origin.
Full-period EDA was already viewed, so this is exploratory validation.
No present-day forecast is produced: the supplied local snapshot ends Sep 2026.

MODEL SEARCH
AR orders 1/2/4; ARDL local orders 1/2/4 and spot orders 1/2, WTI or Brent
plus RBOB. Expanding vs trailing 260 weeks, common complete samples.
Fix best ARDL, then try five single stocks, two group-return signals or all
five stocks, with one/two available return observations. Select using 2024 only.
No futures, seasonal learner, ensemble or neural network is fitted yet.

READ RESULTS
forecast_report.html: method, metrics, plots and caveats
output/validation_ar_ardl.csv and validation_stocks.csv: all candidate scores
output/selected_models.csv: selected input names and window
output/test_predictions.csv: dated out-of-sample forecasts and outcomes
output/test_metrics.csv: MAE/RMSE/bias on identical dates, dollars/gallon
output/test_metrics_by_year.csv: consistency check across test years
output/forecast_design.csv: original dates, targets, lagged inputs and missingness
output/excluded_origins.csv: excluded rows, no imputation

Do not retune on the test results. A statistically significant improvement,
certified real-time simulation, and future performance are not established.

# Run from this folder: source("run_forecasts.R")
# Base R only. One-week-ahead forecasts; raw snapshots are preserved.
dir.create("output", showWarnings = FALSE)
shift <- function(x, k) if (k == 0) x else c(rep(NA, k), head(x, -k))
local <- read.csv("data/local_spot.csv")
stocks <- read.csv("data/stock_returns.csv")
local$date <- as.Date(local$date)
stocks$week_end <- as.Date(stocks$week_end)
stopifnot(all(diff(local$date) == 7), !anyDuplicated(local$date))

# Each row is a forecast origin (Monday), not the target date.
d <- data.frame(origin = local$date, current_price = local$local_price)
d$target_date <- d$origin + 7
d$actual_price <- c(tail(d$current_price, -1), NA)
d$y <- d$actual_price - d$current_price
local_change <- c(NA, diff(d$current_price))
for (k in 0:3) d[[paste0("local", k)]] <- shift(local_change, k)

# local_spot.csv pairs Monday with Friday 3 days earlier.
# Shift spot changes one further week: latest spot period ends origin - 10 days.
for (market in c("wti", "brent", "rbob")) {
  change <- c(NA, diff(local[[market]]))
  for (k in 0:1) d[[paste0(market, k)]] <- shift(change, k + 1)
}
d$spot_week_end <- d$origin - 10
d$stock_week_end <- d$origin - 3
index <- match(d$stock_week_end, stocks$week_end)
equities <- c("XOM", "CVX", "MPC", "VLO", "PSX", "integrated_group", "refining_group")
for (stock in equities) {
  x <- stocks[[stock]][index]
  for (k in 0:1) d[[paste0(stock, k)]] <- shift(x, k)
}

# Preserve calendar before lagging; no imputation. A common complete sample
# supports identical forecast dates and training rows across comparisons.
required <- c("y", paste0("local", 0:3), paste0("wti", 0:1),
              paste0("brent", 0:1), paste0("rbob", 0:1),
              unlist(lapply(equities, function(x) paste0(x, 0:1))))
d$complete <- complete.cases(d[, required])
write.csv(d, "output/forecast_design.csv", row.names = FALSE, na = "")
missing <- d[!d$complete, c("origin", "target_date", "complete")]
write.csv(missing, "output/excluded_origins.csv", row.names = FALSE)
validation <- which(d$complete & d$target_date >= as.Date("2024-01-01") &
                     d$target_date <= as.Date("2024-12-31"))
test <- which(d$complete & d$target_date >= as.Date("2025-01-01"))
stopifnot(length(validation) > 30, length(test) > 50,
          max(d$target_date[validation]) < min(d$target_date[test]))

# Growing window, or the most recent 260 weeks of target observations.
# Training outcomes must already be observed by the forecast origin.
training_rows <- function(i, window) {
  ok <- d$complete & d$target_date <= d$origin[i]
  if (window == "five_years") ok <- ok & d$target_date > d$origin[i] - 7*260
  which(ok)
}
forecast <- function(features, window, rows) {
  formula <- reformulate(features, response = "y")
  predictions <- numeric(length(rows))
  for (j in seq_along(rows)) {
    i <- rows[j]
    train <- training_rows(i, window)
    stopifnot(max(d$target_date[train]) <= d$origin[i], length(train) > 100)
    model <- lm(formula, data = d[train, ])
    if (anyNA(coef(model))) stop("Rank-deficient model; review predictors")
    predictions[j] <- as.numeric(predict(model, newdata = d[i, ]))
  }
  predictions
}
score <- function(predicted, rows) {
  error <- predicted - d$y[rows]
  c(MAE = mean(abs(error)), RMSE = sqrt(mean(error^2)), bias = mean(error))
}
local_features <- function(p) paste0("local", 0:(p-1))
spot_features <- function(p, q, crude) c(local_features(p),
  paste0(crude, 0:(q-1)), paste0("rbob", 0:(q-1)))

## 1. Select AR and ARDL settings on 2024 only.
settings <- list()
validation_scores <- data.frame()
for (window in c("expanding", "five_years")) {
  for (p in c(1, 2, 4)) {
    id <- paste("AR", window, p, sep = "_")
    features <- local_features(p)
    settings[[id]] <- list(features = features, window = window)
    s <- score(forecast(features, window, validation), validation)
    validation_scores <- rbind(validation_scores, data.frame(
      id = id, family = "AR", window = window, p = p, q = 0, crude = "none",
      MAE = s["MAE"], RMSE = s["RMSE"]))
    for (q in 1:2) {
      for (crude in c("wti", "brent")) {
        id <- paste("ARDL", window, p, q, crude, sep = "_")
        features <- spot_features(p, q, crude)
        settings[[id]] <- list(features = features, window = window)
        s <- score(forecast(features, window, validation), validation)
        validation_scores <- rbind(validation_scores, data.frame(
          id = id, family = "ARDL", window = window, p = p, q = q, crude = crude,
          MAE = s["MAE"], RMSE = s["RMSE"]))
      }
    }
  }
}
validation_scores <- validation_scores[order(validation_scores$MAE, validation_scores$RMSE), ]
best_ar <- validation_scores$id[validation_scores$family == "AR"][1]
best_ardl <- validation_scores$id[validation_scores$family == "ARDL"][1]
base <- settings[[best_ardl]]
write.csv(validation_scores, "output/validation_ar_ardl.csv", row.names = FALSE)

## 2. Hold the selected ARDL settings fixed; select equity features on 2024.
stock_sets <- list(XOM = "XOM", CVX = "CVX", MPC = "MPC", VLO = "VLO", PSX = "PSX",
                  two_groups = c("integrated_group", "refining_group"),
                  all_five = c("XOM", "CVX", "MPC", "VLO", "PSX"))
stock_scores <- data.frame()
for (name in names(stock_sets)) {
  for (q in 1:2) {
    id <- paste0("stocks_", name, "_", q)
    features <- c(base$features, unlist(lapply(stock_sets[[name]], function(x) paste0(x, 0:(q-1)))))
    settings[[id]] <- list(features = features, window = base$window)
    s <- score(forecast(features, base$window, validation), validation)
    stock_scores <- rbind(stock_scores, data.frame(id = id, stocks = name,
      stock_lags = q, MAE = s["MAE"], RMSE = s["RMSE"]))
  }
}
stock_scores <- stock_scores[order(stock_scores$MAE, stock_scores$RMSE), ]
best_stock <- stock_scores$id[1]
write.csv(stock_scores, "output/validation_stocks.csv", row.names = FALSE)
selected <- c(AR = best_ar, ARDL = best_ardl, ARDL_stocks = best_stock)

## 3. Frozen choices, sequential one-week-ahead test forecasts.
# Coefficients are refitted each week using only outcomes observed by that week.
predictions <- data.frame(origin = d$origin[test], target_date = d$target_date[test],
                         current_price = d$current_price[test], actual = d$actual_price[test])
test_scores <- data.frame()
validation_selected <- data.frame()
models <- c("naive", names(selected))
for (name in models) {
  if (name == "naive") {
    pred <- rep(0, length(test)); val <- rep(0, length(validation))
  } else {
    config <- settings[[selected[[name]]]]
    pred <- forecast(config$features, config$window, test)
    val <- forecast(config$features, config$window, validation)
  }
  predictions[[name]] <- d$current_price[test] + pred
  s <- score(pred, test)
  test_scores <- rbind(test_scores, data.frame(model = name,
    MAE = s["MAE"], RMSE = s["RMSE"], bias = s["bias"], n = length(test)))
  s <- score(val, validation)
  validation_selected <- rbind(validation_selected, data.frame(model = name,
    MAE = s["MAE"], RMSE = s["RMSE"], n = length(validation)))
}
test_scores$MAE_improvement_vs_naive_pct <- 100*(1-test_scores$MAE/test_scores$MAE[1])
write.csv(predictions, "output/test_predictions.csv", row.names = FALSE)
write.csv(test_scores, "output/test_metrics.csv", row.names = FALSE)
write.csv(validation_selected, "output/validation_selected.csv", row.names = FALSE)

# Show stability across the two test calendar years without changing selections.
yearly <- data.frame()
for (year in unique(format(predictions$target_date, "%Y"))) {
  rows <- format(predictions$target_date, "%Y") == year
  for (name in models) {
    e <- predictions[[name]][rows] - predictions$actual[rows]
    yearly <- rbind(yearly, data.frame(year = year, model = name,
       n = sum(rows), MAE = mean(abs(e)), RMSE = sqrt(mean(e^2))))
  }
}
write.csv(yearly, "output/test_metrics_by_year.csv", row.names = FALSE)

# Save the last test-origin fit for diagnostics, not a forecast of today's prices.
last_i <- tail(test, 1)
diagnostics <- data.frame()
for (name in names(selected)) {
  config <- settings[[selected[[name]]]]
  train <- training_rows(last_i, config$window)
  fit <- lm(reformulate(config$features, response = "y"), data = d[train, ])
  coefficients <- data.frame(term = names(coef(fit)), coefficient = unname(coef(fit)))
  write.csv(coefficients, paste0("output/", name, "_last_fit_coefficients.csv"), row.names = FALSE)
  # Keep calendar gaps when computing residual autocorrelation.
  positions <- which(d$target_date <= d$origin[last_i])
  if (config$window == "five_years") positions <- positions[d$target_date[positions] > d$origin[last_i]-7*260]
  residual_calendar <- rep(NA_real_, length(positions))
  residual_calendar[match(train, positions)] <- residuals(fit)
  png(paste0("output/", name, "_residuals.png"), width = 1400, height = 650, res = 140)
  par(mfrow = c(1, 2))
  plot(d$target_date[positions], residual_calendar, type = "l", xlab = "Training target date",
       ylab = "Residual (USD/gallon)", main = paste(name, "last fit residuals")); abline(h = 0, col = "gray")
  acf(residual_calendar, lag.max = 12, na.action = na.pass, main = "Residual ACF", xlab = "Lag (weeks)")
  dev.off()
}
png("output/test_forecasts.png", width = 1600, height = 900, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(predictions$target_date, predictions$actual, type = "l", lwd = 2,
     ylim = range(predictions[, c("actual", "ARDL", "ARDL_stocks")]),
     xlab = "Target Monday", ylab = "USD/gallon", main = "One-week-ahead forecasts on common test dates")
lines(predictions$target_date, predictions$ARDL, col = "steelblue")
lines(predictions$target_date, predictions$ARDL_stocks, col = "firebrick")
legend("topleft", c("Actual", "ARDL", "ARDL + stocks"),
       col = c("black", "steelblue", "firebrick"), lty = 1, bty = "n")
plot(predictions$target_date, predictions$ARDL-predictions$actual, type = "l", col = "steelblue",
     xlab = "Target Monday", ylab = "Forecast error (USD/gallon)", main = "Forecast errors")
lines(predictions$target_date, predictions$ARDL_stocks-predictions$actual, col = "firebrick")
abline(h = 0, col = "gray", lty = 2)
dev.off()

selection <- data.frame(model = names(selected), id = unname(selected),
  features = vapply(selected, function(id) paste(settings[[id]]$features, collapse = " + "), character(1)),
  window = vapply(selected, function(id) settings[[id]]$window, character(1)))
write.csv(selection, "output/selected_models.csv", row.names = FALSE)
cat("SELECTED MODELS\n"); print(selection, row.names = FALSE)
cat("VALIDATION\n"); print(validation_selected, row.names = FALSE)
cat("TEST\n"); print(test_scores, row.names = FALSE)
cat("BY YEAR\n"); print(yearly, row.names = FALSE)

# Minimal standalone HTML report; the computation is reproducible with base R.
table_html <- function(x) {
  for (j in seq_along(x)) if (is.numeric(x[[j]])) x[[j]] <- round(x[[j]], 5)
  rows <- apply(x, 1, function(r) paste0("<tr><td>", paste(r, collapse = "</td><td>"), "</td></tr>"))
  paste0("<table><tr><th>", paste(names(x), collapse = "</th><th>"), "</th></tr>", paste(rows, collapse = ""), "</table>")
}
report <- c('<!doctype html><html><head><meta charset="UTF-8"><title>Weekly gasoline forecasting</title>',
 '<style>body{font:16px/1.6 sans-serif;max-width:1100px;margin:40px auto;padding:0 20px}table{border-collapse:collapse;font-size:14px}td,th{border:1px solid #ddd;padding:8px;text-align:left}img{max-width:100%}h1,h2{line-height:1.3}</style></head><body>',
 '<h1>One week ahead gasoline forecasting</h1><p>Predict the change between consecutive Monday regional retail prices, then add it to the current price. No futures, seasonality or neural network is used in this first comparison.</p>',
 '<h2>Time assumptions and sample</h2><p>Assume the current local Monday observation is available at the forecast origin. Spot inputs end at least 10 days earlier; equity returns end the preceding Friday. Exact historical release timestamps and revisions are not verified. The stock snapshot contains retrospectively adjusted prices. Therefore this is an exploratory chronological backtest, not certified real-time performance.</p>',
 paste0('<p>2024 validation targets: ', length(validation), '; test targets from 2025 onward: ', length(test), '. Test range: ', min(d$target_date[test]), ' to ', max(d$target_date[test]), '. All models use identical complete target rows. Missing rows are not imputed; excluded origins are saved. Recent five years means a trailing 260-week training window.</p>'),
 '<p>AR orders: 1, 2, 4. ARDL uses WTI or Brent plus RBOB, each with 1 or 2 available weekly changes. Search over growing and five-year windows. Select by 2024 MAE, then RMSE. Stock comparison fixes the selected ARDL settings and tries five single stocks, two groups, or all five, with 1 or 2 available weekly returns. Settings are then frozen; coefficients are refitted at each test origin using only previously observed targets.</p>',
 '<p>Earlier EDA viewed the full period, so this test is not completely untouched. Validation involves multiple candidate choices. These results are preliminary and need subsequent fresh-data evaluation. Observation dates are not publication dates. No forecast of the current calendar week is produced from these old snapshots.</p>',
 '<h2>Selected configurations</h2>', table_html(selection),
 '<p>local0 is the origin-week local change; local1 is one week earlier. wti0/brent0/rbob0 is the change ending on origin minus 10 days. Stock0 is the return ending the preceding Friday; stock1 is one week earlier.</p>',
 '<h2>Validation results</h2>', table_html(validation_selected),
 '<h2>Test results</h2><p>MAE, RMSE and bias are in dollars per gallon. Positive bias means overprediction. Negative percentage improvement means worse than the naive baseline.</p>', table_html(test_scores),
 '<h2>Test results by year</h2>', table_html(yearly),
 '<h2>Initial interpretation</h2><p>Adding the validation-selected VLO return reduces ARDL test MAE by about 2.06% and RMSE by about 2.61%. However, the local-only AR model has lower test MAE than either ARDL variant. Thus these data do not yet establish that stocks should be retained. The small equity improvement has not been tested for statistical significance, and performance varies substantially by year. Do not choose new stock features by inspecting this test.</p>',
 '<img src="output/test_forecasts.png" alt="Weekly test forecasts">',
 '<h2>Residual diagnostics</h2><p>These are the last historical training fits. Residual dependence or changing variance can remain; model assumptions should not be inferred from accuracy alone. Gaps remain on the calendar in ACF plots. Do not interpret differences in selected models as causal effects.</p>',
 '<img src="output/AR_residuals.png" alt="AR residual diagnostics">',
 '<img src="output/ARDL_residuals.png" alt="ARDL residual diagnostics">',
 '<img src="output/ARDL_stocks_residuals.png" alt="Stock model residual diagnostics">',
 '</body></html>')
writeLines(report, "forecast_report.html")
capture.output(sessionInfo(), file = "output/session_info.txt")
write.csv(data.frame(file = c("data/local_spot.csv", "data/stock_returns.csv"),
  md5 = unname(tools::md5sum(c("data/local_spot.csv", "data/stock_returns.csv")))),
  "output/input_manifest.csv", row.names = FALSE)

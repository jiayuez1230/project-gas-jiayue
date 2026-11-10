Run from this stock_eda folder in RStudio.

1. Knit stock_prices_eda.Rmd to regenerate the HTML, clean CSVs, and figures
   using the stored raw snapshots. Packages: rmarkdown, knitr, jsonlite.
2. Optional: source("download_stocks.R") refreshes the fixed requested
   2016-12-01 to 2026-11-02 daily snapshot using httr and Yahoo Finance.
   It overwrites raw stock JSON files; preserve a copy first if comparing vintages.

Main outputs:
- data/clean/stock_weekly_returns.csv: five stocks and two group signals
- data/clean/local_stock_weekly.csv: local prices/changes and stock returns
- output/stock_local_level_correlations.csv: level associations
- output/stock_local_lag_correlations.csv: weekly-change lag associations

All calculations are descriptive. No forecasting model has been fitted.
The weekly calendar is preserved; local missing values are not imputed.

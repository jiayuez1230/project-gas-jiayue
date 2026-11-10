# Yahoo Finance daily chart data. No API key is embedded.
symbols <- c("XOM", "CVX", "MPC", "VLO", "PSX")
start <- as.numeric(as.POSIXct("2016-12-01", tz = "UTC"))
end <- as.numeric(as.POSIXct("2026-11-03", tz = "UTC"))
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
for (symbol in symbols) {
  response <- httr::GET(
    paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol),
    query = list(period1 = start, period2 = end, interval = "1d", events = "div,splits"),
    httr::user_agent("Mozilla/5.0"), httr::timeout(40))
  httr::stop_for_status(response)
  body <- httr::content(response, as = "text", encoding = "UTF-8")
  writeLines(body, paste0("data/raw/", symbol, ".json"))
  # Keep JSON intact; the report handles the response structure.
  message(symbol, " downloaded")
}

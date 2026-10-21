####### -------------- Function for Daily Stocks Data

DailyTimeSeries <- function(symb) {
  Symbol <- "IBM"
  url = paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=", Symbol, "&outputsize=full&apikey=6UXZPKFR0U1PJUG0")
  Daily_Time_series <- jsonlite::fromJSON(url)
  L1 <- Daily_Time_series$`Time Series (Daily)`
  Daily_Time_series_data <- plyr::ldply(L1, data.frame)
  colnames(Daily_Time_series_data) <- c("Date", "Open", "high", "low", "close", "volume")

  return(Daily_Time_series_data)
}
# DailyTimeSeries Function accepts one symbol at a time and returns a dataframe
# output1 <- DailyTimeSeries("IBM")


####### -------------- Function Quarterly Data

#######-------------- Function for Daily Stocks Data

DailyTimeSeries <- function(symb)
{
  Symbol <- "IBM"
  url = paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",Symbol,"&outputsize=full&apikey=6UXZPKFR0U1PJUG0")
  Daily_Time_series <- jsonlite::fromJSON(url)
  L1 <- Daily_Time_series$`Time Series (Daily)`
  Daily_Time_series_data <- plyr::ldply(L1,data.frame)
  colnames(Daily_Time_series_data) <- c("Date","Open","high","low","close","volume")
  
  return(Daily_Time_series_data)
}
# DailyTimeSeries Function accepts one symbol at a time and returns a dataframe
#output1 <- DailyTimeSeries("IBM")


#######-------------- Function Quarterly Data
Quarterly_data <- function(symb)
{
  Symbol <- symb
  func <- c('INCOME_STATEMENT','BALANCE_SHEET','CASH_FLOW')
  Q_reports <- c()
  for (f in 1:length(func)){
    url = paste0("https://www.alphavantage.co/query?function=",func[f],"&symbol=",Symbol,"&outputsize=full&apikey=6UXZPKFR0U1PJUG0")
    Json_D <- jsonlite::fromJSON(url)
    QReports <- as.data.frame(Json_D$quarterlyReports)
    Q_reports <- append(Q_reports,QReports)
  }
  # Merging Income Statement, Cashflow and Balancesheet dataframes into 1 dataframe
  df_Quarterly <- data.frame(Q_reports)

  return(df_Quarterly)
}

# It takes one symbol as an argument and returns a dataframe which includes (CashFlows, Income Statement, Balance sheet )
#output <- Quarterly_data("AAPL")

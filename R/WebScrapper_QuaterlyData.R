

#######-------------- Function Quarterly Data
Quarterly_data <- function(symb)
{
# Required Libraries
requireNamespace("RJSONIO")
requireNamespace("jsonlite")
  Companies <- list()
  func <- c('INCOME_STATEMENT','BALANCE_SHEET','CASH_FLOW')
  for (Symbol in symb){
    print(Symbol)
    Q_reports <- c()
    for (f in 1:length(func)){
      url = paste0("https://www.alphavantage.co/query?function=",func[f],"&symbol=",Symbol,"&outputsize=full&apikey=6UXZPKFR0U1PJUG0")
      Json_D <- fromJSON(url)
      QReports <- as.data.frame(Json_D$quarterlyReports)
      Q_reports <- append(Q_reports,QReports)
    }
    # Merging Income Statement, Cashflow and Balancesheet dataframes into 1 dataframe
    df_Quarterly <- as.data.frame(Q_reports)
    Companies[[Symbol]] = df_Quarterly
    Sys.sleep(15)
  }
  return(Companies)
}

# You have to pass a vector as an argument to this function. You can use the function for one symbol as well as
# more than one symbol
#output <- Quarterly_data(c("AAPL"))

#output2 <- Quarterly_data(c("IBM","GOOGL"))



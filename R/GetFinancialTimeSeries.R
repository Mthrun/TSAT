GetFinancialTimeSeries=function(url="https://finance.yahoo.com",start = "1998-01-01",
                                quote = "Adj",instrument='ibm',PlotIt=TRUE){
requireNamespace('tseries')
  tryCatch({
  con <- url(url)
  if(!inherits(try(open(con), silent = TRUE), "try-error")) {
    close(con)
    x <- tseries::get.hist.quote(instrument = instrument, start = start,quote = quote)
  }
  if(PlotIt)
    plot(x, main = instrument,ylab=quote,xlab='Time')
  return(x)
  },error=function(e) return(NULL))
  
}
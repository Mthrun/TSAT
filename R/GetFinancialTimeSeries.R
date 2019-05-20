GetFinancialTimeSeries=function(url="https://finance.yahoo.com",start = "1998-01-01",end,
                                quote = "Adj",instrument='ibm',PlotIt=TRUE){
requireNamespace('tseries')
#atlernative: TTR:getYahooData() zu pruefen
  tryCatch({
  con <- url(url)
  if(!inherits(try(open(con), silent = TRUE), "try-error")) {
    close(con)
    if(missing(end))
      x <- tseries::get.hist.quote(instrument = instrument, start = start,quote = quote)
    else
      x <- tseries::get.hist.quote(instrument = instrument, start = start,quote = quote,end = end)
  }
  if(PlotIt)
    plot(x, main = instrument,ylab=quote,xlab='Time')
  return(x)
  },error=function(e) return(NULL))
  
}
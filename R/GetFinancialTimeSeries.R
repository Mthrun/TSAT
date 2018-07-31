GetFinancialTimeSeries=function(url="https://finance.yahoo.com",start = "1998-01-01",
                                quote = "Adj",instrument='ibm',PlotIt=TRUE){
requireNamespace('tseries')
  con <- url(url)
  if(!inherits(try(open(con), silent = TRUE), "try-error")) {
    close(con)
    x <- tseries::get.hist.quote(instrument = instrument, start = start,quote = quote)
    
    # x <- get.hist.quote(instrument = "^gspc", start = start,
    #                     quote = quote)
    # plot(x)
    # x <- get.hist.quote(instrument = instrument, quote = c("Cl", "Vol"))
    # plot(x, main = "International Business Machines Corp")
    # spc <- get.hist.quote(instrument = "^gspc", start = "1998-01-01",
    #                       quote = "Close")
    # ibm <- get.hist.quote(instrument = "ibm",  start = "1998-01-01",
    #                       quote = "Adj")
    # require("zoo") # For merge() method.
    # x <- merge(spc, ibm)
    # plot(x, main = "IBM vs S&P 500")
  }
  if(PlotIt)
    plot(x, main = instrument,ylab=quote,xlab='Time')
  return(x)
}
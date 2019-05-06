AutoCorr=function(TimeSeries , nLags=2 ,PlotIt=FALSE,...){

  
  res1=acf(TimeSeries, lag.max = nLags, type = "correlation",
      plot = PlotIt, na.action = na.fail,...)
  return(invisible(res1))
  
}
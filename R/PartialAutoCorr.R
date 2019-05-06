PartialAutoCorr=function(TimeSeries , nLags=2 ,PlotIt=FALSE,...){

  
  res1=pacf(TimeSeries, lag.max = nLags,
      plot = PlotIt, na.action = na.fail,...)
  return(invisible(res1))
  
}
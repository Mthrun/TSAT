# AutoCorr = AutoCorr(TimeSeries, nLags = 2, PlotIt = FALSE,...)
#
# Description:
# Calculates the AutoCorrelation function
#
# INPUT
# TimeSeries         A numeric time series object, or a numeric vector/matrix.        
# nLags              Number of lags. Default is 2.  
# PlotIt             Boolean, TRUE if auto correlation plot should be printed, FALSE else. Default is FALSE.  
# ...                further arguments passed on to output of ccf.
#
# OUTPUT 
# "acf" object/list with:
# acf                A three dimensional array containing the estimated acf.
# type              "Correlation"
# n.used            The number of observations in the time series.
# lag               A three dimensional array containing the lags at which the acf is estimated.
# series            Name of time series
# snames            The series names for a multivariate time series. NULL for univariate time series.
#
# Author: MCT

AutoCorr=function(TimeSeries , nLags=2 ,PlotIt=FALSE,...){

  
  res1=acf(TimeSeries, lag.max = nLags, type = "correlation",
      plot = PlotIt, na.action = na.fail,...)
  return(invisible(res1))
  
}
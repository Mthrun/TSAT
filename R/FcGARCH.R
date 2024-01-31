# res = FcGARCH(DataVec)
#
# DESCRIPTION
# Autoregressive conditional heteroskedasticity (ARCH) model is a statistical model for time series data 
# that describes the variance of the current error term or innovation as a function of the actual sizes of the 
# previous time periods error terms often the variance is related to the squares of the previous innovations - Wikipedia
#
# INPUT
# DataVec             [1:n] numerical vector of time series data.
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# ForecastHorizon     Scalar defining the timesteps to forecast ahead
# TestandTrain        TRUE: Data is divided by Horizon, FALSE: Real Forecast is generated
# OPTIONAL
# Time                [1:n] character vector of Time in the length of data
# PlotIt              FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
# Silent              If FALSE, print diverse ouptuts of keras. Default is TRUE
# ...                 Further specification using the seasonal::seas interface of the X-13-ARIMA-SEATS standard. 
#                     Please see also references for further details
#
# OUTPUT
# Forecast            [1:Horizont] object of stats::ts
# Model               Model, the output of ugarchspec in mode invisible
#
# DETAILS
# Quelle: http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
# warning('Under Development')
#
# Author: MCT 2014

FcGARCH=function(DataVec,VarianceModel=list(model = "sGARCH", garchOrder = c(1, 1)),ForecastHorizon, DistributionModel = 'norm',PlotIt=FALSE,Summary=FALSE,...){
  #RelSchluss=dbt.Transforms::relDiff4Prices(DataVec)
#  if(missing(ForecastHorizon))
#    ForecastHorizon=round(0.2*length(DataVec))
  
#  if(missing(SplitAt)) {
#    warning('Input for SplitAt was not given. Setting SplitAt to length(DataVec)')
#    SplitAt = length(DataVec)
#  }
  if(missing(ForecastHorizon)) {
    warning('Input for ForecastHorizon was not given. Setting ForecastHorizon to 1')
    ForecastHorizon = 1
  }
#  if(missing(Time)) {
#    Time = 1:length(DataVec)
#    warning('No input for Time was given, will use 1:length(DataVec) for Time')
#  }
  
  if(!is.logical(Summary)) {
    warning('Input for Summary is not logical. Setting Summary to FALSE')
    Summary = FALSE
  }
  

  #inputs = checkInputForecasting(DataVec, Time, SplitAt, ForecastHorizon, PlotIt)
  inputs = checkInputForecasting(DataVec, 1:length(DataVec), length(DataVec), ForecastHorizon, PlotIt)
  DataVec = inputs$DataVec
  Time = inputs$Time # Currently not used
  SplitAt = inputs$SplitAt # Currently not used
  ForecastHorizon = inputs$ForecastHorizon
  PlotIt = inputs$PlotIt
  
  
  if(isTRUE(PlotIt) && isTRUE(Summary)) {
    warning('Can either plot forecast or summary. Setting Summary to FALSE')
    Summary = FALSE
  }
  

  errorMessage = packageMissingError("rugarch", n, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Model = NULL,
        Forecast = rep(NaN, length(DataVec)),
        Info = errorMessage
      )
    )
  }
  
  RelSchluss=DiffFilter(DataVec)

  # Initialisierung
  spec <- rugarch::ugarchspec(variance.model = VarianceModel, distribution.model = DistributionModel,...)
  
  # Analyse der Renditen
  garchfit <- rugarch::ugarchfit(spec=spec,data=RelSchluss,solver.control=list(trace=0),show=TRUE)
  #str(garchfit) #Was ist in hier gespeichert?
  if(Summary)
    rugarch::plot(garchfit,which="all")
  # Sum Squares Error betrachten, sind die Resdiuen um Null herum?
  forc = rugarch::ugarchforecast(garchfit, n.ahead=ForecastHorizon) # this means that 100 data points are left from the end with which to
  if(PlotIt)
    rugarch::plot(forc,which=1)
  return(invisible(list(Forecast=forc,Model=garchfit)))
}
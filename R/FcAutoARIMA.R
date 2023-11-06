# res = FcAutoArima(DataVec, SplitAt, ForecastHorizon, Time)
#
# DESCRIPTION
# Automatic autoregressive and moving average modelling with difference filter
#
# INPUT
# DataVec             [1:n] numerical vector of time series data.
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# ForecastHorizon     Scalar defining the timesteps to forecast ahead
# OPTIONAL
# Time                [1:n] character vector of Time in the length of data
# PlotIt              FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
# Seasonal            Optional, if FALSE: no seasonality in data an one step forecast only. Default is TRUE
# PlotBackwardInd     Optional, How many units to plot back in time?
# xlab                see plot function
# ylab                see plot function
# ...                 Further arguments for plotting, see plot
#
# OUTPUT
# Forecast            [1:ForecastHorizon] of test data Y, test time and forecast FF
# ArimaObject         Forecast object, the output of forecast (package)
# Model               Model, the output of auto.arima
#
# DETAILS
# - Autoregressive part (AR) with Lag 4 w.r.t signal term
# - Difference Filter (I): 1 Lag, because noon stationary TS (time dependent expectation value of time series)
# - Moving Average (MA) with Lag 4 w.r.t noise term
# - requires homoscedastic time series - variance does not depend on time
#
# Author: MCT

#FcAutoARIMA=function(Data,Time,ForecastHorizon,SplitAt,PlotIt=TRUE,Seasonal=TRUE,PlotBackwardInd,main='',xlab='Time',ylab='Data',...){
FcAutoARIMA=function(DataVec,SplitAt,ForecastHorizon,Time,PlotIt=TRUE,Seasonal=TRUE,PlotBackwardInd,main='',xlab='Time',ylab='Data',...){
  # author: MT 2014/ edited 2018, edited Dec, 2019
  
  if(missing(SplitAt)) {
    warning('Input for SplitAt was not given. Setting SplitAt to length(DataVec)')
    SplitAt = length(DataVec)
  }
  if(missing(ForecastHorizon)) {
    warning('Input for ForecastHorizon was not given. Setting ForecastHorizon to 1')
    ForecastHorizon = 1
  }
  if(missing(Time)) {
    Time = 1:length(DataVec)
    warning('No input for Time was given, will use 1:length(DataVec) for Time')
  }
  
  inputs = checkInputForecasting(DataVec, Time, SplitAt, ForecastHorizon, PlotIt)
  DataVec = inputs$DataVec
  Time = inputs$Time
  SplitAt = inputs$SplitAt
  ForecastHorizon = inputs$ForecastHorizon
  PlotIt = inputs$PlotIt
  
  if(!is.logical(Seasonal)) {
    warning('Input for Seasonal is not logical. Setting Seasonal to TRUE')
    Seasonal = TRUE
  }
  
  errorMessage = packageMissingError("forecast", length(DataVec), SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Forecast = rep(NaN, length(DataVec)-SplitAt),
        ArimaObject = NULL,
        Model = NULL,
        Info = errorMessage
      )
    )
  }
  
  errorMessage = packageMissingError("zoo", length(DataVec), SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Forecast = rep(NaN, length(DataVec)-SplitAt),
        ArimaObject = NULL,
        Model = NULL,
        Info = errorMessage
      )
    )
  }
  
  DataFrame=data.frame(Time=Time,Data=DataVec)
  requireNamespace('forecast')
  requireNamespace('zoo')
  #requireNamespace('fanplot')
  if(missing(PlotBackwardInd)) {
    PlotBackwardInd=nrow(DataFrame)
  } else if(PlotBackwardInd < 1 || PlotBackwardInd > nrow(DataFrame)) {
    warning('PlotBackwardInd should be a value between 1 and length of data')
    PlotBackwardInd=nrow(DataFrame)
  }
    
  
  n=nrow(DataFrame)
  test=tail(DataFrame,n = n-SplitAt)
#  train=with(head(DataFrame,SplitAt), zoo::zoo(head(Data,SplitAt), order.by = head(Time,SplitAt)))
  train=with(head(DataFrame,SplitAt), zoo::zoo(head(DataVec,SplitAt), order.by = head(Time,SplitAt)))

  fit=forecast::auto.arima(train,seasonal=Seasonal)
  #trainingForecast=forecast::forecast(train,h=ForecastHorizon)
  future=forecast::forecast(fit,h=ForecastHorizon)
  if(PlotIt){
    plot(tail(DataFrame$Time,PlotBackwardInd),tail(DataFrame$Data,PlotBackwardInd),type='l',main=paste(main,future$method,' - black calls, red predicton, blue 85% confindence interval'),xlab=xlab,ylab=ylab,...)
    points(tail(DataFrame$Time,ForecastHorizon),future$mean,type='l',col='red')
    points(tail(DataFrame$Time,ForecastHorizon),future$lower[,1],type='l',col='blue')
    points(tail(DataFrame$Time,ForecastHorizon),future$upper[,1],type='l',col='blue')
  }
  # #Doku, Under Development
  # #author MT 2014 and 2018
  # #Quelle die besser als eigener Ansatz ist: DataVisualization in R
  # warning('Under Development')
  # TStrans=GenerateRegularTS(Data,TimeChar)
  # requireNamespace('forecast')
  # requireNamespace('fanplot')
  # fit=forecast::auto.arima(TStrans)
  # fore=matrix(NA,nrow=Par1,ncol=Par2)
  # for(i in 1:Par1){
  #   fore[,i]=simulate(fit,nsim=Par2)
  # }
  # if(PlotIt){
  #   plot(TStrans,xlim=TimeXlim,main=main)
  # fanplot::fan(fore,type='intervall',start=StartForcastVec,anchor=2.28)
  # }
  # if(is.null(TestTime)) TestTime=rep(NaN,ForecastHorizon)
  # if(is.null(TestData)) TestData=rep(NaN,ForecastHorizon)
  # DF=data.frame(Time=TestTime,)
  FF=zoo::coredata(future$mean)
  Forecast=data.frame(Time=test$Time[1:ForecastHorizon],FF=FF[1:ForecastHorizon],Y=test$Data[1:ForecastHorizon])
  return(invisible(list(Forecast=Forecast,ArimaObject=future,Model=fit)))
}
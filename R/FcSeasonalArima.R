# res = FcSeasonalArima(DataVec,SplitAt,ForecastHorizon,Frequency,Time)
#
# DESCRIPTION
# Automatic Forecasting using the seasonal arima method based on the X-13-ARIMA-SEATS standard.
#
# INPUT
# DataVec             [1:n] numerical vector or a time series object. If a time series object is given, the argument "Frequency" does not need to be given.
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# ForecastHorizon     Scalar defining the timesteps to forecast ahead
# Frequency           Either "days", "weeks", "months" or "quarters" or "years", see ConvertNumerical2TSobject. Can be ignored if DataVec is a time series object.
# Time                [1:n] character vector of Time in the length of data. Can be ignored if DataVec is a time series object.
# OPTIONAL
# PlotIt              FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
# ...                 Further specification using the seasonal::seas interface of the X-13-ARIMA-SEATS standard. 
#                     Please see also references for further details
#
# OUTPUT
# Forecast            [1:Horizont] object of stats::ts
# Accuracy            Accuracy of TestSet against Forecast, if TestandTrain==TRUE
# TaT                 TrainingSet and TestSet, if TestandTrain==TRUE
# Model               Output of seasonal::seas
# ForecastMaxlead     Output of seasonal::series, which is an extended forecast if upper and lower boundary.
#
# DETAILS
# X-13-ARIMA-SEATS is the standard of the seasonal adjustment software by the US Census Bureau. 
# This function is an easy-to-use wrapper of the function seasonal::seas calling the interface.
#
# Author: MCT
#FcSeasonalArima=function(TimeSeries,Horizon,TestandTrain=TRUE,PlotIt=TRUE,...){

FcSeasonalArima=function(DataVec,SplitAt,ForecastHorizon,Frequency,Time,PlotIt=TRUE,...) {
  
  if(missing(SplitAt)) {
    warning('Input for SplitAt was not given. Setting SplitAt to length(DataVec)')
    SplitAt = length(DataVec)
  }
  if(missing(ForecastHorizon)) {
    warning('Input for ForecastHorizon was not given. Setting ForecastHorizon to 1')
    ForecastHorizon = 1
  }
  
  if(!is.ts(DataVec)) {
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
    
    if(missing(Frequency)) {
      Frequency = "days"
      warning('No input for Frequency was given, which is needed to parse DataVec to a time series. Using "days" as Frequency"')
    }
    
    TimeSeries = ConvertNumerical2TSobject(DataVec, Time=Time, Frequency=Frequency)
    
  } else {
    TimeSeries = DataVec
  }
  
  n=length(TimeSeries)
  
  errorListReturn = list(Forecast = rep(NaN, n-SplitAt),
                          Accuracy = NaN,
                          TaT = DataVec,
                          Model = NULL,
                          ForecastMaxlead = NULL,
                          Info = ""
                        )
  errorMessage = packageMissingError("seasonal", n, SplitAt)
  if(errorMessage != FALSE){
    errorListReturn$Info=errorMessage
    return(errorListReturn)
  }
  
  errorMessage = packageMissingError("CombMSC", n, SplitAt)
  if(errorMessage != FALSE){
    errorListReturn$Info=errorMessage
    return(errorListReturn)
  }
  
  errorMessage = packageMissingError("forecast", n, SplitAt)
  if(errorMessage != FALSE){
    errorListReturn$Info=errorMessage
    return(errorListReturn)
  }
  
  
  if(SplitAt < n) TestandTrain = TRUE
  else TestandTrain = FALSE
  
  if(isTRUE(TestandTrain)){
#  if(SplitAt < n){
    #TaT=CombMSC::splitTrainTest(TimeSeries,n-length(tail(TimeSeries,ForecastHorizon)))
    TaT=CombMSC::splitTrainTest(TimeSeries,n-length(tail(TimeSeries,n-SplitAt)))
    Train=TaT$train
  }else{
    Train=TimeSeries
    TaT=list(train=TimeSeries,test=NULL)
  }
  
#https://www.census.gov/ts/x13as/docX13AS.pdf
  #he  default  is  one  year  of  forecasts  (unless  a  SEATS
  #seasonal adjustment is requested - then the default is three years of forecasts) and 120 is
  # the maximum. 
  #setting forecast.maxlead lower results in 3 year forecast
  m <- seasonal::seas(Train, forecast.save = "forecasts",forecast.maxlead = ForecastHorizon,forecast.maxback=0,...)
  ff=seasonal::series(m, "forecast.forecasts")
  
  if(isTRUE(PlotIt)){
    plot(TimeSeries)
    fplot=CombMSC::splitTrainTest(ff[,"forecast"],ForecastHorizon)
    points(fplot$train,col='red')
  }
  if(isTRUE(TestandTrain)){
    if(ForecastHorizon > n-SplitAt) {
      Acc=forecast::accuracy(fplot$train[1:length(TaT$test)],TaT$test)
    } else if(ForecastHorizon < n-SplitAt) {
      Acc=forecast::accuracy(fplot$train,TaT$test[1:ForecastHorizon])
    } else {
      Acc=forecast::accuracy(fplot$train,TaT$test)
    }
  }else{
    Acc=NULL
  }
  return(list(Forecast=fplot$train,Accuracy=Acc,TaT=TaT,Model=m,ForecastMaxlead=ff))
}
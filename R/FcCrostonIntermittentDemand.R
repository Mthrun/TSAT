# res = FcCrostonIntermittentDemand(DataVec)
#
# DESCRIPTION
# The Croston model answers the question How much demand will we have on average per period?
# This function returns forecasts and other information for Croston's forecasts.
#
# INPUT
# DataVec             [1:n] numerical vector time series data.
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# ForecastHorizon     Scalar defining the timesteps to forecast ahead
# Time                [1:n] character vector of Time in the length of data
# OPTIONAL
# ModelType           Croston's method variant: 1. "croston" Croston's method; 
#                     2. "sba" Syntetos-Boylan approximation; 
                      # 3. "sbj" Shale-Boylan-Johnston, see tsintermittent::crost.
# PlotIt              FALSE (default), do nothing. TRUE: plots the forecast versus the validation set.
# ...                 Further optional parameters for tsintermittent::crost
#
# OUTPUT
# Forecast            [1:ForecastHorizon] numeric vector, forecast for test set times
# TestSet             [ForecastHorizon+1:n] TestSet
# Model               Output of tsintermittent::crost
# TrainingSet         [1:ForecastHorizon] Trainingset
#
# DETAILS
# The Croston method is suitable if demand appears at random, with many or even most time periods having no demand; 
# where demand does occur, the historical data is randomly distributed, independently or almost independently of 
# the demand interval. Such demand patterns are known as "lumpy demand" or intermittent, irregular, 
# random or sporadic demand. 
# The approach is  based on Croston's (1972) method for intermittent demand forecasting, 
# also described in Shenstone and Hyndman (2005).
# Croston's method involves using simple exponential smoothing (SES) on the non-zero elements of the time series 
# and a separate application of SES to the times between non-zero elements of the time series. 
# The smoothing parameters of the two applications of SES are assumed to be equal and are denoted by alpha. 
# Optimisation of the types of croston methods is described in [Kourentzes, 2014].
#
# Author: MCT

FcCrostonIntermittentDemand=function(DataVec,Time,ForecastHorizon,SplitAt,Frequency= "days",ModelType="sbj",PlotIt=FALSE,...){
  
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
  
  n = length(DataVec)
  
  errorMessage = packageMissingError("tsintermittent", n, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Model = NULL,
        Forecast = rep(NaN, length(DataVec)-SplitAt),
        ForecastTrain = rep(NaN, SplitAt),
        Info = errorMessage
      )
    )
  }
  errorMessage = packageMissingError("forecast", n, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Model = NULL,
        Forecast = rep(NaN, length(DataVec)-SplitAt),
        ForecastTrain = rep(NaN, SplitAt),
        Info = errorMessage
      )
    )
  }
  errorMessage = packageMissingError("lubridate", N, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        Model = NULL,
        Forecast = rep(NaN, length(DataVec)-SplitAt),
        ForecastTrain = rep(NaN, SplitAt),
        Info = errorMessage
      )
    )
  }

  
  train=TSAT::ConvertNumerical2TSobject(head(DataVec,SplitAt),head(Time,SplitAt),Frequency =Frequency)
  test=tail(DataVec,n-SplitAt)[1:ForecastHorizon]   
  ttime=tail(Time,n-SplitAt)[1:ForecastHorizon]
  fc=rep(0,ForecastHorizon)
  trainingFc=rep(0,SplitAt)
  model='Setting forecast to zero'
  tryCatch({
    model=tsintermittent::crost(train,h=ForecastHorizon,type=ModelType,outplot = PlotIt,...)
    fc=model$frc.out
    trainingFc=model$frc.in
  },error=function(e) {
    print(e)
    print('Setting forecast to zero')
  }
  )
  #acc=forecast::accuracy(f = fc,test)
  #testfull=TSAT::ConvertNumerical2TSobject(test, tail(Time,SplitAt),Frequency =Frequency)
  #DF=data.frame(Time=ttime,FF=fc,TestData=test)
  #return(list(Forecast=DF,Model=model,TrainingSet=train))
  return(list(Forecast=fc,Model=model,TrainingSet=train, TestSet=test, TrainingForecast=trainingFc))
}
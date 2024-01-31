# res = FcBayesianStructuralTimeSeries(DataVec, SplitAt, Time, ForecastHorizon)
#
# DESCRIPTION
# ToDo
#
# INPUT
# DataVec             [1:n] numerical vector of time series data.
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# ForecastHorizon     Scalar defining the timesteps to forecast ahead
# OPTIONAL
# Time                [1:n] character vector of Time in the length of data
# Frequency           Either days, weeks, months or quarters or years, see TSAT::ConvertNumerical2TSobject.
# nseasons            Number of seasons. See bsts::AddSeasonal
# niter               Number of iterations. See bsts::bsts
# burn                Number of MCMC iterations to be discarded as burn-in. See bsts::predict.bsts
# PlotIt              FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
#
# OUTPUT
# Forecast            [1:ForecastHorizon] forecasted values
# ForecastTime        [1:ForecastHorizon] Time values of forecasts
# UpperLower          [1:2,1:ForecastHorizon] matrix of upper and lower bounds of the credible interval for the prediction, see bsts::predict.bsts
# ModelPrediction     Object of class predict of bsts package, see bsts::predict.bsts
# Model               Model object of class bsts, see bsts::bsts
#
# DETAILS
# ToDo
# Wrapper for functions of the bsts package.
#
# If length(DataVec)-SplitAt=ForecastHorizon than forecast is completely  on test data set.
# Is ForecastHorizon>length(DataVec)-SplitAt than a real forecast is made of ForecastHorizon-(n-SplitAt) steps.
#
# Author: MCT


FcBayesianStructuralTimeSeries=function(DataVec,SplitAt,Time,Frequency="months",ForecastHorizon,nseasons = 12, niter = 250, burn = 100,PlotIt=FALSE){

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
  
  if(!(is.numeric(nseasons) && length(nseasons) == 1 && nseasons >= 0)) {
    warning('nseasons should be a positive scalar. Setting nseasons to 12')
    nseasons = 12
  }
  if(!(is.numeric(niter) && length(niter) == 1 && niter >= 0)) {
    warning('niter should be a positive scalar. Setting niter to 250')
    niter = 250
  }
  if(!(is.numeric(burn) && length(burn) == 1 && burn >= 0)) {
    warning('burn should be a positive scalar. Setting burn to 100')
    burn = 100
  }
  
  errorMessage = packageMissingError("bsts", length(DataVec), SplitAt)
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
  
  DF=data.frame(Data=DataVec,Time=Time)
  train=DF[1:SplitAt,]
  
  if(SplitAt<nrow(DF)){
    testind=seq(from=(SplitAt+1),to=nrow(DF),by=1)
    testdata=DF[testind,]
    if(missing(ForecastHorizon)){
      ForecastHorizon=nrow(testdata)
    }
    Ytest=ConvertNumerical2TSobject(NumericVectorOrMatrix = testdata$Data,Time = testdata$Time,Frequency = Frequency)
  }else{
    #stop("SplitAt>length(Data) (i.e. future forecasts) is not implemented yet")
    # if(missing(ForecastHorizon)){
    #   ForecastHorizon=1
    # }
    # testdata=DF[SplitAt,]
    # for(rr in 2:(ForecastHorizon+1))
    #   testdata=rbind(testdata,DF[SplitAt,])
    # 
    # testdata$ds=seq(from=testdata$ds[1],length.out = ForecastHorizon+1,by = Frequency)
    # testdata=testdata[-1,]
    testdata=NULL
  }
  
  Y=ConvertNumerical2TSobject(NumericVectorOrMatrix = train$Data,Time = train$Time,Frequency = Frequency)

#y <- log(AirPassengers)
ss <- bsts::AddLocalLinearTrend(list(), Y)
ss <- bsts::AddSeasonal(ss, Y, nseasons = nseasons)
model <- bsts::bsts(Y, state.specification = ss, niter = niter) 
predTrain <- bsts::predict.bsts(model, horizon = ForecastHorizon, burn = burn)
if(is.null(testdata)){
  pred.full <- bsts::predict.bsts(model,horizon = ForecastHorizon)
}else{
  pred.full <- bsts::predict.bsts(model, newdata = Ytest,horizon = ForecastHorizon)
}

if(!is.null(testdata)){
  TimeTest=testdata$Time
}else{
  TimeTest=seq(from=tail(train$Time,1),length.out = ForecastHorizon+1,by=Frequency)[-1]
}
# TS=pred$original.serie
# Train=TSAT::ConvertTS2DF(TS)
if(isTRUE(PlotIt)){
  if(!is.null(testdata)){
    plot(TimeTest,pred.full$mean,type = "l",col="red",xlab = Frequency,ylab = "Data")
    points(TimeTest,testdata$Data,type = "l",col="blue")
  }else{
    plot(tail(train$Time,nseasons),tail(train$Data,nseasons),type = "l",col="blue",xlim = c(min(tail(train$Time,nseasons)),max(TimeTest)),xlab = Frequency,ylab = "Data")
    points(TimeTest,pred.full$mean,type = "l",col="red")
  }

}
return(list(Forecast=pred.full$mean,ForecastTime=TimeTest,UpperLower=pred.full$interval,ModelPrediction=predTrain,Model=model))
}
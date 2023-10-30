FcBayesianStructuralTimeSeries=function(Data, Time,Frequency="months",nseasons = 12, niter = 250, SplitDataAt,ForecastPeriods = 12, burn = 100,PlotIt=FALSE){

  DF=data.frame(Data=Data,Time=Time)
  train=DF[1:SplitDataAt,]
  
  if(SplitDataAt<nrow(DF)){
    testind=seq(from=(SplitDataAt+1),to=nrow(DF),by=1)
    testdata=DF[testind,]
    if(missing(ForecastPeriods)){
      ForecastPeriods=nrow(testdata)
    }
    Ytest=ConvertNumerical2TSobject(NumericVectorOrMatrix = testdata$Data,Time = testdata$Time,Frequency = Frequency)
  }else{
    #stop("SplitDataAt>length(Data) (i.e. future forecasts) is not implemented yet")
    # if(missing(ForecastPeriods)){
    #   ForecastPeriods=1
    # }
    # testdata=DF[SplitDataAt,]
    # for(rr in 2:(ForecastPeriods+1))
    #   testdata=rbind(testdata,DF[SplitDataAt,])
    # 
    # testdata$ds=seq(from=testdata$ds[1],length.out = ForecastPeriods+1,by = Frequency)
    # testdata=testdata[-1,]
    testdata=NULL
  }
  
  Y=ConvertNumerical2TSobject(NumericVectorOrMatrix = train$Data,Time = train$Time,Frequency = Frequency)

#y <- log(AirPassengers)
ss <- bsts::AddLocalLinearTrend(list(), Y)
ss <- bsts::AddSeasonal(ss, Y, nseasons = nseasons)
model <- bsts::bsts(Y, state.specification = ss, niter = niter) 
predTrain <- bsts::predict.bsts(model, horizon = ForecastPeriods, burn = burn)
if(is.null(testdata)){
  pred.full <- bsts::predict.bsts(model,horizon = ForecastPeriods)
}else{
  pred.full <- bsts::predict.bsts(model, newdata = Ytest,horizon = ForecastPeriods)
}

if(!is.null(testdata)){
  TimeTest=testdata$Time
}else{
  TimeTest=seq(from=tail(train$Time,1),length.out = ForecastPeriods+1,by=Frequency)[-1]
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
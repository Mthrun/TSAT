FcGradientBoosting=function(Time,Datavector,Frequency='days',Horizon,FUN=sum,PlotIt=TRUE){
  
  # devtools::install_github("ellisp/forecastxgb-r-package/pkg")
  requireNamespace('forecastxgb')
  

    switch(Frequency,
      days={
        DT=data.frame(Time=Time,Datavector=Datavector)
        N=nrow(DT)
        Splitted=list(
          TrainingSet = head(DT$Data,N-Horizon),
          TrainingTime = head(DT$Time,N-Horizon),
          TestSet = tail(DT$Data,Horizon),
          TestTime = tail(DT$Time,Horizon))
      },
      months={
        DT=TSAT::aggregateDays2Months(Time,Datavector,FUN = FUN,Header = c('Time','Data'))
        N=nrow(DT)
        Splitted=list(
          TrainingSet = head(DT$Data,N-Horizon),
          TrainingTime = head(DT$Time,N-Horizon),
          TestSet = tail(DT$Data,Horizon),
          TestTime = tail(DT$Time,Horizon))
      },
      weeks={
        DT=TSAT::aggregateDays2Weeks(Time,Datavector,FUN = FUN,Header = c('Time','Data'))
        N=nrow(DT)
        Splitted=list(
          TrainingSet = head(DT$Data,N-Horizon),
          TrainingTime = head(DT$Time,N-Horizon),
          TestSet = tail(DT$Data,Horizon),
          TestTime = tail(DT$Time,Horizon))
      },
      {stop('please chose correct frequency')}
    )
   

  TS=TSAT::ConvertNumerical2TSobject(Time =Splitted$TrainingTime ,NumericVectorOrMatrix = Splitted$TrainingSet,Frequency = Frequency)

  model <- forecastxgb::xgbar(TS)
  
  # fc <- forecastxgb::forecast.xgbar(model, h = Horizon)
  
  fc <- forecast::forecast(model, h = Horizon)
  if(PlotIt){
   # plot(fc)
  plot(Splitted$TestTime,Splitted$TestSet,type='l',col='black')
  points(Splitted$TestTime,fc$mean,type='l',col='red')
  }
  requireNamespace('forecast')
  acc=forecast::accuracy(fc$mean,Splitted$TestSet)
  
  return(list(
    Forecast = fc$mean,
    Model = model,
    ForecastStats=fc,
    TestData =data.frame(Time=Splitted$TestTime,TestSet=Splitted$TestSet),
    TrainData=data.frame(Time=Splitted$TrainingTime,TrainingSet=Splitted$TrainingSet)
  ))
}
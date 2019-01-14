RandomForestForecast=function(Time, DF,formula=NULL,Horizon,Package='randomForest',
                              AutoCorrelation,NoOfTree=200,PlotIt=TRUE,...){
  N=nrow(as.matrix(DF))
  if(!missing(Time)&!is.null(formula)){
    if(!is.Date(Time)){
      warning('Time is not a date, calling "as.Date".')
      Time=as.Date(Time)
    }
    if(length(Time)!=N){
      warning('Time has not the length of No. of rows of DF. Algorithm could be failing.')
    }
    requireNamespace('lubridate')
    DF$Weekdays=lubridate::wday(Time)
    DF$WeekNo=lubridate::isoweek(Time)
    DF$Months=lubridate::month(Time)
    DF$Quarters=lubridate::quarter(Time)
    DF$Years=lubridate::year(Time)
    
    #Feiertage reinrechnen ----
    hols=TSAT::GermanHolidays
    hols$Time=as.Date(hols$Time)
    DF$Holidays=Time %in% hols$Time
    
    TestTime=tail(Time,Horizon)
    TrainingTime = head(Time,N-Horizon)
  }else{
    Time=NULL
    TestTime=NULL
    TrainingTime=NULL
  }
  #Autokorellation reinrechnen ----
  if(!missing(AutoCorrelation)&!is.null(formula)){
    DF$DaysPrior=TSAT::LagVector(DF[,AutoCorrelation],Horizon)
    if((2*Horizon+1)<N){
      DF$DaysPrior[1:Horizon]=mean(DF$DaysPrior[(Horizon+1):(2*Horizon)],na.rm = T)
    }else{
      DF$DaysPrior[1:Horizon]=0
    }
  }
    requireNamespace('randomForest')


    Splitted=list(
    TrainingSet = head(DF,N-Horizon),
    TestSet = tail(DF,Horizon))


  if(is.null(formula)){
    #zyklisch zuruecksetzen leads to autocorralation

    Ntrain=nrow(as.matrix(Splitted$TrainingSet))
    Praediktor=c(tail(Splitted$TrainingSet,Horizon),head(Splitted$TrainingSet,Ntrain-Horizon))
    
    switch (Package,
            randomForest = {
              model = randomForest::randomForest(x=Splitted$TrainingSet,y=Praediktor,
                                                 ntree=NoOfTree,...)
              y_pred = predict(model,newdata = as.matrix(PraediktorTest))
            },
            ranger={
              model = ranger::ranger(data=Splitted$TrainingSet,formula=Praediktor~.,
                                     num.trees=NoOfTree,classification=FALSE,...)
              y_pred = predict(model,data = Splitted$TestSet)$predictions
            },
            {stop("Please choose either 'ranger' or 'randomForest'.")}
    )

    
    
    Ntest=nrow(as.matrix(Splitted$TestSet))
    PraediktorTest=c(tail(Splitted$TestSet,Horizon),head(Splitted$TestSet,Ntest-Horizon))

    
  }else{


  switch (Package,
          randomForest = {
            model = randomForest::randomForest(formula=formula,
                                               data=Splitted$TrainingSet,
                                               ntree=NoOfTree,...)
            y_pred = predict(model,newdata = Splitted$TestSet)
          },
          ranger={
            model = ranger::ranger(data=Splitted$TrainingSet,formula=formula,
                                   num.trees=NoOfTree,classification=FALSE,...)
          
            y_pred = predict(model,data = Splitted$TestSet)$predictions
            
          },
          {stop("Please choose either 'ranger' or 'randomForest'.")}
  )
  
 

  }# end is.null(formula)

  Forecast = y_pred
  TestData=Splitted$TestSet
  TrainData=Splitted$TrainingSet
  if(!is.null(TestTime)){
    names(Forecast)=as.character(TestTime)
    rownames(TestData)=as.character(TestTime)
    rownames(TrainData)=as.character(TrainingTime)
    
  }
  
  if(is.null(formula)){#no idea how to omplement otherwise
    if(PlotIt){
      plot(1:Horizon,Splitted$TestSet,type='l',col='black')
      points(1:Horizon,y_pred,type='l',col='red')
    }
     requireNamespace('forecast')
     acc=forecast::accuracy(y_pred,Splitted$TestSet)
  }else{
  acc=NULL
  if(PlotIt){
    if(!is.null(Time))
      plot(TestTime,Forecast,type='l')
    else
      plot(1:Horizon,Forecast,type='l')
  }
  }
  
  return(list(
    Forecast=Forecast,
    TestData =TestData,
    QualityMeasures=acc,
    Model = model,
    TrainData=Splitted$TrainingSet
  ))
}

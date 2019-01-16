RandomForestForecast=function(Time, DF, formula=NULL,Horizon,Package='randomForest',
                              AutoCorrelation,NoOfTree=200,PlotIt=TRUE,Holidays,...){
  N=nrow(as.matrix(DF))
  requireNamespace('lubridate')
  if(!missing(Time)&!is.null(formula)){
    if(!lubridate::is.Date(Time)){
      warning('Time is not a date, calling "as.Date".')
      Time=as.Date(Time)
    }
    if(length(Time)!=N){
      warning('Time has not the length of No. of rows of DF. Algorithm could be failing.')
    }

    DF$Weekdays=lubridate::wday(Time)
    DF$WeekNo=lubridate::isoweek(Time)
    DF$Months=lubridate::month(Time)
    DF$Quarters=lubridate::quarter(Time)
    DF$Years=lubridate::year(Time)
    
    #Feiertage reinrechnen ----
    if(missing(Holidays)){
    hols=TSAT::GermanHolidays
    }else{
      hols=Holidays
    }
    hols$Time=as.Date(hols$Time)
    DF$Holidays=Time %in% hols$Time
    DF$Workingdays=GetWorkingDays(Time,HolidaysTime = hols$Time,GermanBridgeDay = F)$WorkingDay

    TestTime=tail(Time,Horizon)
    TrainingTime = head(Time,N-Horizon)
  }else{
    Time=NULL
    TestTime=NULL
    TrainingTime=NULL
  }
  
  #Dauer zwischen zwei Messungen
  duration=rep(1,length(Time))
  for(i in 2:(length(Time))){
    duration[i]=difftime(Time[i],Time[i-1],units = 'days')#as.numeric(Time[i]-Time[i-1],units='days')
  }
  DF$Duration=duration
  #Autokorellation reinrechnen ----
  if(!missing(AutoCorrelation)&!is.null(formula)){
    x=DF[,AutoCorrelation]
    DF$DaysPrior=TSAT::LagVector(x,Horizon)
    lastvalue=TSAT::LagVector(x,1)
    if((2*Horizon+1)<N){
      DF$DaysPrior[1:Horizon]=mean(DF$DaysPrior[(Horizon+1):(2*Horizon)],na.rm = T)
      
    }else{
      DF$DaysPrior[1:Horizon]=0
    }
    lastvalue[1]=mean(lastvalue[(1+1):(2*1)],na.rm = T)
    #Renditen
    x2=x
    x2[x2<0]=0
    lastvalue[lastvalue<0]=0
    DF$Renditen=DatabionicSwarm::RelativeDifference(x2,lastvalue,na.rm = T,epsilon = 10^-14)
    #Aehnlichsten Punkte
    distance=matrix(NaN,length(x),length(x))
    for(i in 1:length(x)){
      for(j in 1:length(x))
        if(i<j){
          distance[i,j]=abs(x[i]-x[j])
        }
    }
    distvect=as.numeric(distance[upper.tri(distance,F)])
    q=quantile(distvect,c(0.05,0.99),na.rm = T)
    ind=which(distance<=q[1],arr.ind=T)
    DF$Similar=rep(q[2],length(x))
    u=sort(unique(ind[,1]))
    
    for(l in u){
      indi=sort(which(ind[,1]==l),decreasing = F)
      DF$Similar[l]=distance[ind[indi[1],1],ind[indi[1],2]]
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
            Importance=randomForest::importance(model)
          },
          ranger={
            model = ranger::ranger(data=Splitted$TrainingSet,formula=formula,
                                   num.trees=NoOfTree,classification=FALSE,...)
          
            y_pred = predict(model,data = Splitted$TestSet)$predictions
            Importance=ranger::importance(model)
            
            
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
  

  

  ordered=order(Importance,decreasing = T)
  Importance=Importance[ordered,]
  return(list(
    Forecast=Forecast,
    TestData =TestData,
    FeatureImportance=Importance,
    QualityMeasures=acc,
    Model = model,
    TrainData=Splitted$TrainingSet
  ))
}

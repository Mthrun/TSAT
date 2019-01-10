RandomForestForecast=function(DF,formula=NULL,Package='randomForest',Horizon,NoOfTree=200,PlotIt=TRUE,...){
  
  requireNamespace('randomForest')

  N=nrow(as.matrix(DF))
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
  }
  return(list(
    Forecast = y_pred,
    TestData =Splitted$TestSet,
    QualityMeasures=acc,
    Model = model,
    TrainData=Splitted$TrainingSet
  ))
}
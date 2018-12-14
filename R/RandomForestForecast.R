RandomForestForecast=function(DF,formula=NULL,Horizon,PlotIt=TRUE){
  
  requireNamespace('randomForest')
  
  N=nrow(as.matrix(DF))
  Splitted=list(
    TrainingSet = head(DF,N-Horizon),
    TestSet = tail(DF,Horizon))

  if(is.null(formula)){
    #zyklisch zuruecksetzen leads to autocorralation
    Ntrain=nrow(as.matrix(Splitted$TrainingSet))
    Praediktor=c(tail(Splitted$TrainingSet,Horizon),head(Splitted$TrainingSet,Ntrain-Horizon))
    model = randomForest::randomForest(x=Splitted$TrainingSet,y=Praediktor,
                                       ntree=100)
    
    Ntest=nrow(as.matrix(Splitted$TestSet))
    PraediktorTest=c(tail(Splitted$TestSet,Horizon),head(Splitted$TestSet,Ntest-Horizon))
    y_pred = predict(model,newdata = as.matrix(PraediktorTest))
    
  }else{
  model = randomForest::randomForest(formula=formula,
                           data=Splitted$TrainingSet,
                           ntree=100)
  y_pred = predict(model,newdata = Splitted$TestSet)
  }

  
  if(PlotIt){
    plot(1:Horizon,Splitted$TestSet,type='l',col='black')
    points(1:Horizon,y_pred,type='l',col='red')
  }
   requireNamespace('forecast')
   acc=forecast::accuracy(y_pred,Splitted$TestSet)
  
  return(list(
    Forecast = y_pred,
    Model = model,
    QualityMeasures=acc,
    TestData =Splitted$TestSet,
    TrainData=Splitted$TrainingSet
  ))
}
ExtremeLearningMachines=function(DataVector,Time,SplitDataAt=10, FirstPredictor,SecondPredictor,No_HiddenLayers=NULL,Scaled=TRUE,No_TrainingNetworks=10,PlotEvaluation=FALSE,PlotFuture=TRUE,...){
  
  requireNamespace('forecast')
  requireNamespace('nnfor')
  requireNamespace('DatabionicSwarm')
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  
  if(!missing(FirstPredictor)&!missing(SecondPredictor))
    Predictors=cbind(FirstPredictor,SecondPredictor)
  else if(!missing(FirstPredictor))
    Predictors=as.matrix(FirstPredictor)
  else
    Predictors=NULL
  
  Time=as.Date(Time)
  if(Scaled){
    requireNamespace('DatabionicSwarm')
  if(!is.null(Predictors))
      PredictorTS=ts(data=DatabionicSwarm::RobustNormalization(Predictors),start=min(Time))
  else
    PredictorTS=NULL
  #A simple scaling procedure after Milligan and Cooper 1977
  #Save for later processing
  quants = quantile(DataVector, c(0.01, 0.5, 0.99), na.rm = F)
  minX = quants[1]
  maxX = quants[3]
  Denom = maxX - minX
  ResponseTS=ts(data=DatabionicSwarm::RobustNormalization(DataVector),start=min(Time))
  }else{
    PredictorTS=ts(data=Predictors,start=min(Time))
    ResponseTS=ts(data=DataVector,start=min(Time))
  }
  #How many Weeks in Future should be predicted
  N=length(ResponseTS)
  TrainData=head(ResponseTS,SplitDataAt)
  TestData=tail(ResponseTS,N-SplitDataAt)
  if(!is.null(Predictors)){
  TrainRegr=head(PredictorTS,SplitDataAt)
  TestRegr=tail(PredictorTS,N-SplitDataAt)
  }else{
    TrainRegr=NULL
    TestRegr=NULL
  }
  #print(str(TrainRegr))
  #Build model wit Trainings data
  if(PlotEvaluation)
    m = graphics::layout(matrix(c(1, 2, 1, 2), 2, 2))
  
    modelScaled=nnfor::elm(TrainData,outplot = PlotEvaluation,reps = No_TrainingNetworks,xreg = TrainRegr,hd=No_HiddenLayers,...)#,xreg.lags=c(1,1),direct = F)

  if(PlotEvaluation){
    title(xlab='Time in Unixtime units since 1970-01-01',ylab='Datavector',main=paste('Model on Training Data with',No_TrainingNetworks,'Reptitions'))#Underfitting?
    plot(modelScaled)
  }
  #Forecast with model on the part of Testdata of Predictors
  predicted=forecast::forecast(modelScaled,h=N-SplitDataAt,xreg=PredictorTS)$mean
  
  if(Scaled){ #Scale back
    predicted=predicted*Denom+minX
    TrainData=TrainData*Denom+minX
    TestData=TestData*Denom+minX
  }
  
  AccuracyTest=forecast::accuracy(predicted, TestData)
  par(def.par)

  
if(PlotFuture)
  get('plotEvaluationFilteredTS', envir = asNamespace('TSAT'), inherits = FALSE)(Time[(SplitDataAt+1):N],TestData,predicted,FALSE)
  
return(list(Model=modelScaled,Forecast=as.numeric(predicted),TrainingData=as.numeric(TrainData),TestData=as.numeric(TestData),Accuracy=AccuracyTest))
}

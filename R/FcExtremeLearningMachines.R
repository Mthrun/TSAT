FcExtremeLearningMachines=function(DataVector,Time,SplitDataAt=10, Predictors,No_HiddenLayers=NULL,Scaled=TRUE,No_TrainingNetworks=10,PlotEvaluation=FALSE,PlotFuture=F,...){
  
  requireNamespace('forecast')
  requireNamespace('nnfor')
  requireNamespace('DatabionicSwarm')

  if(missing(Predictors))
    Predictors=NULL

  
  Time=as.Date(Time)
  if(Scaled){
    requireNamespace('DatabionicSwarm')
  if(!is.null(Predictors))
      PredictorTS=ts(data=DataVisualizations::RobustNormalization(Predictors),start=min(Time))
  else
    PredictorTS=NULL
  #A simple scaling procedure after Milligan and Cooper 1977
  #Save for later processing
  quants = quantile(DataVector, c(0.01, 0.5, 0.99), na.rm = F)
  minX = quants[1]
  maxX = quants[3]
  Denom = maxX - minX
  ResponseTS=ts(data=Datavisualizations::RobustNormalization(DataVector),start=min(Time))
  }else{
    if(!is.null(Predictors))
      PredictorTS=ts(data=Predictors,start=min(Time))
    else
      PredictorTS=NULL
    
    ResponseTS=ts(data=DataVector,start=min(Time))
    
   
  }
  #How many Weeks in Future should be predicted
  N=length(ResponseTS)
  if(!is.null(Predictors))
    if(nrow(PredictorTS)!=N) stop('Number of Cases of Predictors has to be equal to number of cases (length) of Datavectors.')
     
  if(SplitDataAt>N) stop('"SplitDataAt" is a higher number then length of cases of data')

    
    TrainData=head(ResponseTS,SplitDataAt)
    TestData=tail(ResponseTS,N-SplitDataAt)
    ForecastTime=tail(Time,N-SplitDataAt)
  if(!is.null(Predictors)){
    TrainRegr=head(PredictorTS,SplitDataAt)
    TestRegr=tail(PredictorTS,N-SplitDataAt)
  }else{
    TrainRegr=NULL
    TestRegr=NULL
  }
  #print(str(TrainRegr))
  #Build model wit Trainings data
  if(PlotEvaluation){
    def.par <-
      par(no.readonly = TRUE) # save default, for resetting...
    m = graphics::layout(matrix(c(1, 2, 1, 2), 2, 2))
  }

  
    modelScaled=nnfor::elm(TrainData,outplot = PlotEvaluation,reps = No_TrainingNetworks,xreg = TrainRegr,hd=No_HiddenLayers,...)#,xreg.lags=c(1,1),direct = F)

  if(PlotEvaluation){
    title(xlab='Time in Unixtime units since 1970-01-01',ylab='Datavector',main=paste('Model on Training Data with',No_TrainingNetworks,'Reptitions'))#Underfitting?
    plot(modelScaled)
    par(def.par)
  }
  #Forecast with model on the part of Testdata of Predictors
  predicted=forecast::forecast(modelScaled,h=N-SplitDataAt,xreg=PredictorTS)$mean
  
  if(Scaled){ #Scale back
    predicted=predicted*Denom+minX
    TrainData=TrainData*Denom+minX
    TestData=TestData*Denom+minX
  }
  
  AccuracyTest=forecast::accuracy(predicted, TestData)


  
if(PlotFuture)
  get('plotEvaluationFilteredTS', envir = asNamespace('TSAT'), inherits = FALSE)(Time[(SplitDataAt+1):N],TestData,predicted,FALSE)
  
return(list(Model=modelScaled,Forecast=as.numeric(predicted),ForecastTime=ForecastTime,TrainingData=as.numeric(TrainData),TestData=as.numeric(TestData),Accuracy=AccuracyTest))
}

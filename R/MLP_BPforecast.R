MLP_BPforecast=function(Response,ForecastHorizont=365,Predictor1,Predictor2,HiddenLayers=4,Threshold=0.05,Repetitions=10,ErrorFunction='sse',PlotEvaluation=FALSE,PlotFuture=TRUE){
  
  requireNamespace('neuralnet')
  requireNamespace('forecast')
  
  DF=data.frame(Response=Response,Predictor1=Predictor1,Predictor2=Predictor2)
  
  TrainingsData=head(DF,nrow(DF)-ForecastHorizont)
  out=neuralnet::neuralnet(data=TrainingsData,Response~Predictor1+Predictor2,hidden=HiddenLayers,threshold=Threshold,rep=Repetitions,err.fct = ErrorFunction)
  
  erg <- out$net.result[[1]]
  if(PlotEvaluation){
    plot(head(DF,nrow(DF)-ForecastHorizont)$Response,type='l')
    points(erg,type='l',col='red')
  }
  forecast=neuralnet::compute(out,tail(DF,ForecastHorizont)[,2:3])
  
  if(PlotFuture){
    plot(1:ForecastHorizont,tail(DF,ForecastHorizont)[,1],type='l')
    points(1:ForecastHorizont,forecast$net.result,type='l',col='red')
  }
  acc=forecast::accuracy(as.numeric(forecast$net.result),tail(DF,ForecastHorizont)[,1])

  return(list(Forecast=forecast$net.result,TestData=tail(DF,ForecastHorizont)[,1],TrainData=TrainingsData,TestDataOutput=forecast,Accuracy=acc,Model=out))  
}


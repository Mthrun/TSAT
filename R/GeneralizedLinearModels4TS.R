GeneralizedLinearModels4TS=function(Response,SplitDataAt,Predictor1,Predictor2=NULL,Time,PlotIt=TRUE,Summary=FALSE,...){
  #response : Full$CallsAsIssues
  #predictiors:  Full$Temperature
  
  N=length(Response)


  if(is.null(Predictor2)){
  Train=data.frame(Response=Response[1:SplitDataAt],Predictor1=Predictor1[1:SplitDataAt])
  TestSet=data.frame(Response=Response[(SplitDataAt+1):N],Predictor1=Predictor1[(SplitDataAt+1):N])
  
  model=glm(Response~ Predictor1,data = Train,...)

  new <- data.frame(Predictor1 = TestSet$Predictor1)
  predicted=predict(model,new)
  
  }else{
    Train=data.frame(Response=Response[1:SplitDataAt],Predictor1=Predictor1[1:SplitDataAt],Predictor2=Predictor2[1:SplitDataAt])
    TestSet=data.frame(Response=Response[(SplitDataAt+1):N],Predictor1=Predictor1[(SplitDataAt+1):N],Predictor2=Predictor2[(SplitDataAt+1):N])
    
    model=glm(Response~ Predictor1+Predictor2,data = Train)
    
    new <- data.frame(Predictor1 = TestSet$Predictor1,Predictor2 = TestSet$Predictor2)
    predicted=predict(model,new)
  }

  if(Summary){
    summary(model)
    # 1-pchisq(410883496,68)
    
    plot(model)
  }
  
  if(PlotIt){
    if(missing(Time)){
      # plot(TestSet$Response,type = 'l')
      # points(predicted,type='l',col='red')
      plotEvaluationFilteredTS(1:length(predicted),TestSet$Response,predicted,TRUE)
    }else{
      plotEvaluationFilteredTS(Time[(SplitDataAt+1):N],TestSet$Response,predicted,FALSE)
    }
  }
  x=predicted
  y=TestSet$Response
  Residuals=x-y
  AccuracyTrain=forecast::accuracy(predicted, TestSet$Response)
  # InspectVariable(abs(x-y)/max(y))
  return(invisible(list(Forecast=predicted,TestData=TestSet$Response,Accuracy=AccuracyTrain,Residuals=Residuals,Model=model)))
}
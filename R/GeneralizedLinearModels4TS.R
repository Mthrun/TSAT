GeneralizedLinearModels4TS=function(Response,SplitDataAt,Predictor1,Predictor2=NULL,CorrectionFactor=FALSE,PlotIt=TRUE,Time,Summary=FALSE,...){
  #response : Full$CallsAsIssues
  #predictiors:  Full$Temperature
  Rathena::GeneralizedLinearModels4TSv2(Response,SplitDataAt,Predictor1,Predictor2,CorrectionFactor,PlotIt,Time,Summary,...)
  # 
  # N=length(Response)
  # if(missing(Time)|is.null(Time)){
  #   
  #   TestSetTime=rep(NA,N-SplitDataAt)
  #   TrainingTime=rep(NA,SplitDataAt)
  # }else{
  #   TestSetTime=Time[(SplitDataAt+1):N]
  #   TrainingTime=Time[1:SplitDataAt]
  # 
  # }
  # 
  # if(is.null(Predictor2)){
  # Train=data.frame(Response=Response[1:SplitDataAt],Predictor1=Predictor1[1:SplitDataAt])
  # TestSet=data.frame(Response=Response[(SplitDataAt+1):N],Predictor1=Predictor1[(SplitDataAt+1):N])
  # 
  # model=glm(Response~ Predictor1,data = Train,...)
  # 
  # new <- data.frame(Predictor1 = TestSet$Predictor1)
  # predicted=predict(model,new)
  # 
  # }else{
  #   Train=data.frame(Response=Response[1:SplitDataAt],Predictor1=Predictor1[1:SplitDataAt],Predictor2=Predictor2[1:SplitDataAt])
  #   TestSet=data.frame(Response=Response[(SplitDataAt+1):N],Predictor1=Predictor1[(SplitDataAt+1):N],Predictor2=Predictor2[(SplitDataAt+1):N])
  #   
  #   model=glm(Response~ Predictor1+Predictor2,data = Train)
  #   
  #   new <- data.frame(Predictor1 = TestSet$Predictor1,Predictor2 = TestSet$Predictor2)
  #   predicted=predict(model,new)
  # }
  # if(CorrectionFactor)
  #   # Factor=mean(c((Response[SplitDataAt]/predicted[1]),1))
  #   Factor=Response[SplitDataAt]/predicted[1]
  # else
  #   Factor=1
  # 
  # predicted=predicted*Factor
  # 
  # if(Summary){
  #   summary(model)
  #   # 1-pchisq(410883496,68)
  #   
  #   plot(model)
  # }
  # 
  # if(PlotIt){
  #   if(missing(Time)){
  #     # plot(TestSet$Response,type = 'l')
  #     # points(predicted,type='l',col='red')
  #     plotEvaluationFilteredTS(1:length(predicted),TestSet$Response,predicted,TRUE)
  #   }else{
  #     plotEvaluationFilteredTS(Time[(SplitDataAt+1):N],TestSet$Response,predicted,FALSE)
  #   }
  # }
  # x=predicted
  # y=TestSet$Response
  # Residuals=x-y
  # AccuracyTrain=forecast::accuracy(predicted, TestSet$Response)
  # # InspectVariable(abs(x-y)/max(y))
  # return(invisible(list(Forecast=predicted,TestData=data.frame(Response=TestSet$Response,Time=TestSetTime),Accuracy=AccuracyTrain,Residuals=Residuals,Model=model,TrainData=data.frame(Response=Train$Response,Time=TrainingTime),CorrectionFactor=Factor)))
}

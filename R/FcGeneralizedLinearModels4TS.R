# res = FcGeneralizedLinearModels4TS(Response, SplitAt, Predictor1, Time)
#
# DESCRIPTION
# Generalized linear models for the forecast of time series in the multivariate case
#
# INPUT
# Response            [1:n] vector with an value of each time j in [1,n]
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# Predictor1          [1:n] vector with an value of each time j in [1,n]
# Predictor2          [1:n] vector with an value of each time j in [1,n]. Can also be NULL if not used
# OPTIONAL
# Time                [1:n] time vector in case of PlotIt=TRUE
# CorrectionFactor    Scalar, if TRUE: the predicted time series will begin at the same point as the last value of the training set. All other values will be mulitplied by a factor accordingly.
# PlotIt              FALSE (default), do nothing. TRUE: plots an evaluation of the forecast on the test data
# Summary             Output of glm is evaluated further
# ...                 Further arguments passed to glm
#
# OUTPUT
# Forecast            [k:n], the forecast, of the time interval [k,n] which was not used in the model
# TestSet             [k:n], the part of Response not used in the model
# Accuracy            ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix 
# Errors              Residuals: TestSet-Forecast
# Model               Output of glm
#
# DETAILS
# Assumption: Response and Predictors have the same time intervals, the same starting time and the same length.
#
# Author: MCT

FcGeneralizedLinearModels4TS=function(Response,SplitAt,Predictor1,Predictor2=NULL,Time,CorrectionFactor=FALSE,PlotIt=FALSE,Summary=FALSE,...){
  #response : Full$CallsAsIssues
  #predictiors:  Full$Temperature
  
  
  if(missing(SplitAt)) {
    warning('Input for SplitAt was not given. Setting SplitAt to length(DataVec)')
    SplitAt = length(Response)
  }
  if(missing(Time)) {
    Time = 1:length(Response)
    warning('No input for Time was given, will use 1:length(DataVec) for Time')
  }
  
  inputs = checkInputForecasting(Response, Time, SplitAt, 1, PlotIt)
  Response = inputs$DataVec
  Time = inputs$Time
  SplitAt = inputs$SplitAt
  PlotIt = inputs$PlotIt
  
  if(!is.vector(Predictor1) || mode(Predictor1)!="numeric") {
    stop('Input for Predictor1 should be a numeric vector')
  }

  
  if(!is.logical(CorrectionFactor)) {
    warning('Input for CorrectionFactor is not logical. Setting CorrectionFactor to FALSE')
    CorrectionFactor = FALSE
  }
  
  if(!is.logical(Summary)) {
    warning('Input for Summary is not logical. Setting Summary to FALSE')
    Summary = FALSE
  }
  
  
  
  N=length(Response)
  TestSetTime=Time[(SplitAt+1):N]
  TrainingTime=Time[1:SplitAt]


  if(is.null(Predictor2)){
    Train=data.frame(Response=Response[1:SplitAt],Predictor1=Predictor1[1:SplitAt])
    TestSet=data.frame(Response=Response[(SplitAt+1):N],Predictor1=Predictor1[(SplitAt+1):N])
    
    model=glm(Response~ Predictor1,data = Train,...)
  
    new <- data.frame(Predictor1 = TestSet$Predictor1)
    predicted=predict(model,new)
  
  }else{
    if(!is.vector(Predictor2) || mode(Predictor2)!="numeric") {
      stop('Input for Predictor2 should be a numeric vector')
    }
    
    Train=data.frame(Response=Response[1:SplitAt],Predictor1=Predictor1[1:SplitAt],Predictor2=Predictor2[1:SplitAt])
    TestSet=data.frame(Response=Response[(SplitAt+1):N],Predictor1=Predictor1[(SplitAt+1):N],Predictor2=Predictor2[(SplitAt+1):N])
    
    model=glm(Response~ Predictor1+Predictor2,data = Train)
    
    new <- data.frame(Predictor1 = TestSet$Predictor1,Predictor2 = TestSet$Predictor2)
    predicted=predict(model,new)
  }
  if(CorrectionFactor)
    # Factor=mean(c((Response[SplitAt]/predicted[1]),1))
    Factor=Response[SplitAt]/predicted[1]
  else
    Factor=1
  
  predicted=predicted*Factor
  
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
      plotEvaluationFilteredTS(Time[(SplitAt+1):N],TestSet$Response,predicted,FALSE)
    }
  }
  x=predicted
  y=TestSet$Response
  Residuals=x-y
  AccuracyTrain=forecast::accuracy(predicted, TestSet$Response)
  # InspectVariable(abs(x-y)/max(y))
  return(invisible(list(Forecast=predicted,TestData=data.frame(Response=TestSet$Response,Time=TestSetTime),Accuracy=AccuracyTrain,Residuals=Residuals,Model=model,TrainData=data.frame(Response=Train$Response,Time=TrainingTime),CorrectionFactor=Factor)))
}

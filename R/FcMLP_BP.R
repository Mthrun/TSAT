# res = FcMLP_BP(DataVec, SplitAt, Predictor1, Predictor2, Time)
#
# DESCRIPTION
# Foracasting by multilayer perceptron feedforward network with resilient backpropagation with weight backtracking.
#
# INPUT
# DataVec             [1:n] numerical vector of time series data
# SplitAt             Index of row where the DataVec is divided into test and train data. If not given n is used
# Predictor1          [1:n] vector with an value of each time j in [1,n]
# Predictor2          [1:n] vector with an value of each time j in [1,n]
# OPTIONAL
# Time                [1:n] character vector of Time in the length of data
# HiddenLayers        Number of hidden layers, see neuralnet::neuralnet
# Threshold           Threshold for the partial derivatives of ErrorFunction , please see neuralnet::neuralnet
# Repetitions         Number of repetitions for the neural network's training.
# ErrorFunction       Differentiable function that is used for the calculation of the error, string alternatives are 'ce' and 'sse', 
#                     please see neuralnet::neuralnet
# PlotEvaluation      Plot output of training for neuralnet
# PlotIt              A logical determining whether or not to plot the forecast in comparison to the validation set.
#
# OUTPUT
# Forecast            [k:n], the forecast, of the time interval [k,n] which was not used in the model
# TestSet             [k:n,1], the part of Response not used in the model
# TrainData           [1:k,1:3], the part of Response not used in the model
# TestDataOutput      output of compute
# Accuracy            ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix
# Model               Output of neuralnet
#
# DETAILS
# This functions trains a MLP/BP network and then forecasts a new sample of data using the trained ANN. Training and Testdata are splitted up 
# from the arguments using SplitAt Testing data is only used to compare against the forecast.
#
# Seems to be good for longterm forcasting. Short term forcasting does not work well.
#
# Author: MCT

FcMLP_BP=function(Response,SplitAt,Predictor1,Predictor2,Time,HiddenLayers=4,Threshold=0.05,Repetitions=10,ErrorFunction='sse',PlotEvaluation=FALSE,PlotIt=FALSE){
  
  n = length(Response)
  
  if(!is.logical(PlotEvaluation)) {
    warning('Input for PlotEvaluation is not logical. Setting PlotEvaluation to FALSE')
    PlotEvaluation = FALSE
  }
  if(!(is.numeric(HiddenLayers) && length(HiddenLayers) == 1 && HiddenLayers >= 0)) {
    warning('HiddenLayers should be a positive scalar. Setting HiddenLayers to 4')
    HiddenLayers = 4
  }
  if(!(is.numeric(Repetitions) && length(Repetitions) == 1 && Repetitions >= 0)) {
    warning('Repetitions should be a positive scalar. Setting Repetitions to 10')
    Repetitions = 10
  }
  
  if(length(Predictor1) != length(Predictor2)) {
    stop("Predictors have to be of equal length")
  }
  
  if(!is.vector(Predictor1) || !is.vector(Predictor2)) {
    warning('Input for Predictors are not a vector, parse to vector. If Matrix was given, all columns will be represented as single vector')
    Predictor1=as.vector(Predictor1)
    Predictor2=as.vector(Predictor2)
  }
  
  if(mode(Predictor1)!="numeric" || mode(Predictor2)!="numeric") {
    warning('Predicotrs are not a numeric vector. Trying to parse DateVec to numeric')
    Predictor1=as.numeric(Predictor1)
    Predictor2=as.numeric(Predictor2)
  } 
  
  if(length(Predictor1) != length(Response)) {
    warning('Predictors are not the same length as the response. Cutting predictors')
    Predictor1=Predictor1[1:n]
    Predictor2=Predictor2[1:n]
  } 
  
  if(missing(SplitAt)) {
    warning('Input for SplitAt was not given. Setting SplitAt to length(Response)')
    SplitAt = length(Response)
  }

  if(missing(Time)) {
    Time = 1:length(Response)
    warning('No input for Time was given, will use 1:length(Response) for Time')
  }
  
  inputs = checkInputForecasting(Response, Time, SplitAt, 1, PlotIt)
  Response = inputs$DataVec
  Time = inputs$Time
  SplitAt = inputs$SplitAt
  PlotIt = inputs$PlotIt
  
  errorMessage = packageMissingError("neuralnet", n, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        TestData = Response[n-SplitAt:n],
        TrainData = Response[1:SplitAt],
        Forecast = rep(NaN, length(Response)-SplitAt),
        TestDataOutput = NULL,
        Accuracy = 0,
        Model = NULL,
        Info = errorMessage
      )
    )
  }
  errorMessage = packageMissingError("forecast", n, SplitAt)
  if(errorMessage != FALSE){
    return(
      list(
        TestData = Response[n-SplitAt:n],
        TrainData = Response[1:SplitAt],
        Forecast = rep(NaN, length(Response)-SplitAt),
        TestDataOutput = NULL,
        Accuracy = 0,
        Model = NULL,
        Info = errorMessage
      )
    )
  }
  
  testSize = n-SplitAt
  
  DF=data.frame(Response=Response,Predictor1=Predictor1,Predictor2=Predictor2)
  
  TrainingsData=head(DF,nrow(DF)-testSize)
  out=neuralnet::neuralnet(data=TrainingsData,Response~Predictor1+Predictor2,hidden=HiddenLayers,threshold=Threshold,rep=Repetitions,err.fct = ErrorFunction)
  
  erg <- out$net.result[[1]]
  if(PlotEvaluation){
    plot(head(DF,nrow(DF)-testSize)$Response,type='l')
    points(erg,type='l',col='red')
  }
  forecast=neuralnet::compute(out,tail(DF,testSize)[,2:3])

  if(PlotIt){
    plot(1:testSize,tail(DF,testSize)[,1],type='l')
    points(1:testSize,forecast$net.result,type='l',col='red')
  }
  acc=forecast::accuracy(as.numeric(forecast$net.result),tail(DF,testSize)[,1])

  return(list(Forecast=forecast$net.result,TestData=tail(DF,testSize)[,1],TrainData=TrainingsData,TestDataOutput=forecast,Accuracy=acc,Model=out))  
}


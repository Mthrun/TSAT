CrostonIntermittentDemand=function(NumericVector,Time,Frequency= "days",Horizon=3*30,ModelType="sbj",PlotIt=FALSE,...){
  
  requireNamespace('TSAT')
  requireNamespace('tsintermittent')
  requireNamespace('forecast')
  requireNamespace('lubridate')
  
  n=length(NumericVector)
  if(n!=length(Time)){
    stop('Length of "Time" does not equal length of "NumericVector".')
  }
  if(!lubridate::is.Date(Time)){
    Time=as.Date(Time)
    warning('Time is not a "Date" object, Trying to transform with "as.Date"...')
  }
  if(sum(!is.finite(Time))!=0) stop('Time has NAs.')
  
  train=TSAT::ConvertNumerical2TSobject(head(NumericVector,n-Horizon),head(Time,n-Horizon),Frequency =Frequency)
  test=tail(NumericVector,Horizon)   
  
  m=tsintermittent::crost(train,h=Horizon,type=ModelType,outplot = PlotIt,...)
  fc=m$frc.out
  acc=forecast::accuracy(f = fc,test)
  testfull=TSAT::ConvertNumerical2TSobject(test, tail(Time,Horizon),Frequency =Frequency)
  return(list(Forecast=fc,Accuracy=acc,TestSet=testfull,Model=m,TrainingSet=train))
}
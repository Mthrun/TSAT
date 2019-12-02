CrostonIntermittentDemand=function(NumericVector,Time,ForecastHorizon,SplitAt,Frequency= "days",ModelType="sbj",PlotIt=FALSE,...){
  
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
  
  train=TSAT::ConvertNumerical2TSobject(head(NumericVector,SplitAt),head(Time,SplitAt),Frequency =Frequency)
  test=tail(NumericVector,n-SplitAt)[1:ForecastHorizon]   
  ttime=tail(Time,n-SplitAt)[1:ForecastHorizon]
  
  m=tsintermittent::crost(train,h=ForecastHorizon,type=ModelType,outplot = PlotIt,...)
  fc=m$frc.out
  #acc=forecast::accuracy(f = fc,test)
  #testfull=TSAT::ConvertNumerical2TSobject(test, tail(Time,SplitAt),Frequency =Frequency)
  DF=data.frame(Time=ttime,FF=fc,TestData=test)
  return(list(Forecast=DF,Model=m,TrainingSet=train))
}
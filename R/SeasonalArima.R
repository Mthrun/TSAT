SeasonalArima=function(TimeSeries,Horizon,TestandTrain=TRUE,PlotIt=TRUE,...){
  requireNamespace('seasonal')
  requireNamespace('CombMSC')
  requireNamespace('forecast')
  n=length(TimeSeries)
  if(isTRUE(TestandTrain)){
    TaT=CombMSC::splitTrainTest(TimeSeries,n-length(tail(TimeSeries,Horizon)))
    Train=TaT$train
  }else{
    Train=TimeSeries
    TaT=list(train=TimeSeries,test=NULL)
  }
#https://www.census.gov/ts/x13as/docX13AS.pdf
  #he  default  is  one  year  of  forecasts  (unless  a  SEATS
  #seasonal adjustment is requested - then the default is three years of forecasts) and 120 is
  # the maximum. 
  #setting forecast.maxlead lower results in 3 year forecast
  m <- seasonal::seas(Train, forecast.save = "forecasts",forecast.maxlead = Horizon,forecast.maxback=0,...)
  ff=seasonal::series(m, "forecast.forecasts")
  
  if(isTRUE(PlotIt)){
    plot(TimeSeries)
    fplot=CombMSC::splitTrainTest(ff[,"forecast"],Horizon)
    points(fplot$train,col='red')
  }
  if(isTRUE(TestandTrain)){
    Acc=forecast::accuracy(fplot$train,TaT$test)
  }else{
    Acc=NULL
  }
  return(list(Forecast=fplot$train,Accuracy=Acc,TaT=TaT,Model=m,ForecastMaxlead=ff))
}
FcAutoARIMA=function(Data,Time,ForecastHorizon,SplitAt,PlotIt=TRUE,Seasonal=TRUE,PlotBackwardInd,main='',xlab='Time',ylab='Data',...){
  # author: MT 2014/ edited 2018, edited Dec, 2019
  DataFrame=data.frame(Time=Time,Data=Data)
  requireNamespace('forecast')
  requireNamespace('zoo')
  #requireNamespace('fanplot')
  if(missing(PlotBackwardInd))#
    PlotBackwardInd=nrow(DataFrame)
  
  n=nrow(DataFrame)
  test=tail(DataFrame,n = n-SplitAt)
  train= with(head(DataFrame,SplitAt), zoo::zoo(head(Data,SplitAt), order.by = head(Time,SplitAt)))

  fit=forecast::auto.arima(train,seasonal=Seasonal)
  future=forecast::forecast(fit,h=ForecastHorizon)
  if(PlotIt){
    plot(tail(DataFrame$Time,PlotBackwardInd),tail(DataFrame$Data,PlotBackwardInd),type='l',main=paste(main,future$method,' - black calls, red predicton, blue 85% confindence interval'),xlab=xlab,ylab=ylab,...)
    points(tail(DataFrame$Time,ForecastHorizon),future$mean,type='l',col='red')
    points(tail(DataFrame$Time,ForecastHorizon),future$lower[,1],type='l',col='blue')
    points(tail(DataFrame$Time,ForecastHorizon),future$upper[,1],type='l',col='blue')
  }
  # #Doku, Under Development
  # #author MT 2014 and 2018
  # #Quelle die besser als eigener Ansatz ist: DataVisualization in R
  # warning('Under Development')
  # TStrans=GenerateRegularTS(Data,TimeChar)
  # requireNamespace('forecast')
  # requireNamespace('fanplot')
  # fit=forecast::auto.arima(TStrans)
  # fore=matrix(NA,nrow=Par1,ncol=Par2)
  # for(i in 1:Par1){
  #   fore[,i]=simulate(fit,nsim=Par2)
  # }
  # if(PlotIt){
  #   plot(TStrans,xlim=TimeXlim,main=main)
  # fanplot::fan(fore,type='intervall',start=StartForcastVec,anchor=2.28)
  # }
  # if(is.null(TestTime)) TestTime=rep(NaN,ForecastHorizon)
  # if(is.null(TestData)) TestData=rep(NaN,ForecastHorizon)
  # DF=data.frame(Time=TestTime,)
  FF=zoo::coredata(future$mean)
  Forecast=data.frame(Time=test$Time[1:ForecastHorizon],FF=FF[1:ForecastHorizon],Y=test$Data[1:ForecastHorizon])
  return(invisible(list(Forecast=Forecast,ArimaObject=future,Model=fit)))
}
autoARIMA=function(Data,Time,ForecastHorizon,PlotIt=T,PlotBackwardInd,main='',xlab='Time',ylab='Data',...){
  # author: MT 2014/ edited 2018
  DataFrame=data.frame(Time=Time,Data=Data)
  requireNamespace('forecast')
  requireNamespace('zoo')
  #requireNamespace('fanplot')
  if(missing(PlotBackwardInd))#
    PlotBackwardInd=nrow(DataFrame)
  
  if(missing(ForecastHorizon))
    ForecastHorizon=round(0.2*length(Data))
  
  df=DataFrame[1:(nrow(DataFrame)-ForecastHorizon),]
  df= with(df, zoo::zoo(Data, order.by = Time))
  
  fit=forecast::auto.arima(df)
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
  return(invisible(list(Forecast=future,Model=fit)))
}
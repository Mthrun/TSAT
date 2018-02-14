autoARIMA=function(Data,TimeChar,Par1=20,Par2=5,TimeXlim,StartForcastVec=c(2018,2),PlotIt=T,main){
  
  #Doku, Under Development
  #author MT 2014 and 2018
  #Quelle die besser als eigener Ansatz ist: DataVisualization in R
  warning('Under Development')
  TStrans=GenerateRegularTS(Data,TimeChar)
  requireNamespace('forecast')
  requireNamespace('fanplot')
  fit=forecast::auto.arima(TStrans)
  fore=matrix(NA,nrow=Par1,ncol=Par2)
  for(i in 1:Par1){
    fore[,i]=simulate(fit,nsim=Par2)
  }
  if(PlotIt){
    plot(TStrans,xlim=TimeXlim,main=main)
  fanplot::fan(fore,type='intervall',start=StartForcastVec,anchor=2.28)
  }
  return(list(Fore=fore,fit=fit))
}
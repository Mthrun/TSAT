GARCH=function(Data,VarianceModel=list(model = "sGARCH", garchOrder = c(1, 1)),NoDataForecastHorizon=100,PlotIt=TRUE){
  #Doku, Under Development

  # author: MT 2014
  #Quelle: http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
  warning('Under Development')
  RelSchluss=dbt.Transforms::relDiff4Prices(Data)
  requireNamespace('rugarch') 
  # Initialisierung
  spec <- rugarch::ugarchspec(variance.model = VarianceModel, distribution.model = 'norm')
  
  # Analyse der Renditen
  garchfit <- rugarch::ugarchfit(spec=spec,data=RelSchluss,solver.control=list(trace=0),show=TRUE)
  str(garchfit) #Was ist in hier gespeichert?
  if(PlotIt)
    plot(garchfit,which="all")
  # Sum Squares Error betrachten, sind die Resduuen um Null herum?
  forc = rugarch::ugarchforecast(garchfit, n.ahead=NoDataForecastHorizon) # this means that 100 data points are left from the end with which to
  if(PlotIt)
    plot(forc)
  return(list(Garchfit=garchfit,Forc=forc))
}
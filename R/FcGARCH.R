FcGARCH=function(Data,VarianceModel=list(model = "sGARCH", garchOrder = c(1, 1)),ForecastHorizon, DistributionModel = 'norm',PlotIt=TRUE,Summary=FALSE,...){
  #Doku, Under Development

  # author: MT 2014
  #Quelle: http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
  #warning('Under Development')
  #RelSchluss=dbt.Transforms::relDiff4Prices(Data)
  if(missing(ForecastHorizon))
    ForecastHorizon=round(0.2*length(Data))
  
  RelSchluss=DiffFilter(Data)
  requireNamespace('rugarch') 
  # Initialisierung
  spec <- rugarch::ugarchspec(variance.model = VarianceModel, distribution.model = DistributionModel,...)
  
  # Analyse der Renditen
  garchfit <- rugarch::ugarchfit(spec=spec,data=RelSchluss,solver.control=list(trace=0),show=TRUE)
  #str(garchfit) #Was ist in hier gespeichert?
  if(Summary)
    rugarch::plot(garchfit,which="all")
  # Sum Squares Error betrachten, sind die Resduuen um Null herum?
  forc = rugarch::ugarchforecast(garchfit, n.ahead=ForecastHorizon) # this means that 100 data points are left from the end with which to
  if(PlotIt)
    rugarch::plot(forc,which=1)
  return(invisible(list(Forecast=forc,Model=garchfit)))
}
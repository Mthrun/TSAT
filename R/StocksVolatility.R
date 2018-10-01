StocksVolatility=function(AdjClose,NumberOfDay=10){
  
  #ToDo: Verstehen und Anwenden
  requireNamespace('TTR')
  return(TTR::volatility(AdjClose,n=NumberOfDays,calc='close',N=260,mean0=FALSE))
  
}
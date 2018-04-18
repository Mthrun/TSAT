autoCompoundModelOLD=function(Data,TimeChar,PlotIt=T){
  #Doku, Under Development
  #author MT 2018
  warning('Under Development')
  #Note: It seems that manually is necessary
  requireNamespace('zoo')
  requireNamespace('DataVisualizations')
  
  TStrans=GenerateRegularTS(Data,TimeChar)
  Key=1:length(TStrans)
  Period=c(as.numeric(TStrans),as.numeric(TStrans)[order(Key,decreasing = T)])
  plot(Period)
  DoublePeriod=c(Period,Period[length(Period):1])
  plot(DoublePeriod)

  TSperiod=zoo::zoo(DoublePeriod,order.by = 1:length(DoublePeriod),frequency = 4)
  if(PlotIt)
    plot(TSperiod)
  
  d=decompose(TSperiod)
  #d=decompose(datastats,type=multiplicative)
  if(PlotIt){
    plot(d)
    DataVisualizations::InspectVariable(d$random,N = 'Residuum')
  }
  
  return(list(Trend=d$trend,
  Seasonal=d$seasonal,
  Residum=d$random))
}
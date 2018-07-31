GrangerCausalityTest=function(TimeSeries1,TimeSeries2,Order = 1)
  {
  if(length(TimeSeries1)!=length(TimeSeries2)) stop('Length of TimeSeries1 does not equal length of TimeSeries2')
  

    DF=data.frame(TimeSeries1,TimeSeries2)
    Header=c('TimeSeries1','TimeSeries2')
    colnames(DF)=Header
    requireNamespace('lmtest')
    # lmtest::grangertest(TimeSeries1, TimeSeries2, order = order)
    out1=lmtest::grangertest(TimeSeries2~TimeSeries1, order = Order)

    requireNamespace('vars')
	requireNamespace('MASS') #ginv function
	
    test=vars::VAR(cbind(TimeSeries1,TimeSeries2),p=Order)
    out2=vars::causality(test,cause='TimeSeries1')

  return(list(WaldTest=out1,VARtest=out2))
}
CointegrationOfTwoTS=function(TS1,TS2,alpha=1, type='c',PlotIt=TRUE){
  #do not search for right alpha with PLS statistics
  #regression analysis fails if ts are non stationary
  CointegrationOrder1=TS1-alpha*TS2
  requireNamespace('fUnitRoots')
  requireNamespace('tseries')
  adf=fUnitRoots::adfTest(CointegrationOrder1,lags=3,type=type)
  adf2=tseries::adf.test(CointegrationOrder1, alternative = "stationary",k=0)
  if(PlotIt){
    requireNamespace('DataVisualizations')
    #if ts1und ts2 cointegrated then linear combination must be stationary
    DataVisualizations::InspectVariable(CointegrationOrder1,N = 'Residuals') #use also dick-fueller test
  }
  return(list(CointegrationOrder1,adf_test=list(adf_fUnitRoots=adf,adg_tseries=adf2)))
}
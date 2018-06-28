CointegrationOfTwoTS=function(TS1,TS2,alpha=1,PlotIt=TRUE){
  #do not search for right alpha with PLS statistics
  #regression analysis fails if ts are non stationary
  CointegrationOrder1=TS1-alpha*TS2
  if(PlotIt){
    #if ts1und ts2 cointegrated then linear combination must be stationary
    InspectVariable(CointegrationOrder1,N = 'Residuals') #use also dick-fueller tet
  }
  return(CointegrationOrder1)
}
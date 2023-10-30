# V = CointegrationOfTwoTS(TS1, TS2, alpha = 1, type='c',PlotIt = TRUE)
#
# Description:
# Investigates the Cointegration between two time series. If TS1 and TS2 are cointegrated then 
# a linear combination must be stationary.
#
# INPUT
# TS1               [1:n] vector of data
# TS2               [1:n] vector of data
# alpha             linear factor, do not search for right alpha with PLS statistics.
# type              character string describing the type of UnitrootTests (the unit root regression). 
#                   Valid choices are "nc" for a regression with no intercept (constant) nor time trend, 
#                   and "c" for a regression with an intercept (constant) but no time trend, 
#                   "ct" for a regression with an intercept (constant) and a time trend. The default is "c".
# PlotIt            TRUE: plot distribution analysis of CointegrationOrder1 (which seems to be the residuals?)

# OUTPUT 
# CointegrationOrder1   TS1-alpha*TS2
# adf_fUnitRoots	      Check cointegration with UnitrootTests
# adg_tseries           Check cointegration with adf.test
#
# Details: 
# "In the econometric literature, a time series zt is said to be an integrated process of
# order 1, that is, an I(1) process, if (1−B)zt is stationary and invertible. In general, a
# univariate time series zt is an I(d) process if (1−B)dzt is stationary and invertible,
# where d > 0. The order d is referred to as the order of integration or the multiplicity
# of a unit root. A stationary and invertible time series is said to be an I(0) process."
# A cointegration relationship exists between two (or more) time series when there is a long-term 
# balance between two (or more) transient (integrated) variables. Cointegration is applied to detrend
# time series (non-stationary time series). It represents an alternative to a detrending through 
# the computation of differences (\code{\link{DiffFilter}}). Trend correction is often proposed in order to 
# avoid false regressions. 
#
#
# Author: MCT

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
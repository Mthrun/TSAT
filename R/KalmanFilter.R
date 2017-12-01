KalmanFilter=function(TimeSeries,start,gam0,Fscalar,gamV,G,gamW){
  #Kalman filter program to predict, filter, and smooth related to the material in Section 10.6 4 in Applied Time Series Analysis with R, second edition by Woodward, Gray, and Elliott
  # Input
  # TimeSeries  the univariate data set to be analyzed
  # start	      the scalar version of Xo in item (c) following the state equation (10.47) of the text
  # gam0	      the scalar version of Gamma(0) discussed in item (c) following the state equation
  # Fscalar	    scalar version of the matrix F in the state equation
  # gamV	      the value Gamma(v) specified in item (b) following the state equation
  # G	          the scalar observation matrix specified in the observation equation as G(t)
  # gamW	      the variance of the (univariate) white noise denoted by Gamma(w) in item (c) following (10.48)
  # OUTPUT
  # pfs	
  # a table giving results such as those in Table 10.1 in Woodward, Gray, and Elliott book
  # MT: still only a working solution, to be expented
  return(tswge::kalman.wge(y=TimeSeries, start, gam0, Fscalar, gamV, G, gamW))
}
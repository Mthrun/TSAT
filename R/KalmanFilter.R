KalmanFilter=function(Datavector,start=0,gam0=1,Fscalar=0.9,gamV=1,G=1,gamW=0.75,PlotIt=TRUE,Short=FALSE){
  #Kalman filter program to predict, filter, and smooth related to the material in Section 10.6 4 in Applied Time Series Analysis with R, second edition by Woodward, Gray, and Elliott
  # Input
  # Datavector  [1:n], the univariate data set to be analyzed
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
  # info: https://magesblog.com/post/2015-01-06-kalman-filter-example-visualised-with-r/
  output=tswge::kalman.wge(y=Datavector,start=start, gam0=gam0, F=Fscalar, gamV=gamV, G=G, gamW=gamW)
  
  if(PlotIt){
    plotEvaluationFilteredTS(1:length(Datavector),output[,"Prediction"],output[,"Data"],Short = Short,MarkedPoints = NULL)
  }
  return(invisible(list(FilteredData=output[,"Prediction"],FULL=output)))
}
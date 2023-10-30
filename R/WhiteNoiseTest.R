# V=WhiteNoiseTest=(TimeSeries,lags=1,type="c",PlotIt=TRUE)
#
# DESCRIPTION
# Test fails to reject the null hypothesis of no white noise if p-value < 0.05. Consequently, 
# b the data is a (Gaussian) white noise if a p-value is below 0.05.
#
# INPUT
# TimeSeries            [1:n] vector of data, e.g. residuals of time series
# OPTIONAL
# lags                	Number of lags to investigate in the statistical test. Default is 1
# type                  A character string describing the type of UnitrootTests (the unit root regression). 
#                       Valid choices are "nc" for a regression with no intercept (constant) nor time trend, 
#                       and "c" for a regression with an intercept (constant) but no time trend, 
#                       "ct" for a regression with an intercept (constant) and a time trend. The default is "c"  
# PlotIt                If TRUE plots the gaussian in relation to the TimeSeries data.
#                       If each sample has a normal distribution with zero mean, the signal is said to be Gaussian white noise [Diebold, 2007].
#                       Default is TRUE
#
# OUTPUT
# output of fUnitRoots::UnitrootTests
#
# DETAILS
# White noise can be described as a random process, e.g. Brownian Movement, Random Walk.
# The simplest unit-root nonstationary time series is the univariate random walk [Tsay, 2013].
# Therefore, using distribution analysis and a unit root test, this function can serve as a indication for white noise, 
# because unit root is a feature of white noise.
# If the mean is around zero (red line visible in plot) and the distribution gaussian (magenta line overlaps blue line) 
# and the pvalue is small than white noise can be assumed. 
# It is a difficult task to try to generally to proof white noise. Thus, if one of the two approaches (statistical versus visual) 
# do not agree, than the result is unclear and the residuals should be tested with other approaches.
#
#
# Author: MCT

WhiteNoiseTest=function(TimeSeries,lags=1,type="c",PlotIt=TRUE){
  requireNamespace('fUnitRoots')
  requireNamespace('DataVisualizations')
  out=NULL
  try({
    #Error in if (class(x) == "timeSeries") x <- series(x) :
  out=fUnitRoots::unitrootTest(x=TimeSeries,lags = lags,type = type
  )
  }
  )
  if(PlotIt){
    m <- mean(TimeSeries,na.rm=TRUE) ; # Schaetzung der Parameter der Normalverteilung
    s <- sd(TimeSeries,na.rm=TRUE) ;   # Schaetzung der Parameter der Normalverteilung
    if(!is.null(out))
      pval=out@test$p.value[1]
    else
      pval=1
    
    if(pval>0.001){
      pval=round(pval,4)
      string=paste('TimeSeries is Gaussian white noise, p-value of',pval[1])
    }else{
      string=paste('TimeSeries is Gaussian white noise, p-value < 0.001')
    }
    
    pdeVal        = DataVisualizations::ParetoDensityEstimation(TimeSeries)
    Normaldist <- dnorm(pdeVal$kernels,m,s)
    plot(
      pdeVal$kernels,
      pdeVal$paretoDensity,
      type = 'l',
      xaxs = 'i',
      yaxs = 'i',
      xlab = 'TimeSeries (blue), Gaussian distribution (magenta)',
      ylab = 'PDE',
      col = 'blue',
      main = string
    )
    points(pdeVal$kernels,Normaldist,type='l',col='magenta')
    abline(v =0,col='red')
  }
  return(invisible(out))
}
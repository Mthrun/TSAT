WhiteNoiseTest=function(TimeSeries,lags=1,type="c",PlotIt=TRUE){
  requireNamespace('fUnitRoots')
  requireNamespace('AdaptGauss')
  out=fUnitRoots::unitrootTest(x=TimeSeries,lags = lags,type = type)
  if(PlotIt){
    m <- mean(TimeSeries,na.rm=TRUE) ; # Schaetzung der Parameter der Normalverteilung
    s <- sd(TimeSeries,na.rm=TRUE) ;   # Schaetzung der Parameter der Normalverteilung
    
    pval=out@test$p.value[1]
    if(pval>0.001){
      pval=round(pval,4)
      string=paste('Residuals are not Gaussian white noise with a p-value of',pval[1])
    }else{
      string=paste('Residuals are not Gaussian white noise with a p-value < 0.001')
    }
    pdeVal        = AdaptGauss::ParetoDensityEstimation(TimeSeries)
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
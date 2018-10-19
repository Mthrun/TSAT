CrossCorr=function(FirstTimeSeries , SecondTimeSeries , nLags=2 ,type = "correlation",PlotIt=FALSE,main,...){
#MT auch ccf in R, naeher vergleichen
#CROSSCORR Compute or plot sample cross-correlation function.
#   Compute or plot the sample cross-correlation function (XCF) between
#   univariate, stochastic time series
#
# Inputs:
#   Series1 - Vector of observations of the first univariate time series for
#     which the sample XCF is computed or plotted. The last row of Series1
#     contains the most recent observation.
#
#   Series2 - Vector of observations of the second univariate time series for
#     which the sample XCF is computed or plotted. The last row of Series2
#     contains the most recent observation.
#
# Optional Inputs:
#   nLags - Positive, scalar integer indicating the number of lags of the XCF
#     to compute. If empty or missing, the default is to compute the XCF at
#     lags 0, +/-1, +/-2,...,+/-T, where T is the smaller of 20 or one less
#     than the length of the shortest series.
#
#   type: either "correlation" or "covariance"
#
# Outputs:
#   XCF - Sample cross correlation function between Series1 and Series2. XCF
#     is a vector of length 2*nLags + 1 corresponding to lags 0, +/-1, +/-2,
#     ... +/-nLags. The center element of XCF contains the zeroth lag cross
#     correlation. XCF will be a row (column) vector if Series1 is a row
#     (column) vector.
#
#   Lags - Vector of lags corresponding to XCF (-nLags to +nLags).
#
#   Bounds - Two element vector indicating the approximate upper and lower
#     confidence bounds assuming the input series are completely uncorrelated.
#
# Example:
#   Create a random sequence of 100 Gaussian deviates, and a delayed version
#   lagged by 4 samples. Then see the XCF peak at the 4th lag:
#
#     randn('state',100)               # Start from a known state.
#     x           = randn(100,1)       # 100 Gaussian deviates ~ N(0,1).
#     y           = lagmatrix(x , 4)   # Delay it by 4 samples.
#     y(isnan(y)) = 0                  # Replace NaN's with zeros.
#     crosscorr(x,y)                   # It should peak at the 4th lag.
#
# See also AUTOCORR, PARCORR, FILTER.
if(length(FirstTimeSeries)!=length(SecondTimeSeries)) warning('Timeseries do not have to same length.')
if(!is.vector(FirstTimeSeries)| !is.vector(SecondTimeSeries)) warning('One ore both Timeseries are not vectors, Plotting may not work, ccf function may not work.')

if(sum(!is.finite(FirstTimeSeries))!=0 | sum(!is.finite(SecondTimeSeries))!=0)    warning('One ore both Timeseries have NaN values, Please check if you used the parameter na.action = na.omit correctly for ccf.')

  res1=ccf(FirstTimeSeries, SecondTimeSeries, lag.max = nLags, type = type,
      plot = FALSE,...)
  ind=which.max(abs(res1$acf))
  if(missing(main))
    main=paste('Maximum of ccf(TS1,TS2) =',round(res1$acf[ind],2),'at lag =',ind-nLags)
  
  if(PlotIt){

    def.par <-par(no.readonly = TRUE)# save default, for resetting...
      m <-
        graphics::layout(matrix(c(1, 1, 2, 2), 2, 2))

    res1=ccf(FirstTimeSeries, SecondTimeSeries, lag.max = nLags, type = type,
             plot = PlotIt,main=main,...)
    
   
    #The lag k value returned by ccf(x, y) estimates the correlation between x[t+k] and y[t].
    TSslagged=cbind(LagVector(as.numeric(FirstTimeSeries),ind-nLags),as.numeric(SecondTimeSeries))
    TSslagged=TSslagged[complete.cases(TSslagged),]
    
    if(type == "correlation"){
    plot(TSslagged[,1],TSslagged[,2],xlab=paste0('lag(TS1,',ind-nLags,')'),ylab='TS2',main=paste('Spearman Correlation =',round(cor(TSslagged[,1],TSslagged[,2],method = 'spearman'),2),'at lag =',ind-nLags))
    }
    if(type == "covariance"){
      #requireNamespace('DataVisualizations')
      #DataVisualizations::DualAxisLineChart(1:nrow(TSslagged),TSslagged[,1],TSslagged[,2])
      plot(TSslagged[,1],type='l',col='black',xlab='Time',ylab=paste0('lag(TS1,',ind-nLags,')','(black) TS2(blue)'),main=paste('Spearman Covariance =',round(cov(TSslagged[,1],TSslagged[,2],method = 'spearman'),2),'at lag =',ind-nLags))
      points(TSslagged[,2],type='l',col='blue')
    }
    par(def.par)
   
  }
  bounds =  c(2,-2) / sqrt(length(FirstTimeSeries)) 
  return(invisible(res1))
  
  
  # res2=ccf(FirstTimeSeries, SecondTimeSeries, lag.max = nLags, type = "covariance",
  #     plot = PlotIt, na.action = na.fail)

}
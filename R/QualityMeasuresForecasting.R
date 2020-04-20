QualityMeasuresForecasting=function(Y,FOR,Indices,Mase_Season=1,NonDuplicatedForecasts=FALSE,Silent=TRUE){
#q=QualityMeasuresForecasting(Y,FOR)
# calculates the conventional quality measurements of forecasting
# INPUT
# Y						[1:f] numerical vector of test data for f time steps
# FOR					[1:f] numerical vector of forecast for f time steps
# Indices				Default [1:f], or subset pof that if required
# Mase_Season			MASE requires a seasoj an uses two formulas depending on it, if =1 no season is selected, see documentation of quality measurements for formulas
# Silent				TRUE: no warnings or pints, FALSE; prints for debugging
# OUTPUT
# q1					named vector of the following ordering of errors: 'RMSE','MAE','MAPE','SMAPE','MASE','AbsoluteError','Bias','RAE','RSE','MRD','MRD_BIAS'
# author:               Michael Thrun, www.IAP-GmbH.de  
  requireNamespace('forecast')
  requireNamespace('Metrics')
  
  if(!is.vector(Y)) Y=as.vector(Y)
  if(!is.vector(FOR)) FOR=as.vector(FOR)
  
  if(length(FOR)!=length(Y)){
    stop('Unequal length of forecast For and time series Y')
  }
  
  if(missing(Indices)) Indices=1:length(Y)
  if(max(Indices)>length(Y)){
    Indices=1:length(Y)
    warning('Indices out of range of data. setting to deafult')
  }
  if(min(Indices)<1){
    Indices=1:length(Y)
    warning('Indices out of range of data. setting to deafult')
  }
  # q1=c(NaN,NaN,NaN)
  # q2=NaN
  # q3=NaN 
  # q4=NaN
  # q5=NaN
  # q6=NaN
  # q7=NaN
  # q8=NaN
  # q9=NaN
  erorrs=rep(NaN,11)
  names(erorrs)=c('RMSE','MAE','MAPE','SMAPE','MASE','AbsoluteError','Bias','RAE','RSE','MRD','MRD_BIAS')
  # RMSE, MAE, MAPE
  tryCatch({
    erorrs[1:3]=forecast::accuracy(FOR[Indices],Y[Indices])[c(2,3,5)]
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #SMAPE
  tryCatch({
    erorrs[4]=mean(SMAPE(Y[Indices],FOR[Indices],na.rm = T,Silent=Silent))
    },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #MASE
  tryCatch({
    if(Mase_Season>1)
      erorrs[5]=Metrics::mase(actual = Y[Indices],predicted = FOR[Indices],step_size = Mase_Season)
    else
      erorrs[5]=MASE_nonseasonal(Y[Indices],FOR[Indices],NonDuplicatedForecasts = NonDuplicatedForecasts)
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #AbsoluteError
  tryCatch({
    erorrs[6]=abs((sum(FOR[Indices],na.rm = T)-sum(Y[Indices],na.rm = T))/sum(Y[Indices],na.rm = T))*100
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #BIAS
  tryCatch({
    #bias computes the average amount by which actual is greater than predicted.
    erorrs[7]=Metrics::bias(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #RAE
  tryCatch({
   # rae divides sum(ae(actual, predicted)) by sum(ae(actual, mean(actual))), meaning that it provides the absolute error of the predictions relative to a naive model that predicted the mean for every data point.
    erorrs[8]=Metrics::rae(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #RSE
  tryCatch({
    # rse divides sse(actual, predicted) by sse(actual, mean(actual)), meaning that it provides the squared error of the predictions relative to a naive model that predicted the mean for every data point.
    erorrs[9]=Metrics::rse(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
   # if(isFALSE(Silent))
      warning(e)
  })
  #MRD und bias2
  tryCatch({
    # rse divides sse(actual, predicted) by sse(actual, mean(actual)), meaning that it provides the squared error of the predictions relative to a naive model that predicted the mean for every data point.
    temp=RootDeviance(x= Y[Indices],y = FOR[Indices],Silent=Silent)
    erorrs[10]=temp$MRD
    erorrs[11]=temp$bias
  },error=function(e){
    #if(isFALSE(Silent))
      warning(e)
  })
  #q=c(q1,q2,q3,q4,q5,q6,q7,q8,q9)
  #names(q)=c('RMSE','MAE','MAPE','SMAPE','MASE','AbsoluteError','Bias','RAE','RSE','MRD','MRD_BIAS')
  return(erorrs)
}

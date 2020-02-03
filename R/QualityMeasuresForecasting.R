QualityMeasuresForecasting=function(Y,FOR,Indices,Mase_Season=1,Silent=FALSE){
  requireNamespace('forecast')
  requireNamespace('Metrics')
  
  if(!is.vector(Y)) Y=as.vector(Y)
  if(!is.vector(FOR)) FOR=as.vector(FOR)
  
  if(length(FOR)!=length(Y)){
    stop('Unequal length of forecast For and time series Y')
  }
  
  if(missing(Indices)) Indices=1:length(Y)
  q1=c(NaN,NaN,NaN)
  q2=NaN
  q3=NaN 
  q4=NaN
  q5=NaN
  q6=NaN
  q7=NaN
  q8=NaN
  q9=NaN
  
  # RMSE, MAE, MAPE
  tryCatch({
  q1=forecast::accuracy(FOR[Indices],Y[Indices])[c(2,3,5)]
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #SMAPE
  tryCatch({
    q2=TSAT::SMAPE(Y[Indices],FOR[Indices],na.rm = T,Silent=Silent)
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #MASE
  tryCatch({
    q3=Metrics::mase(actual = Y[Indices],predicted = FOR[Indices],step_size = Mase_Season)
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #AbsoluteError
  tryCatch({
    q4=abs((sum(FOR[Indices],na.rm = T)-sum(Y[Indices],na.rm = T))/sum(Y[Indices],na.rm = T))*100
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #BIAS
  tryCatch({
    #bias computes the average amount by which actual is greater than predicted.
    q5=Metrics::bias(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #RAE
  tryCatch({
   # rae divides sum(ae(actual, predicted)) by sum(ae(actual, mean(actual))), meaning that it provides the absolute error of the predictions relative to a naive model that predicted the mean for every data point.
    q6=Metrics::rae(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #RSE
  tryCatch({
    # rse divides sse(actual, predicted) by sse(actual, mean(actual)), meaning that it provides the squared error of the predictions relative to a naive model that predicted the mean for every data point.
    q7=Metrics::rse(actual = Y[Indices],predicted = FOR[Indices])
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  #MRD und bias2
  tryCatch({
    # rse divides sse(actual, predicted) by sse(actual, mean(actual)), meaning that it provides the squared error of the predictions relative to a naive model that predicted the mean for every data point.
    temp=TSAT::RootDeviance(x= Y[Indices],y = FOR[Indices],Silent=Silent)
    q8=temp$MRD
    q9=temp$bias
  },error=function(e){
    if(isFALSE(Silent))
      warning(e)
  })
  q=c(q1,q2,q3,q4,q5,q6,q7,q8,q9)
  names(q)=c('RMSE','MAE','MAPE','SMAPE','MASE','AbsoluteError','Bias','RAE','RSE','MRD','MRD_BIAS')
  return(q)
}

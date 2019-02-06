CommonForecastingErrors=function(TestdataY,ForecastingF,epsilon=10^-4,na.rm=TRUE,Stepsize=1){
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(TestdataY)&is.finite(ForecastingF))
    ForecastingF <- ForecastingF[noNaNInd]
    TestdataY <- TestdataY[noNaNInd]
  }
  AD=abs(TestdataY-ForecastingF)
  if(isTRUE(any(!is.finite(AD)))) stop('Some of your values are not finite. Please use "na.rm=TRUE".')
  
  ind=which(TestdataY<epsilon & (-TestdataY)>(-epsilon))
  if(length(ind)>0){
    warning('TestdataY value(s) is/are too small to calculate MAPE. Setting these value(s) to "epsilon".')
    TestdataYTMP=epsilon
  }else{
    TestdataYTMP=TestdataY
  }
  

  MAE=mean(AD,na.rm=na.rm)

  MAPE=mean(AD/abs(TestdataYTMP),na.rm=na.rm)
  smape=SMAPE(ForecastingF,TestdataY,na.rm = T,epsilon = epsilon)
  
  requireNamespace('Metrics')
  RMSE=Metrics::rmse(TestdataY,ForecastingF)
  
  u=unique(TestdataY)
  if(length(u)==1&length(TestdataY)>1){
    warning('Only one unique value in Test data. Adding some minor randomness defined by "epsilon" for Test data of MASE in order to get a finite value.')
    if(u==0)
      TestdataYmase=TestdataY+runif(length(TestdataY),min = u-epsilon,max = u+epsilon)
    else
      TestdataYmase=TestdataY+runif(length(TestdataY),min = -u*epsilon,max = u*epsilon)
    
    MASE=round(Metrics::mase(TestdataYmase,ForecastingF, step_size = Stepsize),2)
  }else{
    MASE=Metrics::mase(TestdataY,ForecastingF, step_size = Stepsize)
  }
  requireNamespace('Rathena')
  Bias=Rathena::RootDeviance(TestdataY,ForecastingF)$bias
  MRD=Rathena::RootDeviance(TestdataY,ForecastingF)$MRD
  ForecastErrors=c(MAE,MAPE,smape,MASE,RMSE,Bias,MRD)
  names(ForecastErrors)=c('MAE','MAPE','SMAPE','MASE','RMSE','BIAS','MRD')
  
  return(ForecastErrors)
  }

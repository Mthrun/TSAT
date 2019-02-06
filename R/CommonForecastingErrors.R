CommonForecastingErrors=function(TestdataY,ForecastingF,epsilon=10^-4,na.rm=TRUE,stepsize=1,digits){
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
  if(!missing(digits)){
    exponent=abs(signif(log(epsilon,base = 10),1))
    print(exponent)
    if(digits>=exponent){
      digits=exponent-1
      warning('Rounding has to be to less digits than the exponent of epsilon. Reducing number of digits for rounding.')
    }
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
    
    MASE=round(Metrics::mase(TestdataYmase,ForecastingF, step_size = stepsize),2)
  }else{
    MASE=Metrics::mase(TestdataY,ForecastingF, step_size = stepsize)
  }
  requireNamespace('Rathena')
  Bias=Rathena::RootDeviance(TestdataY,ForecastingF)$bias
  MRD=Rathena::RootDeviance(TestdataY,ForecastingF)$MRD
  ForecastErrors=c(MAE,MAPE,smape,MASE,RMSE,Bias,MRD)
  names(ForecastErrors)=c('MAE','MAPE','SMAPE','MASE','RMSE','BIAS','MRD')
  if(!missing(digits)){
    #Circumenvents Rs rule to round to even numbers in orter to represent an underlying continuous distribution
    #this means 1.5 is now rounded to 2 and 2.5 to 3 instead that bot values are rounded to 2
    ForecastErrors=round(ForecastErrors+epsilon*0.1,digits) 
  }

  return(ForecastErrors)
  }

VARIMA=function(Data,ARp,i=1,MAq,ForecastHorizont=14,PlotIt=TRUE,Time){
#VARIMA(Data, ARp, i = 1, MAq, ForecastHorizont = 14, PlotIt = TRUE, Time)
#Vector autoregessive moving average model with difference filter
#VARMA and VARIMA modelling for multivariate Forecasting
#details: Please read [Tsay, 2013].
#
#INPUT
# Data                    matrix [1:n,1:d]
# ARp                     numerical value, see example
# i                       numerical value, either zero: weakly stationary time series or 1 for non stationary
# MAq                     numerical value, see example
# ForecastHorizont        scalar 'f', forcasting period
# PlotIt                  TRUE: Evaluation plots
# Time                    Optional, for evaluation plots

#OUTPUT
  #   List V with
  #   \item{Model}{List with
  #     
  #     Model: Model output of \code{\link[MTS]{VARMA}}
  #     
  #     OptimizedModel: Further optimized Model of Model using \code{\link[MTS]{refVARMA}}
  #     
  #   }
  #   \item{Train}{[1:(N-f),1:d] Training data for building the model}
  #   \item{Test}{[(N-f+1):N,1:d] Evaluation Data of the Model}
  #   \item{Forecast}{[(N-f+1):N,1:d] Prediction of the Model}
  #   
  # }
# author:     Michael Thrun
# [Tsay, 2013]  Tsay, R. S.: Multivariate time series analysis: with R and financial applications, John Wiley & Sons, ISBN: 978-1-118-61790-8, 2013.




#VARMA and VARIMA modelling for multivariate Forecasting  
  requireNamespace('TSAT')
  requireNamespace('MTS')
  
  if(!is.matrix(Data)) stop('Please use TSAT::ARIMA for univariate models.')
  
  if(i<0) stop('Parameter i has to be positive')
  if(i>1) stop('Not implemented for i>1')
  if(i==0){
    DataTrans=Data
  }else{
    DataTrans=apply(Data, 2, TSAT::DiffFilter)
  }

  N=nrow(DataTrans)
  d=ncol(DataTrans)
  if(missing(Time)) Time=1:N
  
  
  TrainInd=N-ForecastHorizont
  Train=DataTrans[1:TrainInd,]
  Test=DataTrans[(1+TrainInd):N,]
  
  
  model=MTS::VARMA(Train, p = ARp, q = MAq, include.mean = T, fixed = NULL, beta=NULL, sebeta=NULL, prelim = F, details = F, thres = 2)
  
  modelRefined=MTS::refVARMA(model)
  
  pred=MTS::VARMApred(modelRefined,ForecastHorizont)
  
if(PlotIt){
  m <- graphics::layout(matrix(c(seq(from=1,to=d,by=1),seq(from=1,to=d,by=1)), d, 2))
  for(i in 1:d){
  plot(tail(Time,ForecastHorizont),Test[,i],type='l',xlab='Time',ylab=paste0(i,'th variable (black), prediction red'))
  points(tail(Time,ForecastHorizont),pred$pred[,i],type='l',col='red')
  }
  return(list(Model=list(Model=model,OptimizedModel=modelRefined),Train=Train,Test=Test,Forecast=pred))
}
}
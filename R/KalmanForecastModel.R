KalmanForecastModel=function(Data,Horizont=12,...){
  warning('UnderDevelopment')
  #Horizont: how many steps ahead?
  ## an ARIMA fit
  fit3 <- arima(Data, c(3, 0, 0))
  predict(fit3, Horizont)
  ## reconstruct this
  pr <- KalmanForecast(Horizont, fit3$model)
  return(pr)
}
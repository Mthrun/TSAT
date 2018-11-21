SMAPE=function(actual, forecast, digits = 3){
  smooth::SMAPE(actual, forecast, digits = 3)
  #for seasonal MASE see https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments

  # computeMASE <- function(forecast,train,test,period){
  #   
  #   # forecast - forecasted values
  #   # train - data used for forecasting .. used to find scaling factor
  #   # test - actual data used for finding MASE.. same length as forecast
  #   # period - in case of seasonal data.. if not, use 1
  #   
  #   forecast <- as.vector(forecast)
  #   train <- as.vector(train)
  #   test <- as.vector(test)
  #   
  #   n <- length(train)
  #   scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)
  #   
  #   et <- abs(test-forecast)
  #   qt <- et/scalingFactor
  #   meanMASE <- mean(qt)
  #   return(meanMASE)
  # }
}
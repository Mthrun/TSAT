MASE <- function(Forecast,TrainingSet,TestSet,SeasonalLength){
  
  # Forecast - Forecasted values
  # TrainingSet - data used for Forecasting .. used to find scaling factor
  # TestSet - actual data used for finding MASE.. same length as Forecast
  # SeasonalLength - in case of seasonal data.. if not, use 1
  
  Forecast <- as.vector(Forecast)
  TrainingSet <- as.vector(TrainingSet)
  TestSet <- as.vector(TestSet)
  
  n <- length(TrainingSet)
  scalingFactor <- sum(abs(TrainingSet[(SeasonalLength+1):n] - TrainingSet[1:(n-SeasonalLength)])) / (n-SeasonalLength)
  
  et <- abs(TestSet-Forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt)
  return(meanMASE)
}
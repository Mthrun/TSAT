# Derivative = Derivative(Data, FilterWindowSize=5, PlotIt=FALSE)
#
# Description:
# Calculates the empirical first derivative including moving average filtering
#
# INPUT
# Data(1:n)           A numeric time series object, or a numeric vector/matrix.
#
# OPTIONAL
# FilterWindowSize    Window size for the moving average filtering, default is 5.
# PlotIt              Boolean, TRUE if auto correlation plot should be printed, FALSE else. Default is FALSE.
#
# OUTPUT
# Dx                  Empirical 1st derivative
#
# Author: 

Derivative = function(Data, FilterWindowSize=5, PlotIt=FALSE){
  #Data = smooth(Data, 'moving', FilterWindowSize)  # moving window filter
  if (PlotIt) {
    DataPlot = Data
  }
  Data = MovingAverage(Data,FilterWindowSize)
  
  # Subtract the preceding row, not one scalar from all observations.
  # The first difference is zero because no earlier observation is available.
  if (is.matrix(Data)) {
    LastData <- Data[c(1L, head(seq_len(NROW(Data)), -1L)), , drop=FALSE]
  } else LastData <- Data[c(1L, head(seq_along(Data), -1L))]
  Dx <- Data - LastData
  
  # Filter the derivative
  #return(Dx = smooth(Dx, 'moving', FilterWindowSize)) # moving window filter
  Dx = MovingAverage(Dx, FilterWindowSize)
  if (PlotIt) {
    MinD = min(c(DataPlot,Dx), na.rm = T) *0.99
    MaxD = max(c(DataPlot,Dx), na.rm = T) *1.01
    plot.ts(DataPlot, ylim=c(MinD,MaxD), 
                      xlim=c(0,length(DataPlot)), 
                      main='Filtering (red) versus Data(blue)', 
                      col='blue', xlab='Time', ylab='Range of values')
    points(Dx, type='l', col='red', ylim=c(MinD,MaxD), xlim=c(0,length(DataPlot)))
  }
  
  return(Dx = Dx)

 }
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
  
  # Derivation i.e. change to last FiData
  LastData    = Data[length(Data)]  # TE on last FiData
  LastData[1] = Data[2]             # in beginning it's constant
  Dx = Data - LastData              # Derivation i.e. change to last Fix
  
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
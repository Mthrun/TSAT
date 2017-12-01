Derivative=function(Data,FilterWindowSize=5,PlotIt=FALSE){
# DData = Derivative(Data,FilterWindowSize)
# empirical first derivative including filtering
#
# INPUT
# Data(1:n)            time series,
#
# OPTIONAL
# FilterWindowSize                TE wird moving Average gefiltert mit dieser Fensterweite,
#                                 default ==5
#
# OUTPUT
# Dx                empirical 1st derivative

#FilterWindowSize # default laenge des Moving average filters
#Data = smooth(Data,'moving',FilterWindowSize)  # moving window filter
if(PlotIt) DataPlot=Data
Data=MovingAverage(Data,FilterWindowSize)
# Differenzieren i.e Aenderung zum Letzten FiData
LastData   =Data[length(Data)]    # TE am letzten FiData
LastData[1] = Data[2]       # am anfang ists constant
Dx = Data -LastData    # Differenzieren i.e Aenderung zum Letzten Fix
# Ableitung filtern
#return(Dx = smooth(Dx,'moving',FilterWindowSize)) # moving window filter
Dx=MovingAverage(Dx,FilterWindowSize)
if(PlotIt){
  MinD=min(c(DataPlot,Dx),na.rm=T)*0.99
  MaxD=max(c(DataPlot,Dx),na.rm=T)*1.01
  plot.ts(DataPlot,ylim=c(MinD,MaxD),xlim=c(0,length(DataPlot)),main='Filtering (red) versus Data(blue)',col='blue',xlab='Time',ylab='Range of values')
  points(Dx,type='l',col='red',ylim=c(MinD,MaxD),xlim=c(0,length(DataPlot)))
}

return(Dx = Dx)

 }
MovingAverage=function(Data, lag,PlotIt=FALSE){
# Moving Average: FilteredData(i) = mean(Data((i-lag-1):(i-1)
#
# INPUT
# Data[1:n,1]              time series, in colunns
# lag                      nr or previous data points to average
#
# OUTPUT
#  FilteredData[1:n,1]   the filtered data
# author: MT 2017, 

requireNamespace('signal')

FilterVector = c( matrix(1, nrow = 1, ncol = lag),0,matrix(0, nrow = 1, ncol = lag))
FilterVector = FilterVector /sum(FilterVector) 
FilteredData = signal::filter(FilterVector,1,Data) 
# jetzt noch anfang korrigieren
FilteredData[1] =mean(Data,na.rm=T)#Data[1]
for(i in 2:lag){
    FilterVector = c( matrix(1, nrow = 1, ncol = i),0,matrix(0, nrow = 1, ncol = i))
    FilterVector = FilterVector /sum(FilterVector) 
    FD           = signal::filter(FilterVector,1,Data) 
    FilteredData[i] = FD[i] 
} 
if(PlotIt){
  plot.ts(Data,ylim=c(min(Data,na.rm=T)*0.99,max(Data,na.rm=T)*1.01),xlim=c(0,length(Data)),main='Filtering (red) versus Data(blue)',col='blue',xlab='Time',ylab='Range of values')
  points(as.vector(FilteredData),type='l',col='red',ylim=c(min(Data,na.rm=T)*0.99,max(Data,na.rm=T)*1.01),xlim=c(0,length(Data)))
}
return(as.vector(FilteredData))
}
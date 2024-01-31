# FourierAnalysis = FourierAnalysis(Y, SamplingRate, AdjustWindow=FALSE, na.rm=FALSE, PlotIt=FALSE)
#
# Description:
# 
#
# INPUT
# Y[1:n]            Ordered time series values without time stamps.
# SamplingRate      Sampling rate of the Y values.
#
# OPTIONAL
# AdjustWindow      Boolean, if TRUE adjusts Y to (Y, rev(Y), Y, rev(Y)). Default is TRUE.
# na.rm             Boolean, if TRUE removes NaN values automatically through spline interpolation. Default is FALSE.
# PlotIt            Boolean, TRUE if plot should be printed, FALSE else. Default is FALSE.
#
# OUTPUT
# list with:
# PowerSpectrum     Power spectral density of the FFT.
# FFT               Return of the FastFourierTransformation function from TimeSeries.
# ggObject          ggplot2 plot object or NULL.
#
# Author: 

FourierAnalysis = function(Y, SamplingRate, AdjustWindow=FALSE, na.rm=FALSE, PlotIt=FALSE){
  N=length(Y)
  if(isTRUE(AdjustWindow)){
    
    Y=c(Y, rev(Y))
    Y=c(Y, Y)
    
    N=length(Y)
    FFT=FastFourierTransformation(Y, na.rm = na.rm)
   
    if(missing(SamplingRate))
      freq <- FFT$Frequencies
    else
      freq <- FFT$Frequencies*SamplingRate
    #print(freq[1:50])
    PSD <- cbind(freq,FFT$Amplitude)
    colnames(PSD)=c('Frequency','Amplitude')
    #first is DC, second is window frequency
    PSD=PSD[3:(floor(N/2)),]
  }else{
    FFT=FastFourierTransformation(Y, na.rm = na.rm)
    
    if(missing(SamplingRate))
      freq <- FFT$Frequencies
    else
      freq <- FFT$Frequencies*SamplingRate
    #print(freq[1:50])
    PSD <- cbind(freq,FFT$Amplitude)
    colnames(PSD)=c('Frequency','Amplitude')
    #first is DC, second is window frequency
    PSD=PSD[1:(floor(N/2)),]
  }
  
  if(isTRUE(PlotIt)){
    obj=ggplot2::ggplot(data = as.data.frame(PSD), ggplot2::aes_string('Frequency','Amplitude')) + ggplot2::geom_bar(stat = "identity")+ggplot2::ggtitle('Frequency Domain')
  print(obj)
  }else{
    obj=NULL
  }
  
  return(list(PowerSpectrum=PSD, FFT=FFT, ggObject=obj))
}

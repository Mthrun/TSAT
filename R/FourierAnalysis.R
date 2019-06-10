FourierAnalysis=function(Y,SamplingRate,na.rm = FALSE,PlotIt=FALSE){
  N=length(Y)
  Y=c(Y,rev(Y))
  Y=c(Y,Y)
  FFT=FastFourierTransformation(Y,na.rm = na.rm)
 
  if(missing(SamplingRate))
    freq <- FFT$Frequencies
  else
    freq <- FFT$Frequencies*SamplingRate
  PSD <- cbind(freq,FFT$Amplitude)
  colnames(PSD)=c('Frequency','Amplitude')
  PSD=PSD[-1,]
  #PSD=PSD[-1,]
  #nquist frequency
  PSD=PSD[3:(floor(N/2)+1),]
  if(isTRUE(PlotIt)){
    obj=ggplot2::ggplot(data = as.data.frame(PSD), ggplot2::aes_string('Frequency','Amplitude')) + ggplot2::geom_bar(stat = "identity")+ggplot2::ggtitle('Frequency Domain')
  print(obj)
  }else{
    obj=NULL
  }
  return(list(PowerSpectrum=PSD,FFT=FFT,ggObject=obj))
}

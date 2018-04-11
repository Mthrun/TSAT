FastFourierTransformation=function(Timeseries,na.rm=FALSE,PlotIt=FALSE){
 # V=FastFourierTransformation(Timeseries)
# V=FastFourierTransformation((Timeseries,na.rm=TRUE,PlotIt=FALSE)
# Version= 1
# Calculates the FFT
#
# INPUT:                               
# Timeseries        [1:n] Zeitlich geordnete Werte ohne Zeitstempel als vector
#
# OPTIONAL
# na.rm                
# PlotIt                 
# OUTPUT list V with:
# V$Frequenzspektrum                 
# V$Amplitude                     
# V$KomplexeFourierKoeffizienten
# V$Data[1:4n] na removed and transformed data      
# 
# AUTHOR: MT 09/2017 
  if (!is.vector(Timeseries, mode = "numeric")){
    warning("Argument 'Timeseries' must be numeric vector. Trying to transform to a vector...")
    Timeseries=as.vector(Timeseries,mode =  "numeric")
  } 
  if(na.rm==TRUE){
    requireNamespace('imputeTS')
    Timeseries=imputeTS::na.interpolation(Timeseries,option = 'spline')
  }else{
    if(sum(!is.finite(as.numeric(Timeseries)))){
      warning('NaNs in Data. FFT will fail!')
    }
  }

  #Trick 17: verfierfache ZR damit auf jedenfall periodisch
  # see windowing effect
  Timeseries=c(Timeseries,Timeseries[length(Timeseries):1])
  timeseries=c(Timeseries,Timeseries[length(Timeseries):1])
  
  fourierComponents=fft(timeseries)
  KomplexeFourierKoeffizienten=as.complex(fourierComponents)
  N = length(KomplexeFourierKoeffizienten)
  #get the absolute value of the coefficients  
  Amplitude = abs(KomplexeFourierKoeffizienten)
  ##Freqenz
  frequenz=c(1:N)
  
  #Nyquist 0.5 * N * f, f=1/Periode
  
  #Nyquist=round(0.5*N*(1/Periode))

  #ind=seq(from=2,to=Nyquist,by=1)
  if(PlotIt){
    #geht nur bis N/2, ohne 1.Frequenc (DC anteil)
    #1.Oberschwingungen werden ignoriert
    plot(cbind((frequenz[2:N/2])/N,Amplitude[2:N/2]), t="h", lwd=2, 
         xlab="Frequency (Hz)", ylab="Amplitude", 
         ylim=c(0,max(Amplitude)),main=' Amplitude versus Frequenz')
  }
  return(list(Frequenzspektrum=frequenz,Amplitude=Amplitude,KomplexeFourierKoeffizienten=KomplexeFourierKoeffizienten,Data=timeseries))
}
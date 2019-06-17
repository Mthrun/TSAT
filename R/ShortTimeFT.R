ShortTimeFT=function(Datavec,LenWindow=8,TypeWindow='gaussian',GaussianApha=1,PlotIt=TRUE,Window,...){
 
  switch(TypeWindow,
         hanning={
           requireNamespace('e1071')
           out=e1071::stft(Datavec,win = LenWindow,...)
           Spec=out$values
         },
         gaussian={
           #GaborTransformation
           requireNamespace('signal')
           Window=signal::gausswin(n = LenWindow,w = GaussianApha)
           out=signal::specgram(y,window = Window,...)
           ColorRamp = colorRamps::blue2green2red(1024)
           Spec=abs(out$S)
           Spec=t(Spec)
         },
         specific={
           out=signal::specgram(y,window = Window,...)
           ColorRamp = colorRamps::blue2green2red(1024)
           Spec=abs(out$S)
           Spec=t(Spec)
         },
         {
           requireNamespace('signal')
           out=signal::specgram(y,n =LenWindow,...)
           Spec=abs(out$S)
           Spec=t(Spec)
         })
if(PlotIt){
  requireNamespace('colorRamps')
  ColorRamp = colorRamps::blue2green2red(1024)
  image(Spec, axes = F,col = ColorRamp,xlab = 'Time',ylab='Frequency')
}
  
  return(list(Amplitude=Spec,Spectrogram=out))
}
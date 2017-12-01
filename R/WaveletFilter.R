 WaveletFilter= function(TimeSeries,wavelet="haar",CountOrPercent=0,largest=FALSE, PlotIt=FALSE,Fancy=FALSE,loess.span=0,lowerPeriod=16,upperPeriod=1024,dj = 1/250){
# V=WaveletFilter(TimeSeries,largest=TRUE, PlotIt=T)
# V=WaveletFilter(TimeSeries,largest=F, PlotIt=T)
# Filter time series by keeping only the largest or first n(#) DWT coefficients.
#
# INPUT
# TimeSeries          time series if parameter L missing, otherwise wavelet coefficients
# wavelet           wavelet base to use, e.g. "haar" or "la8"
                    # Wavelet FamiliesWavelets
                    # Daubechies	
#   
# OPTIONAL
# CountOrPercent    keep largest n coefficients if > 1
#                   keep largest n# is # < 1
#                   do nothing =1
# largest           flag whether to use largest or first, default is FALSE
# PlotIt            if PlotIt ==1 do an overlay plot of original and filtered, default =0
# If fancy =FALSE, use waveslim package
# if fancy=TURE   use WaveletComp package, that set
   #loess.span = 0 : There is no need to detrend this series. 

   # lowerPeriod = 16, upperPeriod = 128 : This defines the range of periods used in the wavelet transformation.  Only periods of x within this range will be detected.  This range reaches from 16 = 2 4 to 128 = 2 7 .  To define period limits in terms of powers of 2 is convenient (see dj below), but no necessity. 
   # dj = 1/250 :  The  period  range  comprises  the  three  "octaves" [2 4 , 2 5 ) , [2 5 , 2 6 ) , [2 6 , 2 7 ) ,  to- gether with the value 2 7 .  Each octave is divided into 250 suboctaves with bounds given in vec- tor my.w$Period on a logarithmic scale.  In other words: my.w$Period has length 751, and all entries of the vector diff(log(my.w$Period)) are equal. Graphically, the argument dj thus determines the resolution along the period axis. 
   
# OUTPUT
# FilteredTS                      filtered time series
# WaveletDecompositionVector        wavelet coefficients
# BookkeepingVector               Scaling coefficient vector.
# author: MT 2017
if(!Fancy){
   n=length(TimeSeries)
   indices = 1:n
   requireNamespace('waveslim')
   x=TimeSeries
   #computation for nondyadic TimeSeries
   M <- length(x)
   N <- 2^(ceiling(log(M, 2)))
   xx <- c(x, rep(0, N - M))
   # do the Wavelet Transform
   y=waveslim::dwt(xx, wf=wavelet, n.levels=1, boundary="periodic")  
   #Backtransformation does not work :-(
   # J <- length(y) - 1
   # for (j in 1:J) y[[j]] <- y[[j]][1:trunc(M/2^j)]
   res=y

  if(CountOrPercent!=0){
    warning('Does not work yet')
  }
   if(CountOrPercent <= 1){
       CountOrPercent = round(CountOrPercent*N)
  }
   
   
   if(largest){
     res$d1[1:length(res$d1)]=0
     WaveletDecompositionVector=res$s1
     BookkeepingVector=NULL
   }else{
     res$s1[1:length(res$d1)]=0
     BookkeepingVector=res$d1
     WaveletDecompositionVector=NULL
   }
   
   FilteredTS=waveslim::idwt(res)
   #Backtransformation: all values behind that are zeros
   FilteredTS=FilteredTS[1:n]
   if(PlotIt){
    if(CountOrPercent>0){
       main=paste('filtering with', wavelet, 'wavelet', CountOrPercent, 'coefficients kept')
    }else{
      main=paste('wavelet filtering with', wavelet ,'wavelet ')
    } #if CountOrPercent>0
     plot.ts(TimeSeries,type='l',ylim=c(min(TimeSeries,na.rm=T)*0.99,max(TimeSeries,na.rm=T)*1.01),xlim=c(0,length(TimeSeries)),main=main,col='blue',xlab='Time',ylab='Range of values_ Filtering (red) versus Data(blue)')
     points(as.vector(FilteredTS),type='l',col='red',ylim=c(min(TimeSeries,na.rm=T)*0.99,max(TimeSeries,na.rm=T)*1.01),xlim=c(0,length(TimeSeries)))
    #requireRpackage('tswge')
     #tswge::plotts.dwt.wge(waveslim::up.sample(TimeSeries, 16, 0),n.levels=4,type='S8')
   }
    
}else{
  #loess.span = 0 : There is no need to detrend this series. 
  # dt = 1 : one observation of x is made per time unit. (This defines the time unit.) 
  # lowerPeriod = 16, upperPeriod = 128 : This defines the range of periods used in the wavelet transformation.  Only periods of x within this range will be detected.  This range reaches from 16 = 2 4 to 128 = 2 7 .  To define period limits in terms of powers of 2 is convenient (see dj below), but no necessity. 
  # dj = 1/250 :  The  period  range  comprises  the  three  "octaves" [2 4 , 2 5 ) , [2 5 , 2 6 ) , [2 6 , 2 7 ) ,  to- gether with the value 2 7 .  Each octave is divided into 250 suboctaves with bounds given in vec- tor my.w$Period on a logarithmic scale.  In other words: my.w$Period has length 751, and all entries of the vector diff(log(my.w$Period)) are equal. Graphically, the argument dj thus determines the resolution along the period axis. 
  # make.pval = T, n.sim = 10 :  The region of significant periods in x for each t (the area within the white line in Figure 4) is found using 10 simulations.
  requireNamespace('WaveletComp')
   # x = WaveletComp::periodic.series(start.period = 500, length = 4747)
   # x = x + 0.2*rnorm(500)  # add some noise

   my.data = data.frame(x = TimeSeries)
   my.w = WaveletComp::analyze.wavelet(my.data, "x",
                          loess.span = loess.span,
                          dt = 1, dj =dj,
                          lowerPeriod = lowerPeriod,
                          upperPeriod = upperPeriod,
                          make.pval = F, n.sim = 1)

if(PlotIt)
  WaveletComp::wt.image(my.w, color.key = "quantile", n.levels = 250,
            legend.params = list(lab = "wavelet power levels", mar = 4.7))
   
   WaveletDecompositionVector=my.w
   BookkeepingVector=WaveletComp::reconstruct(my.w, plot.waves = F, lwd = c(1,2), legend.coords = "bottomleft")
   FilteredTS=BookkeepingVector$series$x.r
}  
return(list(FilteredTS=FilteredTS, WaveletDecompositionVector=WaveletDecompositionVector, BookkeepingVector=BookkeepingVector))
}
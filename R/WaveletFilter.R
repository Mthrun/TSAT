#WaveletFilter = function(timeSeries, wavelet = "haar", percent = '12.5')
#
#INPUT
#timeSeries DATA
#wavelet    STRING choose wavelet from library
#		        Haar-Wavelet 'haar'
#	        	Daubechy family 'db4', 'la8', 'la16', 'la20'
#	        	'w4'
#	        	'd6', 'd8', 'd16'
#	        	'fk4', 'fk6', 'fk8', 'fk14', 'fk22' 
#	        	'mb4', 'mb8', 'mb16', 'mb24'
#	        	'bl14', 'bl20'
#	        	'bs3.1'
#
#percent  	STRING choose the frequency band as follows:
#		        '6.25' choose the 6.25% lowest frequencies
#	        	'12.5' choose the 12.5% lowest frequencies
#	        	'25' choose the 25% lowest frequencies
#	        	'50' choose the 50% lowest frequencies
#
#plot        BOOLEAN create plot if TRUE, DEFAULT=FALSE
#
#OUTPUT
#Plot Orignal Series
#Plot Reconstructed Series from chosen frequency band
#
#REQUIRED NAMESPACE  "waveslim" https://cran.r-project.org/web/packages/waveslim/index.html

WaveletFilter = function(timeSeries, wavelet = 'haar', percent = '12.5', plot = FALSE){
  
  N=length(timeSeries)
  timeSeries_rev <- rev(timeSeries)
  timeSeries_processed <- rbind(timeSeries, timeSeries_rev)
  timeSeries_processed_rev <- rev(timeSeries_processed)
  timeSeries_processed <- rbind(timeSeries_processed, timeSeries_processed_rev)
  
  timeSeries_processed_length = length(timeSeries_processed)
  floor_of_exponent = floor(log2(timeSeries_processed_length))
  length_of_power_two = 2**floor_of_exponent
  timeSeries_processed = timeSeries_processed[1:length_of_power_two]

  output=switch(percent,
         '6.25' = {
           WaveletDecompositionVector <- waveslim::dwpt(timeSeries_processed, wavelet, n.levels = 4)
           timeSeries_processed.basis <- waveslim::basis(WaveletDecompositionVector, c("w4.0"))
           Reconstruct(WaveletDecompositionVector, timeSeries_processed.basis, timeSeries_processed, plot,N)
         },
         '12.5' = {
           WaveletDecompositionVector <- waveslim::dwpt(timeSeries_processed, wavelet, n.levels = 3)
           timeSeries_processed.basis <- waveslim::basis(WaveletDecompositionVector, c("w3.0"))
           Reconstruct(WaveletDecompositionVector, timeSeries_processed.basis, timeSeries_processed, plot,N)
         },
         '25' = {
           WaveletDecompositionVector <- waveslim::dwpt(timeSeries_processed, wavelet, n.levels = 2)
           timeSeries_processed.basis <- waveslim::basis(WaveletDecompositionVector, c("w2.0"))
           Reconstruct(WaveletDecompositionVector, timeSeries_processed.basis, timeSeries_processed, plot,N)
         },
         '50' = {
           WaveletDecompositionVector <- waveslim::dwpt(timeSeries_processed, wavelet, n.levels = 1)
           timeSeries_processed.basis <- waveslim::basis(WaveletDecompositionVector, c("w1.0"))
           Reconstruct(WaveletDecompositionVector, timeSeries_processed.basis, timeSeries_processed, plot,N)
         },
         stop("Invalid selection for percent"))
  return(output)
}

Reconstruct = function(WDV, tsbasis, timeseries, plotbool,N){
  for(i in 1:length(WDV))
    WDV[[i]] <- tsbasis[i] * WDV[[i]]
  FilteredTS <- waveslim::idwpt(WDV, tsbasis)
  if(plotbool == TRUE){
    par(mfrow=c(2,1), mar=c(5-1,4,4-1,2))
    plot.ts(timeseries[1:N], xlab="", ylab="", main="Original Series")
    plot.ts(FilteredTS[1:N], xlab="", ylab="", main="Reconstructed Series")
  }
  return(list(FilteredTS=FilteredTS[1:N],WDV=WDV))
}
#
#
#
#
#

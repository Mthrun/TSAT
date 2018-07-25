#WaveletFilter = function(timeSeries, wavelet = "haar", level, plot)
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
#level      level of decomposition for reconstruction of specific frequency bands of timeseries
#	    choose the depth of level depending on the depth of frequencies
#	    the higher the level, the lower the frequency bands reconstructed
#
#
#plot       BOOLEAN create plot if TRUE, DEFAULT=FALSE
#
#OUTPUT
#Plot Wavelet Decomposition Vector
#Plot Bookkeeping Vector with lengths of Wavelet Decomposition Vectors
#
#REQUIRED NAMESPACE  "waveslim" https://cran.r-project.org/web/packages/waveslim/index.html
#REQUIRED NAMESPACE  "WaveletComp" https://cran.r-project.org/web/packages/WaveletComp/

WaveletFilter = function(timeSeries, wavelet = 'd6', level = 1, plot = FALSE){

  my_wt = analyze.wavelet(my.data = data.frame(timeSeries))
  dev.new(width=5, height=4)
  wt.image(my_wt)
  
  #Double the timeSeries twice (enlarge timeSeries length by factor 4)
  timeSeries_rev <- rev(timeSeries)
  timeSeries_processed <- rbind(timeSeries, timeSeries_rev)
  timeSeries_processed_rev <- rev(timeSeries_processed)
  timeSeries_processed <- rbind(timeSeries_processed, timeSeries_processed_rev)

  #force length of power two because of dyadic algorithm
  timeSeries_processed_length = length(timeSeries_processed)
  floor_of_exponent = floor(log2(timeSeries_processed_length))
  length_of_power_two = 2**floor_of_exponent
  timeSeries_processed = timeSeries_processed[1:length_of_power_two]
  
  #choose coefficients
  level_basis = paste('w', level, '.0', sep = '')
  timeSeries_processed = timeSeries
  
  #compute Wavelet Decomposition
  WaveletDecompositionVector <- waveslim::dwpt(timeSeries_processed, wavelet,
                                               n.levels = level, boundary = 'reflection')
  timeSeries_processed.basis <- waveslim::basis(WaveletDecompositionVector,level_basis)
  Reconstruct(WaveletDecompositionVector, timeSeries_processed.basis,
              timeSeries_processed, plot)
  
  BookKeepingVector = lengths(WaveletDecompositionVector)
  return(list(WaveletDecompositionVector, BookKeepingVector))
}

Reconstruct = function(WDV, tsbasis, timeseries, plotbool){
  for(i in 1:length(WDV))
    WDV[[i]] <- tsbasis[i] * WDV[[i]]
  FilteredTS <- waveslim::idwpt(WDV, tsbasis)
  if(plotbool == TRUE){
    dev.new(width=5, height=4)
    par(mfrow=c(2,1), mar=c(5-1,4,4-1,2))
    plot.ts(timeseries, xlab="", ylab="", main="Original Series")
    plot.ts(FilteredTS, xlab="", ylab="", main="Reconstructed Series")
  }
  return(FilteredTS=FilteredTS)
}
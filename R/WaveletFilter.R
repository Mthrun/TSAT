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
Rathena::WaveletFilterv2(timeSeries, wavelet, level, plot)
}

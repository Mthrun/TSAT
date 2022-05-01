WaveletOutlierDetection = function(Data, Factor = 1.5, Strategy="mean",
                                   Order = 7, Filter="haar", NumLevels=2,
                                   Boundary="periodic", Fast=T, PlotIt=F,
                                   Threshold="zero", Lambda=0.05,
                                   FilterLevels="all"){
  # DESCRIPTION
  # 
  # INPUT
  # Data[1:n]    Numeric vector with signal filtered with wavelet method
  # Factor       Numeric defining the factor by which a data point needs to 
  #              exceed the data in order to be recognized as outlier.
  # Strategy     Character defining the strategy by which single data points
  #              compared to data. Either, a data point is compared to the mean
  #              of the filtered data over a certain time range (moving average
  #              which time interval is defined by argument Order) around the
  #              data point or data points are compared to the filtered data 
  #              points at their respective time.
  # Order        Integer defining the window size computed for the moving
  #              average.
  # Filter       \code{\link{WaveletFilter}}
  # NumLevels    \code{\link{WaveletFilter}}
  # Boundary     \code{\link{WaveletFilter}}
  # Fast         \code{\link{WaveletFilter}}
  # PlotIt       \code{\link{WaveletFilter}}
  # Threshold    \code{\link{WaveletFilter}}
  # Lambda       \code{\link{WaveletFilter}}
  # FilterLevels \code{\link{WaveletFilter}}
  # 
  # OUTPUT
  # Outlier[1:n]    Numeric vector with same length as the original signal
  #                 carrying only the outliers.
  # 
  # Author: QMS 06.10.2021
  
  # 1. Filter signal using wavelets
  Filtered = TSAT::WaveletFilter(Data         = Data,
                                 Filter       = Filter,
                                 NumLevels    = NumLevels,
                                 Boundary     = Boundary,
                                 Fast         = Fast,
                                 PlotIt       = PlotIt,
                                 Threshold    = Threshold,
                                 Lambda       = Lambda,
                                 FilterLevels = FilterLevels)
  # 2. Decide the definition of an outlier
  # Strategy=="mean": An outlier is x times bigger than the mean around it
  if(Strategy=="mean"){
    Filtered = forecast::ma(Filtered, order = Order)
    Filtered = forecast::na.interp(Filtered)
  }
  # Strategy=="None": An outlier is x times bigger than the actual value
  
  IdxOutliers = which(Data > (Factor*Filtered))
  Outliers = rep(0, length(Data))
  Outliers[IdxOutliers] = Data[IdxOutliers]
  return(Outliers)
}

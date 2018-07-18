FourierFilter = function(Data,
                         CountOrPercent = 0.1,
                         largest = TRUE,
                         AdaptAmp = FALSE,
                         na.rm = TRUE,
                         PlotIt = FALSE) {
  # FilteredData = ft_filter(Data, CountOrPercent, largest,AdaptAmp) #
  # Filter time series by keeping only the largest or first of FFT coefficients.
  #
  # INPUT
  # Data[1:n]          real time series or complex fourier coefficients
  #
  # OPTIONAL
  # CountOrPercent     if > 1 : keep largest n coefficients
  #                    if in [0:1] keep largest percentage of coefficients
  #                   default:  CountOrPercent=0.2
  # largest           if ==TRUE(default): keep largest coefficients (BandPass)
  #                   otherwise:       keep first   coefficients (LowPass)
  # AdaptAmp          ==TRUE means amplitude and DC component of FilteredData is adapted to fit Data
  #                   default ==TRUE
  # PlotIt            Plots Fourier versus Data
  # OUTPUT
  # FilteredData[1:n]      FilteredData time series
  
  #  MT 2017, reimplementtiert matlab Code von ALU 2014
  
  # duch vervierfachen vorn und hinten anpassen
  
  AnzData = length(Data)
  Necht = length(Data)
  ListeV = FastFourierTransformation(Data, na.rm = na.rm)
  Fourier = ListeV$KomplexeFourierKoeffizienten
  Data = ListeV$Data
  N = length(Fourier)
  #Mirrored  = [Data flipud(Data)]
  #Mirrored  = c(Data,Data[seq(from=length(Data),to=1,by=-1)])
  #Data  = [Mirrored flipud(Mirrored)]
  #Data  = c(Mirrored, Mirrored[seq(from=length(Mirrored),to=1,by=-1)])
  # do fft, hier anpassungen zu taetigen
  #Fourier = fft(Data)
  
  # calculate how many to keep if not already done
  if (CountOrPercent <= 1) {
    CountOrPercent = round(CountOrPercent * Necht)
  }
  
  
  if (largest == TRUE) {
    # use largest
    #[dummy indices] = sort(abs(Fourier))
    Amplitude = ListeV$Amplitude
    indices = order(Amplitude, na.last = T, decreasing = T)
    #indices = flipud(fliplr(indices))
    indicestmp = c(indices, indices[seq(from = length(indices),
                                        to = 1,
                                        by = -1)])
    indices = c(indicestmp, indicestmp[seq(from = length(indicestmp),
                                           to = 1,
                                           by = -1)])
  } else{
    # use first
    indices = 1:N
  }
  
  # drop coefficients
  Fourier[indices[CountOrPercent + 1:N]] = 0
  
  # reconstruct time series
  #FilteredData = real(ifft(Fourier))
  FilteredData = Re(fft(Fourier, inverse = TRUE) / (N))
  #print(length(FilteredData))
  #print(length(Data))
  #FilteredData=Fourier
  # anpassen der Amplidude und des DC anteils, wenn gewuenscht
  if (AdaptAmp == TRUE) {
    #warning('Does not work yet!')
    #p = polyfit(FilteredData,Data,1)  # Interpolationsgerade schaetzt Amplitudenfaktor und DC anteil
    ptemp <- lm(FilteredData ~ poly(Data, degree = 1, raw = FALSE))
    p <- rev(ptemp$coefficients)
    #print(p)
    FilteredData = FilteredData * p[1] + p[2] #f(x)=x*m+b
  }  #if AdaptAmp==1
  # back to original
  FilteredData = FilteredData[1:AnzData]
  Data = ListeV$Data[1:AnzData]
  if (PlotIt) {
    plotEvaluationFilteredTS(1:length(Data),Data,FilteredData,Short = FALSE,MarkedPoints = NULL)
  }
  return(FilteredData)
}
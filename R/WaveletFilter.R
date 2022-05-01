WaveletFilter = function(Data, Filter="haar", NumLevels=2, Boundary="periodic",
                         Fast=T, PlotIt=F, Threshold="zero", Lambda=0.05,
                         FilterLevels="all"){
  # DESCRIPTION
  # 
  # INPUT
  # Data[1:n]    Signal filtered with wavelets
  # Filter       Character containing name of wavelet
  # NumLevels    Integer determining the number of wavelet levels to create.
  # Boundary     A character string indicating which boundary method to use.
  #              boundary = "periodic" and boundary = "reflection" are the only
  #              supported methods at this time.
  # Fast         A logical flag which, if true, indicates that the pyramid
  #              algorithm is computed with an internal C function. Otherwise,
  #              only R code is used in all computations.
  # PlotIt       Boolean flag. If TRUE then plots will be generated. Default=F.
  # Threshold    Character with filtering strategy. Default: Threshold="zero"
  #                  Threshold="zero": sets complete level to zero
  #                  Threshold="hard": performs hard thresholding
  #                  Threshold="soft": performs soft thresholding
  # Lambda       Numeric value. If filtering uses hard or soft thresholding,
  #              Lambda indicates the p-th percentile which is used as
  #              threshold. Every value in level below this is zero
  #              (both for hard and soft thresholding). Values greater or equal
  #              are treated accordingly to soft/hard thresholding.
  # FilterLevels Generic: FilterLevels="all" character: chooses all wavelet
  #                       levels for filtering
  #                       FilterLevels[1:n] numeric vector: chooses all wavelet
  #                       levels which are given in vector by setting it to a
  #                       positive number.
  # 
  # OUTPUT
  # FilteredData[1:n]    Signal filtered with wavelets
  # 
  # Author: QMS 06.10.2021
  
  # Plot original data
  if(PlotIt == T){
    plot(Data, type="l")
  }
  
  # Apply wavelet decomposition (Redundant Wavelet Transform == ModWT)
  RWT = wavelets::modwt(X = as.numeric(Data), filter=Filter, n.levels=NumLevels,
                        boundary=Boundary, fast=Fast)
  
  # Plot wavelet decomposition
  if(PlotIt == T){
    par(mfrow=c(NumLevels+1,1))
    for(i in 1:NumLevels){
      plot(1:length(RWT@W[[i]]),RWT@W[[i]], type="l", xlab = paste0("Waveletlevel", i), ylab = "")
    }
    plot(1:length(RWT@V[[NumLevels]]),RWT@W[[NumLevels]], type="l", xlab = paste0("Smooth approximation level", i), ylab = "")
  }
  
  # Decide the filtering
  if(is.character(FilterLevels)){
    if(FilterLevels == "all"){
      for(i in 1:NumLevels){
        if(Threshold=="hard"){
          RWT@W[[i]] = hard_thresholding(RWT@W[[i]], Lambda)
        }else if (Threshold == "soft"){
          RWT@W[[i]] = soft_thresholding(RWT@W[[i]], Lambda)
        }else{
          RWT@W[[i]] = filter_zero(RWT@W[[i]])
        }
      }
    }
  }else{
    if(is.vector(FilterLevels)){
      for(i in FilterLevels){
        if(Threshold=="hard"){
          RWT@W[[i]] = hard_thresholding(RWT@W[[i]], Lambda)
        }else if (Threshold == "soft"){
          RWT@W[[i]] = soft_thresholding(RWT@W[[i]], Lambda)
        }else{
          RWT@W[[i]] = filter_zero(RWT@W[[i]])
        }
      }
    }
  }
  
  # Reconstruct signal from wavelet decomposition
  IRWT = wavelets::imodwt(RWT)
  
  # Plot filtered signal
  if(PlotIt == T){
    plot(IRWT, type="l")
  }
  return(IRWT)
}

filter_zero = function(Data){
  Data = matrix(0,length(Data), 1)
  return(Data)
}

hard_thresholding = function(Data, Lambda){
  HardThreshold = quantile(abs(Data), probs = Lambda)
  HardThreshold = quantile(abs(Data), probs = 1)
  Data[abs(Data) < HardThreshold] = 0
  return(Data)
}

soft_thresholding = function(Data, Lambda){
  HardThreshold = quantile(abs(Data), probs = Lambda)
  Data[abs(Data) < HardThreshold] = 0
  for(j in 1:length(Data)){
    if(abs(Data[j])>=HardThreshold){
      Data[j] = sign(Data[j])*(abs(Data[j]-HardThreshold))
    }
  }
  return(Data)
}

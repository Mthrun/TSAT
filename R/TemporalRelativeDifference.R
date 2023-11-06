RelativeDifference4TS=function(Data,Lag=1,na.rm=FALSE,PlotIt=FALSE,Time,Silent=FALSE){
#V=RelativeDifference4TS(ElectricityBRD$Mrd_KWh,PlotIt=TRUE,Time=(ElectricityBRD$Time))
#V=RelativeDifference4TS(cbind(ElectricityBRD$Mrd_KWh,ElectricityBRD$Mrd_KWh))
#Calculates the relative difference between positive Data and and lagged Data
#INPUT
# Data                numerical matrix of [1:n,1:d], d>=1
# Lag                 The number of lags (in units of observations): back shifting: positiv value,forward shifting: negative value
#                     Default is the difference to yesterday
# na.rm               FALSE; do nothing, TRUE: all nan to zero
# PlotIt              TRUE: plots data
# Silent              TRUE: not warnings or messages
#OUTPUT
# Reldiff             numerical matrix of [1:n,1:d], Negative value indicate that today is lower than yesterday 
#                     and positive values that todays value is higher than yesterdays.

#Details:
# Contrary to linear filters in this cases the range of values lies always between [-2,2]
# If vector is given as input, vector is given back. If matrix is given as input, PlotIt is forced to be set as FALSE.
#
#  Author: MCT, 2023
#
  if (!requireNamespace("DatabionicSwarm", quietly = TRUE)) {
    # If the package is not installed, install it
    install.packages("DatabionicSwarm")
  }
  
  
  if (is.matrix(Data)) {
    PlotIt = FALSE #no plotting
    Rel = apply(
      X = Data,
      2,
      FUN =  RelativeDifference4TS,
      Lag = Lag,
      na.rm = na.rm
    )
    return(Rel)
  } else{#Data is vector
    neg = which(Data < 0)
    if (length(neg) > 0) {
      Data[neg] = 0
      if(isFALSE(Silent))
        warning(
          "RelativeDifference4TS works only for positive values, negatives are set to zero per default which my be incorrect."
        )
    }
    nan_ind = which(!is.finite(Data))
    
    if (length(nan_ind) > 0) {
      if (isTRUE(na.rm)){
        Data[nan_ind] = 0
        if(isFALSE(Silent))
          message(
            "RelativeDifference4TS works only for positive values correctly but missing values were found. These cases are set to zero."
          )
      }else{
        if(isFALSE(Silent))
          warning(
            "RelativeDifference4TS works only for positive values correctly but missing values were found. These cases are automatically removed."
          )
      }
  

    }
    
    Today = Data
    Yesterday = LagVector(Data, k = Lag)
    
    #no nan
    if (Lag > 0) {
      Yesterday[1:Lag] = Yesterday[Lag + 1]
    }
    if (Lag < 0) {
      indNan = tail(1:length(Yesterday), Lag)
      Yesterday[indNan] = Yesterday[min(indNan) - 1]
    }
    if (packageVersion("DatabionicSwarm") != "1.3.0") {
      #no silent parameter
      Rel = DatabionicSwarm::RelativeDifference(Y = Today, X = Yesterday, na.rm = F)
    }else{
      #silent parameter
      Rel = DatabionicSwarm::RelativeDifference(Y = Today, X = Yesterday, 
                                                na.rm = F,Silent=Silent)
    }
    if (isTRUE(na.rm))
      Rel[!is.finite(Rel)] = 0
    
    if (isTRUE(PlotIt)) {
      if (missing(Time)) {
        Time = 1:length(Data)
      }
      Name = deparse1(substitute(Data))
      a = DataVisualizations::DualaxisLinechart(
        Time,
        Data,
        Rel,
        y1lab = Name,
        y2lab = paste0("RelDiff of ", Name, " and its ", "Lag = ", Lag),
        xlab = 'Time'
      )
      print(a)
    }
    return(Rel)
  }#end is matrix
}


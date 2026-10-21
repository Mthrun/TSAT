TemporalForce = function(Data, na.rm = FALSE, Eps = 0.0001, Time, PlotIt = FALSE, Silent = FALSE) {
  # V=TemporalForce(ElectricityBRD$Mrd_KWh,PlotIt=TRUE,Time=(ElectricityBRD$Time))
  # V=TemporalForce(cbind(ElectricityBRD$Mrd_KWh,ElectricityBRD$Mrd_KWh))
  # Calculates the second order relative change force that represents the turning point of the timeseries
  # INPUT
  # Data                numerical matrix of [1:n,1:d], d>=1; if vector named or matrix has rownames and Time input not given, names are transferred to he Reldiff output
  # OPTIONAL
  # Lag                 The number of lags (in units of observations): back shifting: positiv value,forward shifting (causality is broken if used in forecasting"): negative value
  #                     Default is the difference to yesterday
  # Time                character vector 1:n defining the name, is only used to name the Reldiff output and for plotting
  # na.rm               FALSE; do nothing, TRUE: all nan to zero
  # Eps                 Default: 0.0001, Force for which data is varying around eps is set to zero
  # PlotIt              If TRUE: plots data, works only for d=1, otherwise it is internally set to FALSE
  # Silent              If TRUE: not warnings or messages
  # OUTPUT
  # Force             numerical matrix of [1:n,1:d], Negative value indicate that today is lower than yesterday
  #                     and positive values that todays value is higher than yesterdays.

  # Details:
  # If vector is given as input, vector is given back. If matrix is given as input, PlotIt is forced to be set as FALSE.
  # The Force is defined as   Force=(Today-2*Yesterday+DayeforeYesterday)/Yesterday

  ##  Author: MCT, 03/2024
  # Kulkarni and Hairdar, 2009: Forecasting Model for Crude Oil Price Using Artificial Neural Networks and Commoditiy Futures Prices
  # Analysis functions must not install software or change the user's library.
  if (!requireNamespace("DatabionicSwarm", quietly = TRUE))
    stop("Install the optional package 'DatabionicSwarm' to use this method.", call. = FALSE)


  if (is.matrix(Data)) {
    PlotIt = FALSE # no plotting
    Rel = apply(
      X = Data,
      2,
      FUN = TemporalForce,
      Eps = Eps,
      na.rm = na.rm,
      Silent = Silent
    )
    if (!missing(Time)) {
      rownames(Rel) = Time
    } else if (!is.null(rownames(Data))) {
      rownames(Rel) = rownames(Data)
    } else {
      # do noting
    }

    return(Rel)
  } else { # Data is vector

    neg_ind = which(Data < 0)
    if (length(neg_ind) > 0) {
      if (isTRUE(na.rm)) {
        Data[neg_ind] = 0
      }
      if (isFALSE(Silent))
        message(
          "TemporalRelativeDifference works only for positive values correctly but negative values were found. These cases are set to zero."
        )
    } else {}
    nan_ind = which(!is.finite(Data))

    if (length(nan_ind) > 0) {
      if (isTRUE(na.rm)) {
        Data = imputeTS::na_interpolation(Data, option = 'linear')
      }
      if (isFALSE(Silent))
        message(
          "TemporalRelativeDifference works only for positive values correctly but missing values were found. These cases are linearly interpolated."
        )
    } else {}

    Today = Data
    # lag =1:set yesterday as today
    # LagVector(c("t-3","t-2","t-1","t"),1)
    Yesterday = LagVector(Today, k = 1)
    DayeforeYesterday = LagVector(Today, k = 2)
    # no nan
    # if (Lag > 0){
    Lag = 1
    Yesterday[1:Lag] = Yesterday[Lag + 1]
    Lag = 2
    DayeforeYesterday[1:Lag] = DayeforeYesterday[Lag + 1]
    # }

    # computes force from daybefore yesterday, yesterday and today
    # makes sure that no informaton in the future is used!
    Force = (Today - 2 * Yesterday + DayeforeYesterday) / Yesterday

    # Force[Yesterday==0]=0
    Force[abs(Yesterday) <= Eps] = 0

    if (!missing(Time)) {
      names(Force) = Time
    } else if (!is.null(names(Today))) {
      names(Force) = names(Today)
    } else {
      # do noting
    }

    if (isTRUE(na.rm))
      Force[!is.finite(Force)] = 0

    if (isTRUE(PlotIt)) {
      if (missing(Time)) {
        Time = 1:length(Data)
      }
      Name = deparse1(substitute(Data))
      a = DataVisualizations::DualaxisLinechart(
        Time,
        Data,
        Force,
        y1lab = Name,
        y2lab = paste0("Force of ", Name),
        xlab = 'Time'
      )
      print(a)
    }
    return(Force)
  } # end is matrix
}

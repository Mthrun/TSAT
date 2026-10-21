TemporalRelativeDifference = function(Data, Time, Lag = 1, na.rm = FALSE, PlotIt = FALSE, Silent = FALSE) {
  # V=TemporalRelativeDifference(ElectricityBRD$Mrd_KWh,PlotIt=TRUE,Time=(ElectricityBRD$Time))
  # V=TemporalRelativeDifference(cbind(ElectricityBRD$Mrd_KWh,ElectricityBRD$Mrd_KWh))
  # Calculates the relative difference between positive Data and and lagged Data
  # INPUT
  # Data                numerical matrix of [1:n,1:d], d>=1; if vector named or matrix has rownames and Time input not given, names are transferred to he Reldiff output
  # OPTIONAL
  # Lag                 The number of lags (in units of observations): back shifting: positiv value,forward shifting (causality is broken if used in forecasting"): negative value
  #                     Default is the difference to yesterday
  # Time                character vector 1:n defining the name, is only used to name the Reldiff output and for plotting
  # na.rm               FALSE; do nothing, TRUE: all nan to zero
  # PlotIt              If TRUE: plots data, works only for d=1, otherwise it is internally set to FALSE
  # Silent              If TRUE: not warnings or messages
  # OUTPUT
  # Reldiff             numerical matrix of [1:n,1:d], Negative value indicate that today is lower than yesterday
  #                     and positive values that todays value is higher than yesterdays.

  # Details:
  # Contrary to linear filters in this cases the range of values lies always between [-2,2]
  # If vector is given as input, vector is given back. If matrix is given as input, PlotIt is forced to be set as FALSE.
  #
  #  Author: MCT, 2023
  #
  # Analysis functions must not install software or change the user's library.
  if (!requireNamespace("DatabionicSwarm", quietly = TRUE))
    stop("Install the optional package 'DatabionicSwarm' to use this method.", call. = FALSE)


  if (is.matrix(Data)) {
    PlotIt = FALSE # no plotting
    Rel = apply(
      X = Data,
      2,
      FUN = TemporalRelativeDifference,
      Lag = Lag,
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
    Yesterday = LagVector(Data, k = Lag)

    # no nan
    if (Lag > 0) {
      Yesterday[1:Lag] = Yesterday[Lag + 1]
    }
    if (Lag < 0) {
      indNan = tail(1:length(Yesterday), Lag)
      Yesterday[indNan] = Yesterday[min(indNan) - 1]

      warning("TemporalRelativeDifference: Under development for lag <0")
      # you have to swith today with yesterday
      tmp = Today
      Today = Yesterday # yesterday variable is tomorrow!
      Yesterday = tmp # todays variable is yesterday
    }
    # compute difference from yesterday and today
    # makes sure that no informaton in the future is used!
    if (utils::packageVersion("DatabionicSwarm") != "1.3.0") {
      # no silent parameter
      Rel = DatabionicSwarm::RelativeDifference(Y = Today, X = Yesterday, na.rm = F)
    } else {
      # silent parameter
      Rel = DatabionicSwarm::RelativeDifference(
        Y = Today, X = Yesterday,
        na.rm = F, Silent = Silent
      )
    }
    if (!missing(Time)) {
      names(Rel) = Time
    } else if (!is.null(names(Today))) {
      names(Rel) = names(Today)
    } else {
      # do noting
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
  } # end is matrix
}

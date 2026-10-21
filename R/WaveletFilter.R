WaveletFilter = function(Data, Filter = "haar", NumLevels = 2, Boundary = "periodic",
                         Fast = TRUE, PlotIt = FALSE, Threshold = "zero", Lambda = 0.05,
                         FilterLevels = "all") {
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

  # Use logical constants. The short names T and F can be replaced by callers.
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(Fast) || length(Fast) != 1L || is.na(Fast))
    stop("Fast must be TRUE or FALSE.", call. = FALSE)
  if (!requireNamespace("wavelets", quietly = TRUE))
    stop("Install the optional package 'wavelets' to use this method.", call. = FALSE)
  if (PlotIt) {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
  }
  # Plot original data
  if (PlotIt == TRUE) {
    plot(Data, type = "l")
  }

  # Apply wavelet decomposition (Redundant Wavelet Transform == ModWT)
  RWT = wavelets::modwt(
    X = as.numeric(Data), filter = Filter, n.levels = NumLevels,
    boundary = Boundary, fast = Fast
  )

  # Plot wavelet decomposition
  if (PlotIt == TRUE) {
    par(mfrow = c(NumLevels + 1, 1))
    for (i in 1:NumLevels) {
      plot(1:length(RWT@W[[i]]), RWT@W[[i]], type = "l", xlab = paste0("Waveletlevel", i), ylab = "")
    }
    plot(1:length(RWT@V[[NumLevels]]), RWT@W[[NumLevels]], type = "l", xlab = paste0("Smooth approximation level", i), ylab = "")
  }

  # Decide the filtering
  if (is.character(FilterLevels)) {
    if (FilterLevels == "all") {
      for (i in 1:NumLevels) {
        if (Threshold == "hard") {
          RWT@W[[i]] = hard_thresholding(RWT@W[[i]], Lambda)
        } else if (Threshold == "soft") {
          RWT@W[[i]] = soft_thresholding(RWT@W[[i]], Lambda)
        } else {
          RWT@W[[i]] = filter_zero(RWT@W[[i]])
        }
      }
    }
  } else {
    if (is.vector(FilterLevels)) {
      for (i in FilterLevels) {
        if (Threshold == "hard") {
          RWT@W[[i]] = hard_thresholding(RWT@W[[i]], Lambda)
        } else if (Threshold == "soft") {
          RWT@W[[i]] = soft_thresholding(RWT@W[[i]], Lambda)
        } else {
          RWT@W[[i]] = filter_zero(RWT@W[[i]])
        }
      }
    }
  }

  # Reconstruct signal from wavelet decomposition
  IRWT = wavelets::imodwt(RWT)

  # Plot filtered signal
  if (PlotIt == TRUE) {
    plot(IRWT, type = "l")
  }
  return(IRWT)
}

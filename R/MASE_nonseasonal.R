MASE_nonseasonal <- function(Y, For, epsilon = 10^-5,
                             NonDuplicatedForecasts = FALSE, TrainingData = NULL) {
  if (!is.numeric(Y) || !is.numeric(For) || !length(Y) ||
    length(Y) != length(For) || any(!is.finite(c(Y, For))))
    stop("Supply finite response and forecast vectors of equal length.")
  if (!is.logical(NonDuplicatedForecasts) || length(NonDuplicatedForecasts) != 1L || is.na(NonDuplicatedForecasts))
    stop("NonDuplicatedForecasts must be TRUE or FALSE.", call. = FALSE)
  if (NonDuplicatedForecasts)
    warning("NonDuplicatedForecasts is deprecated. All time observations are retained.")
  # Repeated values are separate time observations. Removing them changes the scale.
  scale.data <- if (is.null(TrainingData)) Y else TrainingData
  forecastMASE(Y, For, scale.data, 1L, epsilon)
}

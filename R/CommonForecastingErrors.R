CommonForecastingErrors <- function(
  TestdataY, ForecastingF, epsilon = 10^-4,
  na.rm = TRUE, stepsize = 1, digits, TrainingData = NULL
) {
  if (!is.numeric(TestdataY) || !is.numeric(ForecastingF) ||
    !length(TestdataY) || length(TestdataY) != length(ForecastingF))
    stop("Supply nonempty numeric vectors of equal length.")
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("na.rm must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(stepsize) || length(stepsize) != 1L || !is.finite(stepsize) ||
    stepsize < 1L || stepsize > .Machine$integer.max || stepsize != floor(stepsize))
    stop("stepsize must be one integer >= 1.", call. = FALSE)
  stepsize <- as.integer(stepsize)
  if (length(epsilon) != 1L || !is.finite(epsilon) || epsilon < 0)
    stop("epsilon must be one nonnegative number.")
  scale.data <- if (is.null(TrainingData)) TestdataY else TrainingData
  keep <- is.finite(TestdataY) & is.finite(ForecastingF)
  if (!na.rm && any(!keep)) stop("Supply finite values or set na.rm = TRUE.")
  y <- TestdataY[keep]; f <- ForecastingF[keep]
  out <- stats::setNames(rep(NA_real_, 7), c("MAE", "MAPE", "SMAPE", "MASE", "RMSE", "BIAS", "MRD"))
  if (!length(y)) return(out)
  error <- y - f
  rd <- RootDeviance(y, f)
  # Use percentages and absolute denominators. A zero denominator is undefined.
  # One small observation must not replace every other denominator with epsilon.
  mape <- if (any(abs(y) <= epsilon)) NA_real_ else 100 * mean(abs(error / y))
  # Keep the original time sequence for the naive scale, even when a pair is missing.
  mase <- if (any(!is.finite(scale.data))) NA_real_ else
    forecastMASE(y, f, scale.data, stepsize, epsilon)
  out[] <- c(
    mean(abs(error)), mape, mean(SMAPE(y, f)), mase,
    sqrt(mean(error^2)), rd$bias, rd$MRD
  )
  # Standard rounding does not add an artificial offset to exact zero errors.
  if (!missing(digits)) out <- round(out, digits = digits)
  out
}

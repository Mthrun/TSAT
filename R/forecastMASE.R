forecastMASE <- function(actual, predicted, training, season = 1L, epsilon = 0) {
  if (!is.numeric(season) || length(season) != 1L || !is.finite(season) ||
    season < 1L || season > .Machine$integer.max || season != floor(season))
    stop("season must be one integer >= 1.", call. = FALSE)
  season <- as.integer(season)
  if (!is.numeric(training) || any(!is.finite(training)))
    stop("The MASE scale data must be finite and numeric.")
  if (length(epsilon) != 1L || !is.finite(epsilon) || epsilon < 0)
    stop("epsilon must be one nonnegative number.")
  if (length(training) <= season) return(NA_real_)
  denominator <- mean(abs(diff(training, lag = season)))
  # A zero naive error gives an undefined scale. Do not invent random observations.
  if (denominator <= epsilon) return(NA_real_)
  mean(abs(actual - predicted)) / denominator
}

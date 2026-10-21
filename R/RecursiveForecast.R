# Predict one step, append that prediction to the history, and repeat.
# PredictFunction receives one data frame row and returns one response value.
RecursiveForecast <- function(TrainData, ForecastHorizon, PredictFunction,
                              Lags = 1L, FuturePredictors = NULL, ...) {
  if (!is.numeric(TrainData) || is.complex(TrainData) || !is.null(dim(TrainData)) ||
    !length(TrainData) || any(!is.finite(TrainData)))
    stop("TrainData must be a nonempty finite numeric vector.", call. = FALSE)
  if (!is.numeric(ForecastHorizon) || length(ForecastHorizon) != 1L ||
    !is.finite(ForecastHorizon) || ForecastHorizon < 1L ||
    ForecastHorizon > .Machine$integer.max || ForecastHorizon != floor(ForecastHorizon))
    stop("ForecastHorizon must be one positive integer.", call. = FALSE)
  if (!is.function(PredictFunction))
    stop("PredictFunction must be a function.", call. = FALSE)
  if (is.null(Lags)) Lags <- integer()
  if (!is.numeric(Lags) || any(!is.finite(Lags)) ||
    any(Lags < 1L | Lags != floor(Lags) | Lags > length(TrainData)) ||
    anyDuplicated(Lags))
    stop("Lags must contain distinct positive integers within the training history.",
      call. = FALSE
    )
  if (!is.null(FuturePredictors)) {
    FuturePredictors <- as.data.frame(FuturePredictors)
    if (NROW(FuturePredictors) != ForecastHorizon ||
      anyDuplicated(names(FuturePredictors)) ||
      any(names(FuturePredictors) %in% paste0(".Lag", Lags)))
      stop("FuturePredictors needs one row per step and unique names without lag conflicts.",
        call. = FALSE
      )
    if (anyNA(FuturePredictors) || any(vapply(
      FuturePredictors,
      function(column) is.numeric(column) && any(!is.finite(column)), logical(1)
    )))
      stop("FuturePredictors must not contain missing or infinite values.", call. = FALSE)
  }
  history <- as.numeric(TrainData)
  forecast <- numeric(ForecastHorizon)
  for (i in seq_len(ForecastHorizon)) {
    row <- if (is.null(FuturePredictors)) data.frame(.row = 1)[, FALSE, drop = FALSE] else
      FuturePredictors[i, , drop = FALSE]
    for (lag in Lags)
      row[[paste0(".Lag", lag)]] <- history[length(history) + 1L - lag]
    value <- PredictFunction(row, ...)
    # Do not take the first element of an invalid prediction without reporting it.
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L || !is.finite(value))
      stop("PredictFunction must return one finite numeric response at step ", i, ".",
        call. = FALSE
      )
    forecast[i] <- as.numeric(value)
    # Later lag values use predictions. Future observed responses are not inputs.
    history <- c(history, forecast[i])
  }
  forecast
}

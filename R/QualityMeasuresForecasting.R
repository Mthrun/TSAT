# Evaluate forecasts on the response scale. Use training data for the MASE scale.
# Return one matrix row per series, or NULL if no finite comparisons are available.
QualityMeasuresForecasting <- function(
  Actual = NULL, Forecast = NULL, Indices = NULL,
  SeasonalPeriod = NULL, NonDuplicatedForecasts = FALSE, Silent = TRUE,
  TrainingData = NULL, Y = NULL, FOR = NULL, Mase_Season = NULL
) {
  # Retain named legacy inputs. Conflicting inputs must not be selected silently.
  if (!is.null(Actual) && !is.null(Y) &&
    !isTRUE(all.equal(Actual, Y, check.attributes = FALSE)))
    stop("Use either Actual or Y, or give equal values.", call. = FALSE)
  if (is.null(Actual)) Actual <- Y
  if (!is.null(Forecast) && !is.null(FOR) &&
    !isTRUE(all.equal(Forecast, FOR, check.attributes = FALSE)))
    stop("Use either Forecast or FOR, or give equal values.", call. = FALSE)
  if (is.null(Forecast)) Forecast <- FOR
  if (!is.null(SeasonalPeriod) && !is.null(Mase_Season) &&
    !isTRUE(all.equal(SeasonalPeriod, Mase_Season)))
    stop("Use either SeasonalPeriod or Mase_Season, or give equal values.", call. = FALSE)
  if (is.null(SeasonalPeriod)) SeasonalPeriod <- if (is.null(Mase_Season)) 1L else Mase_Season
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(NonDuplicatedForecasts) || length(NonDuplicatedForecasts) != 1L ||
    is.na(NonDuplicatedForecasts))
    stop("NonDuplicatedForecasts must be TRUE or FALSE.", call. = FALSE)
  if (NonDuplicatedForecasts)
    warning("NonDuplicatedForecasts is deprecated. All time observations are retained.",
      call. = FALSE
    )
  if (!is.numeric(Actual) || is.complex(Actual) || !length(Actual) ||
    length(dim(Actual)) > 2L || !is.numeric(Forecast) || is.complex(Forecast) ||
    length(dim(Forecast)) > 2L)
    stop("Actual and Forecast must be numeric vectors or matrices.", call. = FALSE)
  actual <- as.matrix(Actual)
  forecast <- as.matrix(Forecast)
  if (!identical(dim(actual), dim(forecast)))
    stop("Actual and Forecast must have the same dimensions.", call. = FALSE)
  if (!is.numeric(SeasonalPeriod) || length(SeasonalPeriod) != 1L ||
    !is.finite(SeasonalPeriod) || SeasonalPeriod < 1L ||
    SeasonalPeriod > .Machine$integer.max || SeasonalPeriod != floor(SeasonalPeriod))
    stop("SeasonalPeriod must be one positive integer.", call. = FALSE)
  if (is.null(Indices)) Indices <- seq_len(NROW(actual))
  if (!is.numeric(Indices) || !length(Indices) || any(!is.finite(Indices)) ||
    any(Indices != floor(Indices) | Indices < 1L | Indices > NROW(actual)))
    stop("Indices must identify response rows.", call. = FALSE)
  training <- NULL
  if (!is.null(TrainingData)) {
    if (!is.numeric(TrainingData) || is.complex(TrainingData) ||
      !length(TrainingData) || length(dim(TrainingData)) > 2L ||
      NCOL(TrainingData) != NCOL(actual))
      stop("TrainingData must be numeric and have one column per response series.",
        call. = FALSE
      )
    training <- as.matrix(TrainingData)
  }
  actual <- actual[Indices, , drop = FALSE]
  forecast <- forecast[Indices, , drop = FALSE]
  metrics <- c(
    "N", "MAE", "RMSE", "Bias", "MASE", "MAPE", "SMAPE",
    "AbsoluteError", "RAE", "RSE", "MRD", "MRD_BIAS"
  )
  labels <- colnames(actual)
  if (is.null(labels)) labels <- paste0("Series", seq_len(NCOL(actual)))
  out <- matrix(NA_real_, NCOL(actual), length(metrics),
    dimnames = list(labels, metrics)
  )
  for (j in seq_len(NCOL(actual))) {
    keep <- is.finite(actual[, j]) & is.finite(forecast[, j])
    out[j, "N"] <- sum(keep)
    if (!any(keep)) next
    y <- actual[keep, j]
    f <- forecast[keep, j]
    error <- y - f
    mae <- mean(abs(error))
    # Keep the training order. Removing missing training values would change the lags.
    scale <- if (!is.null(training) && NROW(training) > SeasonalPeriod &&
      all(is.finite(training[, j])))
      mean(abs(diff(training[, j], lag = SeasonalPeriod))) else NA_real_
    denom <- abs(y) + abs(f)
    absolute.reference <- sum(abs(y - mean(y)))
    squared.reference <- sum((y - mean(y))^2)
    roots <- RootDeviance(y, f)
    out[j, -1L] <- c(
      mae, sqrt(mean(error^2)), mean(error),
      if (is.finite(scale) && scale > 0) mae / scale else NA_real_,
      if (all(y != 0)) 100 * mean(abs(error / y)) else NA_real_,
      mean(ifelse(denom == 0, 0, 200 * abs(error) / denom)),
      if (sum(y) != 0) 100 * abs(sum(error) / sum(y)) else NA_real_,
      if (absolute.reference > 0) sum(abs(error)) / absolute.reference else NA_real_,
      if (squared.reference > 0) sum(error^2) / squared.reference else NA_real_,
      roots$MRD, roots$bias
    )
  }
  if (!any(out[, "N"] > 0)) return(NULL)
  out
}

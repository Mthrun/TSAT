FcVARIMA <- function(
  Data, ARp = 1L, i = 1L, MAq = 0L, ForecastHorizont = NULL,
  PlotIt = FALSE, Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95),
  Refine = TRUE, Silent = TRUE
) {
  if (!requireNamespace("MTS", quietly = TRUE))
    stop("Install the optional package 'MTS' to use this method.", call. = FALSE)
  if (!is.matrix(Data) || NCOL(Data) < 2L) stop("VARIMA requires a numeric matrix with at least two series.")
  if (!is.numeric(ARp) || length(ARp) != 1L || !is.finite(ARp) ||
    ARp < 0L || ARp > .Machine$integer.max || ARp != floor(ARp))
    stop("ARp must be one integer >= 0.", call. = FALSE)
  ARp <- as.integer(ARp)
  if (!is.numeric(MAq) || length(MAq) != 1L || !is.finite(MAq) ||
    MAq < 0L || MAq > .Machine$integer.max || MAq != floor(MAq))
    stop("MAq must be one integer >= 0.", call. = FALSE)
  MAq <- as.integer(MAq)
  if (!is.numeric(i) || length(i) != 1L || !is.finite(i) ||
    i < 0L || i > .Machine$integer.max || i != floor(i))
    stop("i must be one integer >= 0.", call. = FALSE)
  i <- as.integer(i)
  if (i > 1L) stop("Only differencing orders zero and one are supported.")
  if (!is.logical(Refine) || length(Refine) != 1L || is.na(Refine))
    stop("Refine must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(ForecastHorizon) && !is.null(ForecastHorizont) &&
    !isTRUE(all.equal(ForecastHorizon, ForecastHorizont, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or ForecastHorizont, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) ForecastHorizont else ForecastHorizon
  if (is.null(ForecastHorizon)) ForecastHorizon <- 14L
  if (is.null(SplitAt)) SplitAt <- if (!is.null(ForecastHorizont)) NROW(Data) - ForecastHorizont else NROW(Data)
  p <- forecastPrepare(Data, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt,
    Multivariate = TRUE
  )
  train <- if (i == 1L) diff(p$Train) else p$Train
  if (Silent) {
    invisible(utils::capture.output(
      raw <- forecastFitVARMA(train, ARp, MAq, Refine, p$h)
    ))
  } else raw <- forecastFitVARMA(train, ARp, MAq, Refine, p$h)
  fit <- raw$Model
  fc <- matrix(raw$Forecast$pred, p$h, NCOL(Data))
  fitted <- matrix(NA_real_, p$k, NCOL(Data), dimnames = list(NULL, colnames(Data)))
  residual <- as.matrix(fit$residuals)
  rows <- seq.int(p$k - NROW(residual) + 1L, p$k)
  fitted[rows, ] <- p$Train[rows, , drop = FALSE] - residual
  if (i == 1L) {
    fc <- apply(fc, 2L, cumsum)
    fc <- matrix(fc, p$h, NCOL(Data))
    fc <- sweep(fc, 2L, p$Train[p$k, ], "+")
  }
  se <- forecastVARMAse(fit, p$h, i == 1L)
  bounds <- forecastBounds(fc, se, p$Level)
  forecast <- matrix(fc,
    nrow = p$h, ncol = NCOL(p$Train),
    dimnames = list(NULL, colnames(p$Train))
  )
  if (!identical(dim(forecast), c(p$h, NCOL(p$Train))) || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  if (!identical(dim(fitted), dim(p$Train)))
    stop("Fitted values do not match the training data.", call. = FALSE)
  lower <- bounds$Lower
  upper <- bounds$Upper
  if (!identical(dim(lower), c(p$h, NCOL(p$Train), length(p$Level))) ||
    !identical(dim(upper), dim(lower)) || any(!is.finite(lower)) ||
    any(!is.finite(upper)) || any(lower > upper))
    stop("Prediction bounds must be finite, ordered, and match the forecast size.", call. = FALSE)
  result <- list(
    Forecast = forecast, ForecastTime = p$ForecastTime,
    TrainData = p$Train, TrainTime = p$TrainTime,
    Fitted = fitted, Residuals = p$Train - fitted,
    TestData = p$TestData, TestTime = p$ForecastTime,
    Lower = lower, Upper = upper,
    Level = p$Level, Model = fit,
    Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
      TrainingData = p$Train, SeasonalPeriod = p$Season
    ),
    Info = list(
      Method = "VARIMA", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response",
      DifferenceOrder = i, Interval = "Gaussian prediction; fitted parameters treated as known"
    ),
    Train = p$Train, Test = p$TestData, ModelForecast = raw$Forecast
  )
  if (PlotIt) PlotForecast(result)
  result
}

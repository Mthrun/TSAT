KalmanForecastModel <- function(
  Data = NULL, Horizont = NULL, ...,
  DataVec = NULL, Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  ModelType = c("trend", "level", "BSM"), TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95), PlotIt = FALSE
) {
  if (!is.null(DataVec) && !is.null(Data) &&
    !isTRUE(all.equal(DataVec, Data, check.attributes = FALSE)))
    stop("Use either DataVec or Data, or give equal values.", call. = FALSE)
  DataVec <- if (is.null(DataVec)) Data else DataVec
  if (!is.null(ForecastHorizon) && !is.null(Horizont) &&
    !isTRUE(all.equal(ForecastHorizon, Horizont, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or Horizont, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) Horizont else ForecastHorizon
  if (is.null(ForecastHorizon)) ForecastHorizon <- 12L
  if (is.null(SplitAt)) SplitAt <- length(DataVec)
  ModelType <- match.arg(ModelType)
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  if (ModelType == "BSM" && (p$Season < 2L || p$k < 2L * p$Season))
    stop("BSM requires SeasonalPeriod >= 2 and at least two complete training cycles.")
  if (stats::var(p$Train) == 0)
    stop("The structural variance model requires nonconstant training data.")
  fit <- stats::StructTS(PrepareTS4Forecast(p$Train, p$TrainTime, p$Season), type = ModelType, ...)
  prediction <- stats::predict(fit, n.ahead = p$h)
  if (!is.null(fit$code) && fit$code != 0L)
    stop("Kalman variance estimation did not converge. Change the model or optimization settings.")
  bounds <- forecastBounds(as.numeric(prediction$pred), as.numeric(prediction$se), p$Level)
  # One-step fitted means use the previous filtered state. They do not use smoothing.
  run <- stats::KalmanRun(p$Train, fit$model0, nit = -1L)
  fitted <- c(NA_real_, as.numeric(run$states[-p$k, , drop = FALSE] %*%
    t(fit$model$T) %*% fit$model$Z))
  forecast <- as.numeric(prediction$pred)
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  if (length(fitted) != p$k)
    stop("Fitted values do not match the training data.", call. = FALSE)
  lower <- bounds$Lower
  upper <- bounds$Upper
  if (!identical(dim(lower), c(p$h, length(p$Level))) ||
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
      Method = paste("Kalman", ModelType), SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response",
      ModelType = ModelType, Variances = fit$coef,
      Interval = "Gaussian prediction; fitted parameters treated as known"
    ),
    Components = fit$fitted,
    PredictionVariance = as.numeric(prediction$se)^2,
    pred = as.numeric(prediction$pred), var = as.numeric(prediction$se)^2
  )
  if (PlotIt) PlotForecast(result)
  result
}

ExponentialSmoothing <- function(
  Time = NULL, Data = NULL, Type = "StateSpaceModel",
  PlotIt = FALSE, ..., DataVec = NULL, SplitAt = NULL, ForecastHorizon = 12L,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95)
) {
  if (!is.null(DataVec) && !is.null(Data) &&
    !isTRUE(all.equal(DataVec, Data, check.attributes = FALSE)))
    stop("Use either DataVec or Data, or give equal values.", call. = FALSE)
  Data <- if (is.null(DataVec)) Data else DataVec
  if (is.null(SplitAt)) SplitAt <- length(Data)
  Type <- match.arg(Type, c(
    "StateSpaceModel", "SSOE", "exponentialSmoothing",
    "HoltWinters", "StructTS"
  ))
  # The former branches used undefined Weekly and dat objects and fixed horizons.
  # Prepare the supplied training data once and use the common forecast contract.
  p <- forecastPrepare(
    Data, Time, SplitAt, ForecastHorizon, TimeStep, SeasonalPeriod,
    FutureTime, Level, PlotIt
  )
  if (Type == "StructTS") return(KalmanForecastModel(
    DataVec = Data, Time = Time,
    SplitAt = SplitAt, ForecastHorizon = p$h, TimeStep = TimeStep, SeasonalPeriod = p$Season,
    FutureTime = FutureTime, Level = Level, PlotIt = PlotIt, ...
  ))
  y <- PrepareTS4Forecast(p$Train, p$TrainTime, p$Season)
  lower <- upper <- NULL
  if (Type == "SSOE") {
    if (!requireNamespace("smooth", quietly = TRUE))
      stop("Install the optional package 'smooth' to use this method.", call. = FALSE)
    # The split already defines training. A second holdout would discard history.
    fit <- smooth::es(y, h = p$h, holdout = FALSE, silent = TRUE, ...)
    mean <- as.numeric(fit$forecast)
    fitted <- as.numeric(fit$fitted)
  } else {
    if (!requireNamespace("forecast", quietly = TRUE))
      stop("Install the optional package 'forecast' to use this method.", call. = FALSE)
    if (Type == "exponentialSmoothing") {
      fc <- forecast::ses(y, h = p$h, level = p$Level, ...)
      fit <- fc$model
    } else {
      if (Type == "StateSpaceModel") fit <- forecast::ets(y, ...)
      else {
        args <- list(...)
        if (p$Season == 1L && is.null(args$gamma)) args$gamma <- FALSE
        fit <- do.call(stats::HoltWinters, c(list(x = y), args))
      }
      fc <- forecast::forecast(fit, h = p$h, level = p$Level)
    }
    mean <- as.numeric(fc$mean)
    fitted <- as.numeric(fc$fitted)
    lower <- as.matrix(fc$lower)
    upper <- as.matrix(fc$upper)
  }
  if (length(fitted) < p$k) fitted <- c(rep(NA_real_, p$k - length(fitted)), fitted)
  forecast <- as.numeric(mean)
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  if (length(fitted) != p$k)
    stop("Fitted values do not match the training data.", call. = FALSE)
  if (is.null(lower) != is.null(upper))
    stop("Both prediction bounds are required.", call. = FALSE)
  if (!is.null(lower)) {
    if (!identical(dim(lower), c(p$h, length(p$Level))) ||
      !identical(dim(upper), dim(lower)) || any(!is.finite(lower)) ||
      any(!is.finite(upper)) || any(lower > upper))
      stop("Prediction bounds must be finite, ordered, and match the forecast size.", call. = FALSE)
  }
  result <- list(
    Forecast = forecast, ForecastTime = p$ForecastTime,
    TrainData = p$Train, TrainTime = p$TrainTime,
    Fitted = fitted, Residuals = p$Train - fitted,
    TestData = p$TestData, TestTime = p$ForecastTime,
    Lower = lower, Upper = upper,
    Level = if (is.null(lower)) NULL else p$Level, Model = fit,
    Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
      TrainingData = p$Train, SeasonalPeriod = p$Season
    ),
    Info = list(
      Method = paste("Exponential smoothing", Type), SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response",
      Type = Type, Interval = if (Type == "SSOE") "unavailable" else
        "native exponential smoothing prediction intervals"
    )
  )
  if (PlotIt) PlotForecast(result)
  result
}

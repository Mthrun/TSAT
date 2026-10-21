FcAutoARIMA <- function(
  DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Time = NULL, PlotIt = FALSE, Seasonal = TRUE, PlotBackwardInd = NULL,
  main = "", xlab = "Time", ylab = "Data", ...,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL,
  Level = c(80, 95), Predictors = NULL, FuturePredictors = NULL
) {
  if (!requireNamespace("forecast", quietly = TRUE))
    stop("Install the optional package 'forecast' to use this method.", call. = FALSE)
  if (!is.logical(Seasonal) || length(Seasonal) != 1L || is.na(Seasonal))
    stop("Seasonal must be TRUE or FALSE.", call. = FALSE)
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  # Check predictor rows here so the training split is visible in this model.
  train.predictors <- future.predictors <- NULL
  if (is.null(Predictors)) {
    if (!is.null(FuturePredictors))
      stop("FuturePredictors requires training Predictors.", call. = FALSE)
  } else {
    predictors <- as.data.frame(Predictors)
    if (!(NROW(predictors) %in% c(p$k, p$n)) || !NCOL(predictors) ||
      anyDuplicated(names(predictors)) ||
      any(grepl("^\\.(Response$|Lag[0-9]+$|Season[0-9]+$)", names(predictors))))
      stop("Predictors need one row per training or supplied observation and unique column names.",
        call. = FALSE
      )
    train.predictors <- predictors[seq_len(p$k), , drop = FALSE]
    if (anyNA(train.predictors) || any(vapply(
      train.predictors,
      function(column) is.numeric(column) && any(!is.finite(column)), logical(1)
    )))
      stop("Training Predictors contain missing or infinite values.", call. = FALSE)
    if (is.null(FuturePredictors)) {
      if (NROW(predictors) < p$k + p$h)
        stop("Supply FuturePredictors for all forecast steps.", call. = FALSE)
      future.predictors <- predictors[p$k + seq_len(p$h), , drop = FALSE]
    } else {
      future.predictors <- as.data.frame(FuturePredictors)
      if (NROW(future.predictors) != p$h ||
        !identical(names(future.predictors), names(train.predictors)))
        stop("FuturePredictors needs ForecastHorizon rows and the training predictor columns.",
          call. = FALSE
        )
    }
    if (anyNA(future.predictors) || any(vapply(
      future.predictors,
      function(column) is.numeric(column) && any(!is.finite(column)), logical(1)
    )))
      stop("FuturePredictors contain missing or infinite values.", call. = FALSE)
  }
  x <- list(Train = train.predictors, Future = future.predictors)
  fit <- forecast::auto.arima(PrepareTS4Forecast(p$Train, p$TrainTime, p$Season),
    seasonal = Seasonal,
    xreg = if (is.null(x$Train)) NULL else as.matrix(x$Train), ...
  )
  # predict.Arima evaluates this call entry. Store the actual regressor values.
  fit$call$xreg <- if (is.null(x$Train)) NULL else as.matrix(x$Train)
  fc <- forecast::forecast(fit,
    h = p$h, level = p$Level,
    xreg = if (is.null(x$Future)) NULL else as.matrix(x$Future)
  )
  forecast <- as.numeric(fc$mean)
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  fitted <- as.numeric(stats::fitted(fit))
  if (length(fitted) != p$k)
    stop("Fitted values do not match the training data.", call. = FALSE)
  lower <- fc$lower
  upper <- fc$upper
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
      Method = "Automatic ARIMA", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response"
    ),
    ArimaObject = fc
  )
  if (PlotIt) PlotForecast(result)
  result
}

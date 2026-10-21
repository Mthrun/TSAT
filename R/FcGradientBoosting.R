FcGradientBoosting <- function(
  DataVec, SplitAt = length(DataVec), Time = NULL,
  ForecastHorizon = 1L, Frequency = NULL, FUN = sum, PlotIt = FALSE,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL,
  Predictors = NULL, FuturePredictors = NULL, Lags = NULL, Lookback = NULL,
  Rounds = 100L, Params = list(max_depth = 3L, eta = 0.1, nthread = 1L), ...
) {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("Install the optional package 'xgboost' to use this method.", call. = FALSE)
  if (!is.null(TimeStep) && !is.null(Frequency) &&
    !isTRUE(all.equal(TimeStep, Frequency, check.attributes = FALSE)))
    stop("Use either TimeStep or Frequency, or give equal values.", call. = FALSE)
  TimeStep <- if (is.null(TimeStep)) Frequency else TimeStep
  if (!is.function(FUN)) stop("FUN must be a function.")
  if (!is.numeric(Rounds) || length(Rounds) != 1L || !is.finite(Rounds) ||
    Rounds < 1L || Rounds > .Machine$integer.max || Rounds != floor(Rounds))
    stop("Rounds must be one integer >= 1.", call. = FALSE)
  Rounds <- as.integer(Rounds)
  if (!is.list(Params) || (length(Params) &&
    (is.null(names(Params)) || any(!nzchar(names(Params))) || anyDuplicated(names(Params)))))
    stop("Params must be a named list.")
  if (!is.null(Params$objective) && Params$objective != "reg:squarederror")
    stop("This wrapper requires the reg:squarederror objective.")
  Params$objective <- "reg:squarederror"
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (!is.null(Lookback)) {
    if (!is.numeric(Lookback) || length(Lookback) != 1L || !is.finite(Lookback) ||
      Lookback < 1L || Lookback > .Machine$integer.max || Lookback != floor(Lookback))
      stop("Lookback must be one positive integer.", call. = FALSE)
    if (!is.null(Lags) && !identical(Lags, as.numeric(seq_len(Lookback))) &&
      !identical(Lags, seq_len(Lookback)))
      stop("Supply either Lags or Lookback, or give equal lag settings.", call. = FALSE)
    Lags <- seq_len(Lookback)
  }
  if (is.null(Lags)) Lags <- integer()
  if (!is.numeric(Lags) || any(!is.finite(Lags)) ||
    any(Lags < 1L | Lags > .Machine$integer.max | Lags != floor(Lags)) ||
    anyDuplicated(Lags))
    stop("Lags must contain distinct positive integers.", call. = FALSE)
  lags <- as.integer(Lags)
  if (!length(lags) && is.null(Predictors)) lags <- seq_len(min(8L, p$k - 3L))
  Lags <- lags
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
  regressors <- list(Train = train.predictors, Future = future.predictors)
  # LagVector shifts the response. Remove the initial rows with unavailable lags.
  if (is.null(regressors$Train) && !length(Lags)) Lags <- 1L
  offset <- if (length(Lags)) max(Lags) else 0L
  if (p$k - offset < 3L)
    stop("The input history leaves too few training rows.", call. = FALSE)
  training.rows <- seq.int(offset + 1L, p$k)
  training.data <- data.frame(.Response = p$Train[training.rows])
  if (!is.null(regressors$Train))
    training.data <- cbind(training.data, regressors$Train[training.rows, , drop = FALSE])
  for (lag in Lags)
    training.data[[paste0(".Lag", lag)]] <- LagVector(p$Train, lag)[training.rows]
  if (p$Season > 1L) {
    if (p$k < 2L * p$Season) stop("Supply at least two seasonal training cycles.")
    if (is.null(regressors$Future))
      regressors$Future <- data.frame(.row = seq_len(p$h))[, FALSE, drop = FALSE]
    for (j in seq.int(2L, p$Season)) {
      name <- paste0(".Season", j)
      training.data[[name]] <- as.numeric((training.rows - 1L) %% p$Season + 1L == j)
      regressors$Future[[name]] <- as.numeric((p$k + seq_len(p$h) - 1L) %% p$Season + 1L == j)
    }
  }
  if (!all(vapply(training.data, is.numeric, logical(1))))
    stop("Gradient boosting predictors must be numeric.")
  features <- names(training.data)[-1L]
  x <- as.matrix(training.data[features])
  forcastSeed(Seed, {
    if (!is.null(Seed)) Params$seed <- as.integer(Seed)
    training <- xgboost::xgb.DMatrix(x, label = training.data$.Response)
    fit <- xgboost::xgb.train(
      params = Params, data = training,
      nrounds = Rounds, verbose = 0L, ...
    )
    fc <- RecursiveForecast(p$Train, p$h, forecastPredictGradientBoosting, Lags,
      regressors$Future,
      Model = fit, Features = features
    )
    fitted <- rep(NA_real_, p$k)
    fitted[training.rows] <- stats::predict(fit, newdata = x)
    forecast <- as.numeric(fc)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    if (length(fitted) != p$k)
      stop("Fitted values do not match the training data.", call. = FALSE)
    result <- list(
      Forecast = forecast, ForecastTime = p$ForecastTime,
      TrainData = p$Train, TrainTime = p$TrainTime,
      Fitted = fitted, Residuals = p$Train - fitted,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = NULL, Upper = NULL,
      Level = NULL, Model = fit,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "Gradient boosting", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Backend = "xgboost", Lags = Lags,
        Rounds = Rounds, FittedType = "in-sample regression fit"
      ),
      ForecastStats = list(mean = fc)
    )
    if (PlotIt) PlotForecast(result)
    result
  })
}

FcRandomForest <- function(
  Time = NULL, DF = NULL, formula = NULL, SplitDataAt = NULL,
  Horizon = NULL, Package = "randomForest", AutoCorrelation = NULL, AutoCorrelationNo = 1,
  NoOfTree = 200, PlotIt = FALSE, Holidays = NULL, SimilarPoints = FALSE, tz = "UTC", ...,
  DataVec = NULL, SplitAt = NULL, ForecastHorizon = NULL, Predictors = NULL,
  FuturePredictors = NULL, Lags = NULL, Lookback = NULL, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL
) {
  Package <- match.arg(Package, c("randomForest", "ranger"))
  if (!requireNamespace(Package, quietly = TRUE))
    stop("Install the optional package '", Package, "' to use this method.", call. = FALSE)
  if (!is.logical(SimilarPoints) || length(SimilarPoints) != 1L || is.na(SimilarPoints))
    stop("SimilarPoints must be TRUE or FALSE.", call. = FALSE)
  if (SimilarPoints) stop("SimilarPoints uses future response values. Set SimilarPoints = FALSE.")
  if (!is.numeric(NoOfTree) || length(NoOfTree) != 1L || !is.finite(NoOfTree) ||
    NoOfTree < 1L || NoOfTree > .Machine$integer.max || NoOfTree != floor(NoOfTree))
    stop("NoOfTree must be one integer >= 1.", call. = FALSE)
  NoOfTree <- as.integer(NoOfTree)
  response.name <- NULL
  if (!is.null(DF)) {
    frame <- as.data.frame(DF)
    response.name <- if (is.null(formula)) names(frame)[1L] else all.vars(formula)[1L]
    if (!response.name %in% names(frame)) stop("The formula response is not a column in DF.")
    if (!is.null(DataVec) && !is.null(frame[[response.name]]) &&
      !isTRUE(all.equal(DataVec, frame[[response.name]], check.attributes = FALSE)))
      stop("Use either DataVec or DF response, or give equal values.", call. = FALSE)
    DataVec <- if (is.null(DataVec)) frame[[response.name]] else DataVec
    columns <- setdiff(names(frame), response.name)
    if (length(columns)) {
      if (!is.null(Predictors) && !is.null(frame[columns]) &&
        !isTRUE(all.equal(Predictors, frame[columns], check.attributes = FALSE)))
        stop("Use either Predictors or DF predictors, or give equal values.", call. = FALSE)
      Predictors <- if (is.null(Predictors)) frame[columns] else Predictors
    }
  }
  if (!is.null(SplitAt) && !is.null(SplitDataAt) &&
    !isTRUE(all.equal(SplitAt, SplitDataAt, check.attributes = FALSE)))
    stop("Use either SplitAt or SplitDataAt, or give equal values.", call. = FALSE)
  SplitAt <- if (is.null(SplitAt)) SplitDataAt else SplitAt
  if (is.null(SplitAt)) SplitAt <- if (!is.null(Horizon)) length(DataVec) - Horizon else length(DataVec)
  if (!is.null(ForecastHorizon) && !is.null(Horizon) &&
    !isTRUE(all.equal(ForecastHorizon, Horizon, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or Horizon, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) Horizon else ForecastHorizon
  if (is.null(ForecastHorizon))
    ForecastHorizon <- if (is.null(DF)) 1L else max(1L, length(DataVec) - SplitAt)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (!is.null(AutoCorrelation)) {
    if (is.null(DF) || length(AutoCorrelation) != 1L ||
      !identical(names(as.data.frame(DF))[AutoCorrelation], response.name))
      stop("AutoCorrelation must select the response column. Use Predictors for other series.")
    if (!is.numeric(AutoCorrelationNo) || length(AutoCorrelationNo) != 1L || !is.finite(AutoCorrelationNo) ||
      AutoCorrelationNo < 1L || AutoCorrelationNo > .Machine$integer.max || AutoCorrelationNo != floor(AutoCorrelationNo))
      stop("AutoCorrelationNo must be one integer >= 1.", call. = FALSE)
    AutoCorrelationNo <- as.integer(AutoCorrelationNo)
    if (!is.null(Lags) && !is.null(seq_len(AutoCorrelationNo)) &&
      !isTRUE(all.equal(Lags, seq_len(AutoCorrelationNo), check.attributes = FALSE)))
      stop("Use either Lags or AutoCorrelationNo, or give equal values.", call. = FALSE)
    Lags <- if (is.null(Lags)) seq_len(AutoCorrelationNo) else Lags
  }
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
  Lags <- as.integer(Lags)
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
  use.calendar <- !is.null(formula) && inherits(p$Time, c("Date", "POSIXct"))
  if (use.calendar) {
    cal <- forecastCalendarPredictors(p$TrainTime, Holidays, tz)
    for (name in names(cal)) training.data[[name]] <- cal[[name]][training.rows]
    if (is.null(regressors$Future)) regressors$Future <- forecastCalendarPredictors(p$ForecastTime, Holidays, tz)
    else for (name in names(cal)) regressors$Future[[name]] <- forecastCalendarPredictors(p$ForecastTime, Holidays, tz)[[name]]
  }
  form <- if (is.null(formula)) stats::as.formula(".Response ~ .") else formula
  form[[2L]] <- as.name(".Response")
  forcastSeed(Seed, {
    if (Package == "randomForest") {
      fit <- randomForest::randomForest(form, data = training.data, ntree = NoOfTree, ...)
      importance <- randomForest::importance(fit)
    } else {
      fit <- ranger::ranger(form,
        data = training.data, num.trees = NoOfTree,
        importance = "permutation", ...
      )
      importance <- fit$variable.importance
    }
    fc <- RecursiveForecast(p$Train, p$h, forecastPredictRandomForest, Lags,
      regressors$Future,
      Model = fit, Package = Package
    )
    fitted <- rep(NA_real_, p$k)
    fitted[training.rows] <- forecastPredictRandomForest(
      training.data[, -1L, drop = FALSE], fit, Package
    )
    forecast <- as.numeric(fc)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    if (length(fitted) != p$k)
      stop("Fitted values do not match the training data.", call. = FALSE)
    out <- list(
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
        Method = paste("Random forest", Package), SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Lags = Lags, FittedType = "in-sample regression fit"
      ),
      FeatureImportance = importance,
      TestDataPredictor = p$TestData,
      TestDataIndicators = regressors$Future
    )
    if (PlotIt) PlotForecast(out)
    out$QualityMeasures <- out$Accuracy
    out
  })
}

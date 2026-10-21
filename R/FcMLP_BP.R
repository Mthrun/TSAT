FcMLP_BP <- function(
  Response = NULL, SplitAt = NULL, Predictor1 = NULL,
  Predictor2 = NULL, Time = NULL, HiddenLayers = 4, Threshold = 0.05,
  Repetitions = 10, ErrorFunction = "sse", PlotEvaluation = FALSE, PlotIt = FALSE,
  DataVec = NULL, ForecastHorizon = NULL, Predictors = NULL, FuturePredictors = NULL,
  Lags = NULL, Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Scaled = TRUE, Seed = NULL, ...
) {
  if (!requireNamespace("neuralnet", quietly = TRUE))
    stop("Install the optional package 'neuralnet' to use this method.", call. = FALSE)
  if (!is.null(DataVec) && !is.null(Response) &&
    !isTRUE(all.equal(DataVec, Response, check.attributes = FALSE)))
    stop("Use either DataVec or Response, or give equal values.", call. = FALSE)
  DataVec <- if (is.null(DataVec)) Response else DataVec
  if (is.null(SplitAt)) SplitAt <- length(DataVec)
  if (is.null(ForecastHorizon))
    ForecastHorizon <- if (is.null(Response)) 1L else max(1L, length(DataVec) - SplitAt)
  if (!is.null(Predictor1)) {
    old <- data.frame(Predictor1 = Predictor1)
    if (!is.null(Predictor2)) old$Predictor2 <- Predictor2
    if (!is.null(Predictors) && !is.null(old) &&
      !isTRUE(all.equal(Predictors, old, check.attributes = FALSE)))
      stop("Use either Predictors or Predictor1/Predictor2, or give equal values.", call. = FALSE)
    Predictors <- if (is.null(Predictors)) old else Predictors
  } else if (!is.null(Predictor2)) stop("Predictor2 requires Predictor1.")
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (!is.logical(Scaled) || length(Scaled) != 1L || is.na(Scaled))
    stop("Scaled must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(PlotEvaluation) || length(PlotEvaluation) != 1L || is.na(PlotEvaluation))
    stop("PlotEvaluation must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(Repetitions) || length(Repetitions) != 1L || !is.finite(Repetitions) ||
    Repetitions < 1L || Repetitions > .Machine$integer.max || Repetitions != floor(Repetitions))
    stop("Repetitions must be one integer >= 1.", call. = FALSE)
  Repetitions <- as.integer(Repetitions)
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
  if (!all(vapply(training.data, is.numeric, logical(1)))) stop("MLP predictors must be numeric.")
  center <- if (Scaled) vapply(training.data, mean, numeric(1)) else rep(0, NCOL(training.data))
  scale <- if (Scaled) vapply(training.data, stats::sd, numeric(1)) else rep(1, NCOL(training.data))
  scale[!is.finite(scale) | scale == 0] <- 1
  train <- as.data.frame(sweep(sweep(as.matrix(training.data), 2, center), 2, scale, "/"))
  forcastSeed(Seed, {
    fit <- neuralnet::neuralnet(.Response ~ .,
      data = train, hidden = HiddenLayers,
      threshold = Threshold, rep = Repetitions, err.fct = ErrorFunction,
      linear.output = TRUE, ...
    )
    if (is.null(fit$weights) || !length(fit$weights)) stop("MLP training did not converge.")
    errors <- fit$result.matrix["error", , drop = TRUE]
    best <- which.min(errors)
    fc <- RecursiveForecast(p$Train, p$h, forecastPredictMLP, Lags,
      regressors$Future,
      Model = fit, Center = center, Scale = scale, Repetition = best
    )
    fitted <- rep(NA_real_, p$k)
    fitted[training.rows] <- forecastPredictMLP(
      training.data[, -1L, drop = FALSE], fit, center, scale, best
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
        Method = "Multilayer perceptron", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Lags = Lags, ScalingCenter = center,
        ScalingScale = scale, Repetition = best
      ),
      TestDataOutput = fc
    )
    if (PlotIt) PlotForecast(out)
    if (PlotIt && PlotEvaluation) graphics::plot(fit, rep = best)
    out
  })
}

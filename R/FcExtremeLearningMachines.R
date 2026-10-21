FcExtremeLearningMachines <- function(
  DataVec, Time = NULL, SplitAt = length(DataVec),
  Predictors = NULL, ForecastHorizon = 1L, No_HiddenLayers = NULL, Scaled = TRUE,
  No_TrainingNetworks = 10, PlotEvaluation = FALSE, PlotIt = FALSE, tz = "UTC", ...,
  FuturePredictors = NULL, Lags = NULL, Lookback = NULL, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL
) {
  if (!requireNamespace("nnfor", quietly = TRUE))
    stop("Install the optional package 'nnfor' to use this method.", call. = FALSE)
  if (!requireNamespace("forecast", quietly = TRUE))
    stop("Install the optional package 'forecast' to use this method.", call. = FALSE)
  if (!is.logical(Scaled) || length(Scaled) != 1L || is.na(Scaled))
    stop("Scaled must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(PlotEvaluation) || length(PlotEvaluation) != 1L || is.na(PlotEvaluation))
    stop("PlotEvaluation must be TRUE or FALSE.", call. = FALSE)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
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
  if (length(lags) && max(lags) >= p$k - 2L) stop("The selected lags leave too few training rows.")
  center <- if (Scaled) mean(p$Train) else 0
  scale <- if (Scaled) stats::sd(p$Train) else 1
  if (!is.finite(scale) || scale == 0) stop("ELM requires nonconstant training data.")
  y <- stats::ts((p$Train - center) / scale, frequency = p$Season)
  if (!is.numeric(No_TrainingNetworks) || length(No_TrainingNetworks) != 1L || !is.finite(No_TrainingNetworks) ||
    No_TrainingNetworks < 1L || No_TrainingNetworks > .Machine$integer.max || No_TrainingNetworks != floor(No_TrainingNetworks))
    stop("No_TrainingNetworks must be one integer >= 1.", call. = FALSE)
  No_TrainingNetworks <- as.integer(No_TrainingNetworks)
  forcastSeed(Seed, {
    fit <- nnfor::elm(y,
      hd = No_HiddenLayers, reps = No_TrainingNetworks,
      outplot = FALSE, lags = if (length(lags)) lags else NULL,
      xreg = if (is.null(x$Train)) NULL else as.matrix(x$Train), ...
    )
    fc <- forecast::forecast(fit,
      h = p$h,
      xreg = if (is.null(x$Train)) NULL else as.matrix(rbind(x$Train, x$Future))
    )
    fitted <- as.numeric(fc$fitted) * scale + center
    if (length(fitted) < p$k) fitted <- c(rep(NA_real_, p$k - length(fitted)), fitted)
    forecast <- as.numeric(as.numeric(fc$mean) * scale + center)
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
        Method = "Extreme learning machine", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        ScalingCenter = center, ScalingScale = scale, Lags = fit$lags
      )
    )
    if (PlotIt) PlotForecast(out)
    out$TrainingData <- out$TrainData
    if (PlotIt && PlotEvaluation) graphics::plot(fit)
    out
  })
}

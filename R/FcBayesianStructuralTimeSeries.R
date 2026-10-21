FcBayesianStructuralTimeSeries <- function(
  DataVec, SplitAt = length(DataVec),
  Time = NULL, Frequency = NULL, ForecastHorizon = 1L, nseasons = NULL,
  niter = 250, burn = 100, PlotIt = FALSE, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95), Seed = NULL
) {
  if (!requireNamespace("bsts", quietly = TRUE))
    stop("Install the optional package 'bsts' to use this method.", call. = FALSE)
  if (!is.null(TimeStep) && !is.null(Frequency) &&
    !isTRUE(all.equal(TimeStep, Frequency, check.attributes = FALSE)))
    stop("Use either TimeStep or Frequency, or give equal values.", call. = FALSE)
  TimeStep <- if (is.null(TimeStep)) Frequency else TimeStep
  if (!is.null(SeasonalPeriod) && !is.null(nseasons) &&
    !isTRUE(all.equal(SeasonalPeriod, nseasons, check.attributes = FALSE)))
    stop("Use either SeasonalPeriod or nseasons, or give equal values.", call. = FALSE)
  SeasonalPeriod <- if (is.null(SeasonalPeriod)) nseasons else SeasonalPeriod
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  if (!is.numeric(niter) || length(niter) != 1L || !is.finite(niter) ||
    niter < 2L || niter > .Machine$integer.max || niter != floor(niter))
    stop("niter must be one integer >= 2.", call. = FALSE)
  niter <- as.integer(niter)
  if (!is.numeric(burn) || length(burn) != 1L || !is.finite(burn) ||
    burn < 0L || burn > .Machine$integer.max || burn != floor(burn))
    stop("burn must be one integer >= 0.", call. = FALSE)
  burn <- as.integer(burn)
  if (burn >= niter) stop("burn must be smaller than niter.")
  forcastSeed(Seed, {
    y <- PrepareTS4Forecast(p$Train, p$TrainTime, p$Season)
    state <- bsts::AddLocalLinearTrend(list(), y)
    if (p$Season > 1L) {
      if (p$k < 2L * p$Season) stop("Supply at least two training cycles.")
      state <- bsts::AddSeasonal(state, y, nseasons = p$Season)
    }
    fit <- bsts::bsts(y,
      state.specification = state, niter = niter, ping = 0,
      seed = Seed
    )
    fc <- stats::predict(fit, horizon = p$h, burn = burn, seed = Seed)
    draws <- as.matrix(fc$distribution)
    lower <- upper <- matrix(NA_real_, p$h, length(p$Level),
      dimnames = list(NULL, paste0(p$Level, "%"))
    )
    for (j in seq_along(p$Level)) {
      lower[, j] <- apply(draws, 2L, stats::quantile, probs = (1 - p$Level[j] / 100) / 2)
      upper[, j] <- apply(draws, 2L, stats::quantile, probs = (1 + p$Level[j] / 100) / 2)
    }
    forecast <- as.numeric(fc$mean)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    if (!identical(dim(lower), c(p$h, length(p$Level))) ||
      !identical(dim(upper), dim(lower)) || any(!is.finite(lower)) ||
      any(!is.finite(upper)) || any(lower > upper))
      stop("Prediction bounds must be finite, ordered, and match the forecast size.", call. = FALSE)
    result <- list(
      Forecast = forecast, ForecastTime = p$ForecastTime,
      TrainData = p$Train, TrainTime = p$TrainTime,
      Fitted = NULL, Residuals = NULL,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = lower, Upper = upper,
      Level = p$Level, Model = fit,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "Bayesian structural time series", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Burn = burn, Iterations = niter
      ),
      ModelPrediction = fc, UpperLower = rbind(upper[, 1], lower[, 1])
    )
    if (PlotIt) PlotForecast(result)
    result
  })
}

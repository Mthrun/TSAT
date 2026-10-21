FcEchoStateANN <- function(
  DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Seasonality = NULL, Scaled = TRUE, Time = NULL, PlotIt = FALSE, NoRolling = TRUE, ...,
  Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL
) {
  if (!requireNamespace("echos", quietly = TRUE))
    stop("Install the optional package 'echos' to use this method.", call. = FALSE)
  if (!is.logical(Scaled) || length(Scaled) != 1L || is.na(Scaled))
    stop("Scaled must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(NoRolling) || length(NoRolling) != 1L || is.na(NoRolling))
    stop("NoRolling must be TRUE or FALSE.", call. = FALSE)
  if (!NoRolling) stop("Use RollingForecast for forecasts from multiple origins.")
  if (!is.null(Lookback) && !is.null(Seasonality) &&
    !isTRUE(all.equal(Lookback, Seasonality, check.attributes = FALSE)))
    stop("Use either Lookback or Seasonality, or give equal values.", call. = FALSE)
  Lookback <- if (is.null(Lookback)) Seasonality else Lookback
  if (is.null(Lookback)) Lookback <- 28L
  if (!is.numeric(Lookback) || length(Lookback) != 1L || !is.finite(Lookback) ||
    Lookback < 1L || Lookback > .Machine$integer.max || Lookback != floor(Lookback))
    stop("Lookback must be one integer >= 1.", call. = FALSE)
  Lookback <- as.integer(Lookback)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (Lookback >= p$k - 2L) stop("Lookback leaves too few training observations.")
  center <- if (Scaled) mean(p$Train) else 0
  scale <- if (Scaled) stats::sd(p$Train) else 1
  if (!is.finite(scale) || scale == 0) stop("Echo state training requires nonconstant data.")
  forcastSeed(Seed, {
    seed <- if (is.null(Seed)) 42L else as.integer(Seed)
    fit <- echos::train_esn(as.numeric((p$Train - center) / scale),
      lags = seq_len(Lookback), n_seed = seed, ...
    )
    fc <- echos::forecast_esn(fit, n_ahead = p$h, n_seed = seed)
    forecast <- as.numeric(as.numeric(fc$point) * scale + center)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    result <- list(
      Forecast = forecast, ForecastTime = p$ForecastTime,
      TrainData = p$Train, TrainTime = p$TrainTime,
      Fitted = NULL, Residuals = NULL,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = NULL, Upper = NULL,
      Level = NULL, Model = fit,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "Echo state network", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Lookback = Lookback, ScalingCenter = center, ScalingScale = scale
      ),
      FitStats = fc
    )
    if (PlotIt) PlotForecast(result)
    result
  })
}

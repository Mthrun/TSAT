FcSeasonalArima <- function(
  DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Frequency = NULL, Time = NULL, PlotIt = FALSE, ...,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = 95
) {
  if (!requireNamespace("seasonal", quietly = TRUE))
    stop("Install the optional package 'seasonal' to use this method.", call. = FALSE)
  if (!is.null(TimeStep) && !is.null(Frequency) &&
    !isTRUE(all.equal(TimeStep, Frequency, check.attributes = FALSE)))
    stop("Use either TimeStep or Frequency, or give equal values.", call. = FALSE)
  TimeStep <- if (is.null(TimeStep)) Frequency else TimeStep
  if (is.null(SeasonalPeriod) && !stats::is.ts(DataVec) && !is.null(TimeStep)) {
    step <- forecastStep(TimeStep)
    if (identical(step, "month")) SeasonalPeriod <- 12L
    if (identical(step, "quarter")) SeasonalPeriod <- 4L
  }
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  if (!p$Season %in% c(4L, 12L)) stop("X-13 requires quarterly or monthly data (period 4 or 12).")
  if (length(p$Level) != 1L) stop("X-13 supports one Level per fit.")
  fit <- seasonal::seas(PrepareTS4Forecast(p$Train, p$TrainTime, p$Season),
    forecast.save = "forecasts",
    forecast.maxlead = p$h, forecast.maxback = 0,
    forecast.probability = p$Level / 100, ...
  )
  raw <- seasonal::series(fit, "forecast.forecasts")
  if (NROW(raw) < p$h) stop("X-13 returned too few forecast rows.")
  raw <- raw[seq_len(p$h), , drop = FALSE]
  lower <- matrix(raw[, "lowerci"], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
  upper <- matrix(raw[, "upperci"], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
  forecast <- as.numeric(raw[, "forecast"])
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
      Method = "X-13 ARIMA-SEATS", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response"
    ),
    ForecastMaxlead = raw,
    TaT = list(train = p$Train, test = p$TestData)
  )
  if (PlotIt) PlotForecast(result)
  result
}

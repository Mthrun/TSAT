FcMAPA <- function(
  DataVec, Time = NULL, SplitAt = length(DataVec), ForecastHorizon = 1L,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = 95,
  PlotIt = FALSE, Aggregation = "mean", Seed = NULL, ...
) {
  if (!requireNamespace("MAPA", quietly = TRUE))
    stop("Install the optional package 'MAPA' to use this method.", call. = FALSE)
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  if (length(p$Level) != 1L) stop("MAPA supports one Level per call.")
  forcastSeed(Seed, {
    fit <- MAPA::mapa(p$Train,
      ppy = p$Season, fh = p$h, comb = Aggregation,
      conf.lvl = p$Level / 100, paral = 0, display = 0, outplot = 0, ...
    )
    lower <- upper <- NULL
    if (!is.null(fit$PI)) {
      interval <- as.matrix(fit$PI)
      if (!identical(dim(interval), c(2L, p$h))) stop("Unexpected MAPA interval dimensions.")
      lower <- matrix(interval[2L, ], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
      upper <- matrix(interval[1L, ], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
    }
    forecast <- as.numeric(fit$outfor)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
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
      Fitted = NULL, Residuals = NULL,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = lower, Upper = upper,
      Level = if (is.null(lower)) NULL else p$Level, Model = fit,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "MAPA", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response"
      ),
      MAPA_Object = fit
    )
    if (PlotIt) PlotForecast(result)
    result
  })
}

FcAdditiveDecompositionModel <- function(
  Data = NULL, SeasonalLength = NULL,
  NoSteps = NULL, alpha = 0.2, beta = 0.1, gamma = 0.1, Silent = TRUE,
  DataVec = NULL, Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, PlotIt = FALSE
) {
  if (!is.null(DataVec) && !is.null(Data) &&
    !isTRUE(all.equal(DataVec, Data, check.attributes = FALSE)))
    stop("Use either DataVec or Data, or give equal values.", call. = FALSE)
  DataVec <- if (is.null(DataVec)) Data else DataVec
  if (!is.null(SeasonalPeriod) && !is.null(SeasonalLength) &&
    !isTRUE(all.equal(SeasonalPeriod, SeasonalLength, check.attributes = FALSE)))
    stop("Use either SeasonalPeriod or SeasonalLength, or give equal values.", call. = FALSE)
  SeasonalPeriod <- if (is.null(SeasonalPeriod)) SeasonalLength else SeasonalPeriod
  if (!is.null(ForecastHorizon) && !is.null(NoSteps) &&
    !isTRUE(all.equal(ForecastHorizon, NoSteps, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or NoSteps, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) NoSteps else ForecastHorizon
  if (is.null(ForecastHorizon)) ForecastHorizon <- 1L
  if (is.null(SplitAt)) SplitAt <- length(DataVec)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  parameters <- c(alpha, beta, gamma)
  if (length(parameters) != 3L || any(!is.finite(parameters)) || any(parameters < 0 | parameters > 1))
    stop("alpha, beta, and gamma must each be a number between zero and one.")
  if (p$k < 2L * p$Season || p$k %% p$Season != 0L)
    stop("The compound model requires at least two complete training cycles and no partial cycle.")
  values <- forecastAdditiveFit(p$Train, p$Season, p$h, alpha, beta, gamma, Silent)
  fit <- list(AlphaBetaGamma = parameters, SeasonalPeriod = p$Season)
  forecast <- as.numeric(tail(values, p$h))
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  fitted <- as.numeric(head(values, p$k))
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
      Method = "Additive compound model", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response",
      FittedType = "one-step prediction after two initialization cycles"
    ),
    FitAndForecast = values, AlphaBetaGamma = parameters
  )
  if (PlotIt) forecastCompoundPlot(out)
  out
}

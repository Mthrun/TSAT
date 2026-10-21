FcCrostonIntermittentDemand <- function(
  DataVec, Time = NULL, ForecastHorizon = 1L,
  SplitAt = length(DataVec), Frequency = NULL, ModelType = "sbj", PlotIt = FALSE, ...,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL
) {
  if (!requireNamespace("tsintermittent", quietly = TRUE))
    stop("Install the optional package 'tsintermittent' to use this method.", call. = FALSE)
  if (!is.null(TimeStep) && !is.null(Frequency) &&
    !isTRUE(all.equal(TimeStep, Frequency, check.attributes = FALSE)))
    stop("Use either TimeStep or Frequency, or give equal values.", call. = FALSE)
  TimeStep <- if (is.null(TimeStep)) Frequency else TimeStep
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  ModelType <- match.arg(ModelType, c("croston", "sba", "sbj"))
  if (any(p$Train < 0)) stop("Croston requires nonnegative demand.")
  if (!any(p$Train > 0)) {
    fit <- list(type = ModelType, frc.out = rep(0, p$h), frc.in = rep(0, p$k))
  } else fit <- tsintermittent::crost(p$Train,
    h = p$h, type = ModelType,
    outplot = FALSE, ...
  )
  forecast <- as.numeric(fit$frc.out)
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  fitted <- as.numeric(fit$frc.in)
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
      Method = paste("Croston", ModelType), SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response"
    ),
    TrainingSet = p$Train, TestSet = p$TestData,
    TrainingForecast = fit$frc.in
  )
  if (PlotIt) PlotForecast(result)
  result
}

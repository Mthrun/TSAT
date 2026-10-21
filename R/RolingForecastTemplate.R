RolingForecastTemplate <- function(
  Dataframe, ForecastingFunction = FcRandomForest,
  Horizon = 7, Steps = 371:7, TimeColumnInd = 1, PlotMAE = FALSE, tz = "UTC", ...
) {
  Time <- Dataframe[[TimeColumnInd]]
  data <- as.data.frame(Dataframe[, -TimeColumnInd, drop = FALSE])
  if (NCOL(data) < 1L) stop("Supply a response column after the time column.")
  out <- RollingForecast(data[[1]],
    ForecastingFunction = ForecastingFunction,
    Origins = NROW(data) - Steps, ForecastHorizon = Horizon, Time = Time,
    Predictors = if (NCOL(data) > 1L) data[, -1L, drop = FALSE] else NULL, ...
  )
  forecasts <- split(out$Forecasts, out$Forecasts$OriginIndex)
  mae <- vapply(forecasts, function(x) if (any(is.finite(x$Error))) mean(abs(x$Error), na.rm = TRUE) else NA_real_, numeric(1))
  if (!is.logical(PlotMAE) || length(PlotMAE) != 1L || is.na(PlotMAE))
    stop("PlotMAE must be TRUE or FALSE.", call. = FALSE)
  if (PlotMAE)
    graphics::plot(as.integer(names(mae)), mae, type = "l", xlab = "Forecast origin", ylab = "MAE")
  list(RollingForecasts = forecasts, MAE = mae, Accuracy = out$Accuracy)
}

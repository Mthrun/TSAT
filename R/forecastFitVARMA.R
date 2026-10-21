# Fit the vector model and request its native forecasts.
forecastFitVARMA <- function(TrainData, ARp, MAq, Refine, ForecastHorizon) {
  fit <- MTS::VARMA(TrainData, p = ARp, q = MAq, details = FALSE)
  if (Refine) fit <- MTS::refVARMA(fit)
  list(Model = fit, Forecast = MTS::VARMApred(fit, h = ForecastHorizon))
}

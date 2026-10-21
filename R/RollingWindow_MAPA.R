RollingWindow_MAPA <- function(
  Data, Time, Horizon = 14, Window = 365,
  Period = 1, Aggregation = "mean"
) {
  if (!is.numeric(Window) || length(Window) != 1L || !is.finite(Window) ||
    Window < 1L || Window > .Machine$integer.max || Window != floor(Window))
    stop("Window must be one integer >= 1.", call. = FALSE)
  Window <- as.integer(Window)
  origins <- length(Data) - Window - Horizon + seq_len(Window)
  out <- RollingForecast(Data,
    ForecastingFunction = FcMAPA, Origins = origins,
    ForecastHorizon = Horizon, Time = Time, SeasonalPeriod = Period,
    Aggregation = Aggregation, ContinueOnError = FALSE
  )
  list(
    Error = matrix(out$Forecasts$Error, nrow = Window, byrow = TRUE),
    Forecast = matrix(out$Forecasts$Forecast, nrow = Window, byrow = TRUE),
    RWData = matrix(out$Forecasts$Actual, nrow = Window, byrow = TRUE),
    RWTime = matrix(as.character(out$Forecasts$Time), nrow = Window, byrow = TRUE),
    ForecastOrigin = Time[origins],
    Setting = list(
      Data = Data, Time = Time, Model = list(Period_ppy = Period, Aggregation_comb = Aggregation),
      Forecast = list(Window = Window, Horizon = Horizon)
    )
  )
}

FcGARCH <- function(
  DataVec, VarianceModel = list(model = "sGARCH", garchOrder = c(1, 1)),
  ForecastHorizon = 1L, DistributionModel = "norm", PlotIt = FALSE, Summary = FALSE, ...,
  Time = NULL, SplitAt = length(DataVec), TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Level = c(80, 95), Difference = TRUE
) {
  if (!requireNamespace("rugarch", quietly = TRUE))
    stop("Install the optional package 'rugarch' to use this method.", call. = FALSE)
  if (!is.logical(Summary) || length(Summary) != 1L || is.na(Summary))
    stop("Summary must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(Difference) || length(Difference) != 1L || is.na(Difference))
    stop("Difference must be TRUE or FALSE.", call. = FALSE)
  p <- forecastPrepare(
    DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime, Level, PlotIt
  )
  y <- if (Difference) diff(p$Train) else p$Train
  spec <- rugarch::ugarchspec(
    variance.model = VarianceModel,
    distribution.model = DistributionModel, ...
  )
  fit <- rugarch::ugarchfit(spec, data = y, solver.control = list(trace = 0))
  # rugarch returns a filter, not a fitted model, when all parameters are fixed.
  # Forecast the specification directly in that case.
  fixed <- methods::is(fit, "uGARCHfilter")
  if (fixed) {
    fc <- rugarch::ugarchforecast(spec, data = y, n.ahead = p$h)
  } else {
    if (fit@fit$convergence != 0L) stop("GARCH fitting did not converge.")
    fc <- rugarch::ugarchforecast(fit, n.ahead = p$h)
  }
  mean <- as.numeric(rugarch::fitted(fc))
  fitted <- as.numeric(rugarch::fitted(fit))
  lower <- upper <- NULL
  if (Difference) {
    mean <- tail(p$Train, 1L) + cumsum(mean)
    fitted <- c(NA_real_, head(p$Train, -1L) + fitted)
  } else if (p$h == 1L) {
    lower <- upper <- matrix(NA_real_, p$h, length(p$Level),
      dimnames = list(NULL, paste0(p$Level, "%"))
    )
    for (j in seq_along(p$Level)) {
      lower[, j] <- as.numeric(rugarch::quantile(fc, (1 - p$Level[j] / 100) / 2))
      upper[, j] <- as.numeric(rugarch::quantile(fc, (1 + p$Level[j] / 100) / 2))
    }
  } else if (DistributionModel == "norm" &&
    spec@model$modelinc["arfima"] == 0 &&
    spec@model$modelinc["archm"] == 0) {
    # sigma(fc) describes future innovations. ARMA terms propagate these errors
    # into later response forecasts. Omitting this sum makes bounds too narrow.
    coefficients <- rugarch::coef(fit)
    ar <- coefficients[grep("^ar[0-9]+$", names(coefficients))]
    ma <- coefficients[grep("^ma[0-9]+$", names(coefficients))]
    psi <- c(1, stats::ARMAtoMA(ar = ar, ma = ma, lag.max = p$h - 1L))
    variance <- as.numeric(rugarch::sigma(fc))^2
    se <- sqrt(vapply(seq_len(p$h), function(h)
      sum(psi[seq_len(h)]^2 * rev(variance[seq_len(h)])), numeric(1)))
    bounds <- forecastBounds(mean, se, p$Level)
    lower <- bounds$Lower
    upper <- bounds$Upper
  }
  # Multi-step non-Gaussian, fractional, and variance-in-mean distributions need
  # path simulation. Do not label innovation quantiles as response intervals.
  forecast <- as.numeric(mean)
  if (length(forecast) != p$h || any(!is.finite(forecast)))
    stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
  if (length(fitted) != p$k)
    stop("Fitted values do not match the training data.", call. = FALSE)
  if (is.null(lower) != is.null(upper))
    stop("Both prediction bounds are required.", call. = FALSE)
  if (!is.null(lower)) {
    if (!identical(dim(lower), c(p$h, length(p$Level))) ||
      !identical(dim(upper), dim(lower)) || any(!is.finite(lower)) ||
      any(!is.finite(upper)) || any(lower > upper))
      stop("Prediction bounds must be finite, ordered, and match the forecast size.", call. = FALSE)
  }
  out <- list(
    Forecast = forecast, ForecastTime = p$ForecastTime,
    TrainData = p$Train, TrainTime = p$TrainTime,
    Fitted = fitted, Residuals = p$Train - fitted,
    TestData = p$TestData, TestTime = p$ForecastTime,
    Lower = lower, Upper = upper,
    Level = if (is.null(lower)) NULL else p$Level, Model = fit,
    Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
      TrainingData = p$Train, SeasonalPeriod = p$Season
    ),
    Info = list(
      Method = "GARCH", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
      ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
      Scale = "response",
      Difference = Difference,
      VolatilityScale = if (Difference) "first differences" else "response",
      Interval = if (Difference) "unavailable on the integrated response scale"
      else if (p$h == 1L) "conditional distribution"
      else if (is.null(lower)) "unavailable for this multi-step distribution"
      else "Gaussian approximation with propagated ARMA uncertainty"
    ),
    Volatility = as.numeric(rugarch::sigma(fc)), ModelForecast = fc
  )
  if (PlotIt) PlotForecast(out)
  if (PlotIt && Summary) graphics::plot(fit, which = "all")
  out
}

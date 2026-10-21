RollingForecast <- function(
  DataVec, ForecastingFunction = FcAutoARIMA, Origins,
  ForecastHorizon = 1L, Time = NULL, Window = NULL, Predictors = NULL,
  ContinueOnError = TRUE, ...
) {
  n <- NROW(DataVec)
  if (!is.numeric(ForecastHorizon) || length(ForecastHorizon) != 1L || !is.finite(ForecastHorizon) ||
    ForecastHorizon < 1L || ForecastHorizon > .Machine$integer.max || ForecastHorizon != floor(ForecastHorizon))
    stop("ForecastHorizon must be one integer >= 1.", call. = FALSE)
  h <- as.integer(ForecastHorizon)
  if (!is.logical(ContinueOnError) || length(ContinueOnError) != 1L || is.na(ContinueOnError))
    stop("ContinueOnError must be TRUE or FALSE.", call. = FALSE)
  if (!is.function(ForecastingFunction)) stop("ForecastingFunction must be a function.")
  if (!length(Origins) || any(!is.finite(Origins)) || any(Origins != floor(Origins)) ||
    any(Origins < 3L | Origins + h > n) || anyDuplicated(Origins))
    stop("Origins must identify distinct training endpoints with complete test horizons.")
  if (is.null(Time)) Time <- if (stats::is.ts(DataVec)) as.numeric(stats::time(DataVec)) else seq_len(n)
  if (is.character(Time)) Time <- as.Date(Time)
  if (length(Time) != n) stop("Time must have one timestamp per observation.")
  if (!is.null(Window)) {
    if (!is.numeric(Window) || length(Window) != 1L || !is.finite(Window) ||
      Window < 3L || Window > .Machine$integer.max || Window != floor(Window))
      stop("Window must be one integer >= 3.", call. = FALSE)
    Window <- as.integer(Window)
  }
  if (!is.null(Predictors) && NROW(Predictors) != n) stop("Predictors must have one row per observation.")
  dots <- list(...)
  if (any(names(dots) %in% c("DataVec", "Data", "Time", "SplitAt", "ForecastHorizon", "FutureTime", "PlotIt", "Predictors", "FuturePredictors")))
    stop("Do not override the rolling forecast inputs through ... .")
  results <- vector("list", length(Origins))
  tables <- vector("list", length(Origins))
  d <- NCOL(DataVec)
  series <- colnames(as.matrix(DataVec))
  if (is.null(series)) series <- paste0("Series", seq_len(d))
  for (index in seq_along(Origins)) {
    origin <- Origins[index]
    rows <- seq.int(if (is.null(Window)) 1L else max(1L, origin - Window + 1L), origin)
    future.rows <- origin + seq_len(h)
    train <- if (is.matrix(DataVec)) DataVec[rows, , drop = FALSE] else DataVec[rows]
    args <- list(
      Time = Time[rows], SplitAt = length(rows), ForecastHorizon = h,
      FutureTime = Time[future.rows], PlotIt = FALSE
    )
    response <- if ("DataVec" %in% names(formals(ForecastingFunction))) "DataVec" else "Data"
    args[[response]] <- train
    if (stats::is.ts(DataVec) && is.null(dots$SeasonalPeriod)) args$SeasonalPeriod <- stats::frequency(DataVec)
    if (!is.null(Predictors)) {
      args$Predictors <- as.data.frame(Predictors)[rows, , drop = FALSE]
      args$FuturePredictors <- as.data.frame(Predictors)[future.rows, , drop = FALSE]
    }
    fit <- tryCatch(do.call(ForecastingFunction, c(args, dots)), error = function(e) e)
    failure <- inherits(fit, "error")
    if (failure && !ContinueOnError) stop(fit)
    actual <- as.matrix(DataVec)[future.rows, , drop = FALSE]
    predictions <- if (failure) matrix(NA_real_, h, d) else as.matrix(fit$Forecast)
    if (!identical(dim(predictions), dim(actual))) stop("The forecast method returned incompatible dimensions.")
    scaled <- matrix(NA_real_, h, d)
    if (!failure) {
      season <- fit$Info$SeasonalPeriod
      for (j in seq_len(d)) if (length(rows) > season) {
        scale <- mean(abs(diff(as.matrix(train)[, j], lag = season)))
        if (is.finite(scale) && scale > 0) scaled[, j] <- abs(actual[, j] - predictions[, j]) / scale
      }
      fit$TestData <- if (d == 1L) as.numeric(actual) else actual
      fit$Accuracy <- QualityMeasuresForecasting(actual, predictions, TrainingData = train, SeasonalPeriod = season)
      results[index] <- list(fit)
    } else results[index] <- list(list(Error = conditionMessage(fit)))
    tables[[index]] <- data.frame(
      Origin = rep(Time[origin], h * d), OriginIndex = origin,
      Lead = rep(seq_len(h), d), Time = rep(Time[future.rows], d), Series = rep(series, each = h),
      Forecast = as.numeric(predictions), Actual = as.numeric(actual),
      Error = as.numeric(actual - predictions), ScaledAbsoluteError = as.numeric(scaled),
      Status = if (failure) "failed" else "ok", Message = if (failure) conditionMessage(fit) else ""
    )
  }
  table <- do.call(rbind, tables)
  groups <- split(table, interaction(table$Series, table$Lead, drop = TRUE))
  accuracy <- do.call(rbind, lapply(groups, function(x) {
    keep <- is.finite(x$Error)
    data.frame(
      Series = x$Series[1], Lead = x$Lead[1], N = sum(keep),
      MAE = if (any(keep)) mean(abs(x$Error[keep])) else NA_real_,
      RMSE = if (any(keep)) sqrt(mean(x$Error[keep]^2)) else NA_real_,
      Bias = if (any(keep)) mean(x$Error[keep]) else NA_real_,
      MASE = if (any(is.finite(x$ScaledAbsoluteError))) mean(x$ScaledAbsoluteError, na.rm = TRUE) else NA_real_
    )
  }))
  rownames(accuracy) <- NULL
  names(results) <- as.character(Time[Origins])
  list(Forecasts = table, Accuracy = accuracy, Results = results)
}

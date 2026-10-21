# Plot the observed training data, response forecasts, test data, and intervals.
# Restore the graphics settings after the plot.
PlotForecast <- function(result) {
  required <- c("TrainData", "TrainTime", "Forecast", "ForecastTime", "TestData")
  if (!is.list(result) || !all(required %in% names(result)))
    stop("result must be a TSAT forecast result.", call. = FALSE)
  train <- as.matrix(result$TrainData)
  forecast <- as.matrix(result$Forecast)
  actual <- as.matrix(result$TestData)
  if (!is.numeric(train) || !is.numeric(forecast) || !is.numeric(actual) ||
    !length(train) || !length(forecast) ||
    any(!is.finite(train)) || any(!is.finite(forecast)) ||
    NCOL(train) != NCOL(forecast) || !identical(dim(actual), dim(forecast)) ||
    length(result$TrainTime) != NROW(train) ||
    length(result$ForecastTime) != NROW(forecast))
    stop("The result data and timestamps must have matching dimensions.", call. = FALSE)
  if (any(!is.finite(as.numeric(result$TrainTime))) ||
    any(!is.finite(as.numeric(result$ForecastTime))))
    stop("The result timestamps must be finite.", call. = FALSE)
  if (is.null(result$Lower) != is.null(result$Upper))
    stop("Both prediction bounds are required.", call. = FALSE)
  if (!is.null(result$Lower)) {
    expected <- if (NCOL(train) > 1L)
      c(NROW(forecast), NCOL(train), length(result$Level)) else
      c(NROW(forecast), length(result$Level))
    if (!is.numeric(result$Lower) || !is.numeric(result$Upper) ||
      !identical(dim(result$Lower), expected) ||
      !identical(dim(result$Upper), expected) ||
      any(!is.finite(result$Lower)) || any(!is.finite(result$Upper)) ||
      any(result$Lower > result$Upper))
      stop("Prediction bounds must be finite, ordered, and match the forecast size.",
        call. = FALSE
      )
  }
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  if (NCOL(train) > 1L) graphics::par(mfrow = c(NCOL(train), 1L))
  for (j in seq_len(NCOL(train))) {
    lower <- upper <- NULL
    if (!is.null(result$Lower)) {
      lower <- if (length(dim(result$Lower)) == 3L)
        matrix(result$Lower[, j, ], nrow = NROW(forecast)) else result$Lower
      upper <- if (length(dim(result$Upper)) == 3L)
        matrix(result$Upper[, j, ], nrow = NROW(forecast)) else result$Upper
    }
    y <- c(train[, j], forecast[, j], actual[, j], lower, upper)
    graphics::plot(result$TrainTime, train[, j],
      type = "l",
      xlim = range(c(result$TrainTime, result$ForecastTime)),
      ylim = range(y, finite = TRUE), xlab = "Time",
      ylab = if (is.null(colnames(train))) "Response" else colnames(train)[j]
    )
    if (!is.null(lower)) for (level in rev(seq_len(NCOL(lower)))) {
      graphics::polygon(c(as.numeric(result$ForecastTime), rev(as.numeric(result$ForecastTime))),
        c(lower[, level], rev(upper[, level])),
        col = grDevices::adjustcolor("steelblue", alpha.f = 0.15), border = NA
      )
      graphics::lines(result$ForecastTime, lower[, level], col = "steelblue", lty = 3)
      graphics::lines(result$ForecastTime, upper[, level], col = "steelblue", lty = 3)
    }
    graphics::lines(result$ForecastTime, forecast[, j], col = "red", type = "o", pch = 16)
    graphics::points(result$ForecastTime, actual[, j])
    graphics::abline(v = as.numeric(tail(result$TrainTime, 1L)), lty = 2)
  }
  invisible(result)
}

WhiteNoiseTest <- function(TimeSeries, lags = 1, type = "Ljung-Box", PlotIt = TRUE) {
  if (!is.numeric(lags) || length(lags) != 1L || !is.finite(lags) ||
    lags < 1L || lags > .Machine$integer.max || lags != floor(lags))
    stop("lags must be one integer >= 1.", call. = FALSE)
  lags <- as.integer(lags)
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(TimeSeries) || any(!is.finite(TimeSeries)) ||
    length(TimeSeries) <= lags || length(TimeSeries) < 3L || stats::var(TimeSeries) == 0)
    stop("Supply a finite, nonconstant series longer than lags.")
  if (type %in% c("c", "nc", "ct")) {
    warning("Unit-root settings are deprecated. Using the Ljung-Box test.")
    type <- "Ljung-Box"
  }
  type <- match.arg(type, c("Ljung-Box", "Box-Pierce"))
  # A unit-root test distinguishes nonstationarity. It does not test white noise.
  # Test serial correlation instead. A large p-value does not prove independence.
  out <- stats::Box.test(TimeSeries, lag = lags, type = type)
  if (PlotIt) {
    graphics::hist(TimeSeries,
      probability = TRUE, xlab = "Response",
      main = paste(type, "p-value:", format.pval(out$p.value)), col = "grey90"
    )
    x <- seq(min(TimeSeries), max(TimeSeries), length.out = 200)
    graphics::lines(x, stats::dnorm(x, mean(TimeSeries), stats::sd(TimeSeries)), col = "magenta")
  }
  invisible(out)
}

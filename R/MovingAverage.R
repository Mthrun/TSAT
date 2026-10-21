MovingAverage <- function(Data, lag, PlotIt = FALSE) {
  if (!is.numeric(lag) || length(lag) != 1L || !is.finite(lag) ||
    lag < 1L || lag > .Machine$integer.max || lag != floor(lag))
    stop("lag must be one integer >= 1.", call. = FALSE)
  lag <- as.integer(lag)
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(Data) || !length(Data) || length(dim(Data)) > 2L)
    stop("Data must be a nonempty numeric vector or matrix.")
  x <- as.matrix(Data)
  # A trailing window uses the current observation and earlier observations.
  # Use only available history at the start. A full-series mean leaks future data.
  out <- x
  for (i in seq_len(NROW(x)))
    out[i, ] <- colMeans(x[seq.int(max(1L, i - lag + 1L), i), , drop = FALSE],
      na.rm = TRUE
    )
  if (PlotIt) {
    graphics::matplot(x, type = "l", xlab = "Time", ylab = "Response")
    graphics::matlines(out, col = "red")
  }
  if (is.null(dim(Data))) {
    out <- as.numeric(out)
    if (stats::is.ts(Data)) out <- stats::ts(out,
      start = stats::start(Data),
      frequency = stats::frequency(Data)
    )
  }
  out
}

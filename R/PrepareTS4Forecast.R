# Convert a regular training response to an R time series.
# Time sets the start. SeasonalPeriod sets the number of observations per cycle.
PrepareTS4Forecast <- function(Data, Time = NULL, SeasonalPeriod = NULL) {
  if (!is.numeric(Data) || is.complex(Data) || !length(Data) ||
    length(dim(Data)) > 2L || any(!is.finite(Data)))
    stop("Data must be a finite numeric vector or matrix.", call. = FALSE)
  if (is.null(SeasonalPeriod))
    SeasonalPeriod <- if (stats::is.ts(Data)) stats::frequency(Data) else 1L
  if (!is.numeric(SeasonalPeriod) || length(SeasonalPeriod) != 1L ||
    !is.finite(SeasonalPeriod) || SeasonalPeriod < 1L ||
    SeasonalPeriod > .Machine$integer.max || SeasonalPeriod != floor(SeasonalPeriod))
    stop("SeasonalPeriod must be one positive integer.", call. = FALSE)
  if (is.null(Time))
    Time <- if (stats::is.ts(Data)) as.numeric(stats::time(Data)) else seq_len(NROW(Data))
  if (is.character(Time)) Time <- as.Date(Time)
  if (inherits(Time, "POSIXlt")) Time <- as.POSIXct(Time)
  if (!(is.numeric(Time) || inherits(Time, c("Date", "POSIXct"))) ||
    length(Time) != NROW(Data) || any(!is.finite(as.numeric(Time))) ||
    any(diff(as.numeric(Time)) <= 0))
    stop("Time must contain one ordered, unique timestamp per observation.", call. = FALSE)
  if (length(Time) > 1L && is.null(forecastInferStep(Time)))
    stop("An R time series requires regular observations.", call. = FALSE)
  first <- Time[1L]
  if (inherits(first, c("Date", "POSIXct"))) {
    year <- as.integer(format(first, "%Y"))
    month <- as.integer(format(first, "%m"))
    start <- if (SeasonalPeriod == 12L) c(year, month) else if (SeasonalPeriod == 4L) c(year, (month - 1L) %/% 3L + 1L) else year
  } else start <- as.numeric(first)
  # Calendar dates cannot be stored in a ts index. The forecast result retains them.
  stats::ts(Data, start = start, frequency = SeasonalPeriod)
}

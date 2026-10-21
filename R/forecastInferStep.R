# Find a regular calendar or numeric interval. Return NULL for an irregular series.
forecastInferStep <- function(Time) {
  if (length(Time) < 2L) return(NULL)
  if (inherits(Time, "POSIXct")) {
    # Prefer calendar units when local dates and clock times are regular.
    # This also preserves month ends and future daylight saving changes.
    zone <- attr(Time, "tzone")
    if (is.null(zone) || !length(zone)) zone <- ""
    for (unit in c("month", "quarter", "year", "day", "week")) {
      candidate <- tryCatch(forecastDates(Time[1L], length(Time) - 1L, unit, Time),
        error = function(e) NULL
      )
      if (!is.null(candidate) &&
        all(abs(as.numeric(candidate) - as.numeric(Time[-1L])) < 1e-6)) return(unit)
    }
  }
  if (inherits(Time, "Date")) {
    month <- as.integer(format(Time, "%Y")) * 12L +
      as.integer(format(Time, "%m"))
    dm <- diff(month)
    for (pair in list(c(1, "month"), c(3, "quarter"), c(12, "year"))) {
      if (all(dm == as.integer(pair[1]))) {
        candidate <- forecastDates(Time[1], length(Time) - 1L, pair[2], Time)
        if (all(candidate == Time[-1L])) return(pair[2])
      }
    }
  }
  delta <- diff(as.numeric(Time))
  if (all(abs(delta - delta[1]) <= 1e-8 * max(1, abs(delta[1]))))
    return(delta[1])
  NULL
}

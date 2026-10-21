fiscalCalendarGroups <- function(Time) {
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  # Use ISO week years on both sides of New Year. Keep week 53 in month 12.
  # Discarding it loses observations and breaks totals.
  week <- lubridate::isoweek(Time)
  year <- lubridate::isoyear(Time)
  pattern <- c(rep(seq_len(12L), rep(c(5L, 4L, 4L), 4L)), 12L)
  as.Date(sprintf("%04d-%02d-01", year, pattern[week]))
}

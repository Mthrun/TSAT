aggregateTime2Days <- function(Time, Data, FUN, Header, tz = "UTC", ...) {
  # Convert in the requested zone before grouping. Do not lose tz in column recursion.
  time <- as.Date(Time, tz = tz)
  aggregateCalendarData(time, Data, time, FUN, if (missing(Header)) NULL else Header, ...)
}

aggregateDays2Months <- function(Time, Data, FUN, Header, Period = "month", tz = "UTC", ...) {
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  time <- as.Date(Time, tz = tz)
  group <- as.Date(lubridate::floor_date(time, unit = Period))
  # Use the same period and time zone for vectors and every matrix column.
  aggregateCalendarData(time, Data, group, FUN, if (missing(Header)) NULL else Header, ...)
}

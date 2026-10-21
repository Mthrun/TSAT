aggregateDays2Weeks <- function(Time, Data, FUN, Header, tz = "UTC", ...) {
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  time <- as.Date(Time, tz = tz)
  # A single path also honors FUN for tibble input and retains missing values.
  group <- as.Date(lubridate::floor_date(time, "week", week_start = 1))
  aggregateCalendarData(time, Data, group, FUN, if (missing(Header)) NULL else Header, ...)
}

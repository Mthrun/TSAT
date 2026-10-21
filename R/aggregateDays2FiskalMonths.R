aggregateDays2FiskalMonths <- function(Time, Data, FUN, Header, tz = "UTC", ...) {
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  # is.Date has no tz argument. Apply the zone during date conversion instead.
  time <- as.Date(Time, tz = tz)
  group <- fiscalCalendarGroups(time)
  # Aggregate daily observations directly. A mean of weekly means gives unequal
  # weights to observations in partial weeks. Missing periods are not invented.
  out <- aggregateCalendarData(time, Data, group, FUN, if (missing(Header)) NULL else Header, ...)
  rownames(out) <- vapply(
    sort(unique(group)), function(g)
      as.character(lubridate::floor_date(min(time[group == g]), "week", week_start = 1)),
    character(1)
  )
  out
}

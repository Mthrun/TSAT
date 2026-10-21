aggregateWeeks2FiskalMonths <- function(Time, Data, FUN, Header, ...) {
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  time <- as.Date(Time)
  if (anyNA(time) || any(lubridate::wday(time, week_start = 1) != 1L))
    stop("Weekly timestamps must be Mondays.")
  group <- fiscalCalendarGroups(time)
  # Keep each supplied week. Do not insert zero values for missing weeks.
  out <- aggregateCalendarData(time, Data, group, FUN, if (missing(Header)) NULL else Header, ...)
  rownames(out) <- vapply(sort(unique(group)), function(g)
    as.character(min(time[group == g])), character(1))
  out
}

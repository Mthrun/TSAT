# Extend a regular calendar. Preserve month ends and local clock times.
forecastDates <- function(origin, count, step, history = origin) {
  if (is.numeric(step)) return(origin + seq_len(count) * step)
  if (!inherits(origin, c("Date", "POSIXct")))
    stop("Calendar TimeStep values require Date or POSIXct timestamps.",
      call. = FALSE
    )
  if (inherits(origin, "Date") && step %in% c("hour", "min", "sec"))
    stop("Use POSIXct timestamps for intervals shorter than one day.",
      call. = FALSE
    )
  if (inherits(origin, "POSIXct") && step %in% c("day", "week", "month", "quarter", "year")) {
    zone <- attr(origin, "tzone")
    if (is.null(zone) || !length(zone)) zone <- ""
    zone <- zone[1L]
    # Calendar steps preserve local clock time. Adding 86400 seconds shifts it at DST.
    dates <- forecastDates(
      as.Date(origin, tz = zone), count, step,
      as.Date(history, tz = zone)
    )
    result <- as.POSIXct(paste(dates, format(origin, "%H:%M:%OS6", tz = zone)), tz = zone)
    if (anyNA(result) ||
      any(format(result, "%H:%M:%S", tz = zone) != format(origin, "%H:%M:%S", tz = zone)))
      stop("A forecast clock time does not exist in this time zone. Supply a valid calendar.",
        call. = FALSE
      )
    return(result)
  }
  if (inherits(origin, "Date") && step %in% c("month", "quarter", "year")) {
    months <- switch(step,
      month = 1L,
      quarter = 3L,
      year = 12L
    )
    first <- as.Date(format(origin, "%Y-%m-01"))
    starts <- seq(first,
      by = paste(months, "months"),
      length.out = count + 1L
    )[-1L]
    ends <- as.Date(format(starts + 32, "%Y-%m-01")) - 1
    past.ends <- as.Date(format(
      as.Date(format(history, "%Y-%m-01")) + 32,
      "%Y-%m-01"
    )) - 1
    if (all(history == past.ends)) return(ends)
    day <- max(as.integer(format(history, "%d")))
    return(starts + pmin(day, as.integer(format(ends, "%d"))) - 1)
  }
  seq(origin, by = step, length.out = count + 1L)[-1L]
}

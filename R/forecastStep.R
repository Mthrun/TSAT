# Check an observation interval and convert plural calendar units to singular units.
forecastStep <- function(TimeStep) {
  if (is.null(TimeStep)) return(NULL)
  if (is.numeric(TimeStep)) {
    if (length(TimeStep) != 1L || !is.finite(TimeStep) || TimeStep <= 0)
      stop("A numeric TimeStep must be positive.", call. = FALSE)
    return(TimeStep)
  }
  if (!is.character(TimeStep) || length(TimeStep) != 1L)
    stop("TimeStep must be a time unit or a positive number.", call. = FALSE)
  units <- c(
    days = "day", weeks = "week", months = "month",
    quarters = "quarter", years = "year", hours = "hour",
    minutes = "min", seconds = "sec"
  )
  if (TimeStep %in% names(units)) TimeStep <- unname(units[TimeStep])
  if (!TimeStep %in% c(
    unname(units), "day", "week", "month", "quarter",
    "year", "hour", "min", "sec"
  ))
    stop("Unsupported TimeStep. Supply FutureTime for a special calendar.",
      call. = FALSE
    )
  TimeStep
}

# Make calendar predictors for random forest training and forecast dates.
forecastCalendarPredictors <- function(Time, Holidays = NULL, tz = "UTC") {
  date <- as.Date(Time, tz = tz)
  holiday <- if (is.null(Holidays)) rep(FALSE, length(date)) else date %in% as.Date(Holidays$Time)
  weekday <- as.integer(format(date, "%u"))
  data.frame(
    Weekdays = weekday, WeekNo = as.integer(format(date, "%V")),
    Months = as.integer(format(date, "%m")),
    Quarters = (as.integer(format(date, "%m")) - 1L) %/% 3L + 1L,
    Years = as.integer(format(date, "%Y")), Holidays = holiday,
    Workingdays = weekday <= 5L & !holiday
  )
}

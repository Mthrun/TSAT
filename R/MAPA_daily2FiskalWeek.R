MAPA_daily2FiskalWeek <- function(
  DailyTime, DailyData, SplitAt, ForecastHorizon = 12,
  ForecastHorizonMonthly = 12, FiskalMonthSeason = 12, FUN = sum, Confidence = c(0.95),
  na.rm = TRUE, tz = "UTC", ...
) {
  if (!requireNamespace("MAPA", quietly = TRUE))
    stop("Install the optional package 'MAPA' to use this method.", call. = FALSE)
  if (!requireNamespace("lubridate", quietly = TRUE))
    stop("Install the optional package 'lubridate' to use this method.", call. = FALSE)
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("na.rm must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(SplitAt) || length(SplitAt) != 1L || !is.finite(SplitAt) ||
    SplitAt < 3L || SplitAt > .Machine$integer.max || SplitAt != floor(SplitAt))
    stop("SplitAt must be one integer >= 3.", call. = FALSE)
  SplitAt <- as.integer(SplitAt)
  if (!is.numeric(ForecastHorizon) || length(ForecastHorizon) != 1L || !is.finite(ForecastHorizon) ||
    ForecastHorizon < 1L || ForecastHorizon > .Machine$integer.max || ForecastHorizon != floor(ForecastHorizon))
    stop("ForecastHorizon must be one integer >= 1.", call. = FALSE)
  h <- as.integer(ForecastHorizon)
  if (!is.numeric(ForecastHorizonMonthly) || length(ForecastHorizonMonthly) != 1L || !is.finite(ForecastHorizonMonthly) ||
    ForecastHorizonMonthly < 1L || ForecastHorizonMonthly > .Machine$integer.max || ForecastHorizonMonthly != floor(ForecastHorizonMonthly))
    stop("ForecastHorizonMonthly must be one integer >= 1.", call. = FALSE)
  hm <- as.integer(ForecastHorizonMonthly)
  if (length(Confidence) != 1L || !is.finite(Confidence) || Confidence <= 0 || Confidence >= 1)
    stop("Confidence must be one probability between zero and one.")
  if (!is.numeric(DailyData) || !is.null(dim(DailyData)))
    stop("DailyData must be a numeric vector.")
  # Apply the requested time zone and missing-value rule before fitting.
  monthly <- aggregateDays2FiskalMonths(DailyTime, DailyData, FUN = FUN, tz = tz, na.rm = na.rm)
  if (SplitAt > NROW(monthly)) stop("SplitAt exceeds the number of fiscal months.")
  fit <- FcMAPA(monthly$Data,
    Time = monthly$Time, SplitAt = SplitAt,
    ForecastHorizon = hm, TimeStep = "month", SeasonalPeriod = FiskalMonthSeason,
    Level = 100 * Confidence, ...
  )
  months <- as.integer(format(fit$ForecastTime, "%m"))
  years <- as.integer(format(fit$ForecastTime, "%Y"))
  pattern <- rep(c(5L, 4L, 4L), 4L)
  count <- pattern[months]
  # The fiscal calendar assigns ISO week 53 to month 12. Use that same rule here.
  leap <- lubridate::isoweek(as.Date(sprintf("%04d-12-28", years))) == 53L
  count[months == 12L & leap] <- count[months == 12L & leap] + 1L
  if (h > sum(count)) stop("ForecastHorizonMonthly does not cover ForecastHorizon weeks.")
  date.parts <- lapply(seq_len(hm), function(i) {
    first <- as.Date(lubridate::floor_date(as.Date(sprintf("%04d-01-04", years[i])),
      "week",
      week_start = 1
    ))
    offset <- if (months[i] == 1L) 0L else sum(pattern[seq_len(months[i] - 1L)])
    seq(first + 7L * offset, by = "week", length.out = count[i])
  })
  # Derive future dates from the calendar. Test data can end before the horizon.
  time <- do.call(c, date.parts)
  forecast <- rep(fit$Forecast / count, count)
  actual <- rep(fit$TestData / count, count)
  lower <- upper <- rep(NA_real_, length(time))
  if (!is.null(fit$Lower)) {
    lower <- rep(fit$Lower[, 1L] / count, count)
    upper <- rep(fit$Upper[, 1L] / count, count)
  }
  index <- seq_len(h)
  list(
    MAPA_F = data.frame(TestTime = time[index], TestData = actual[index], Forecast = forecast[index]),
    MAPA_Object = fit$Model,
    Conf = data.frame(TestTime = time[index], Lower = lower[index], Upper = upper[index])
  )
}

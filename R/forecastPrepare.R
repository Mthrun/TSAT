# Check the response and calendar. Split the training data from observed test data.
# Return explicit forecast timestamps, including dates beyond the supplied data.
forecastPrepare <- function(Data, Time = NULL, SplitAt = NROW(Data),
                            ForecastHorizon = 1L, TimeStep = NULL,
                            SeasonalPeriod = NULL, FutureTime = NULL,
                            Level = c(80, 95), PlotIt = FALSE,
                            Multivariate = FALSE, Regular = TRUE) {
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(Data) || !length(Data) || length(dim(Data)) > 2L)
    stop("Data must be a nonempty numeric vector or matrix.", call. = FALSE)
  multi <- is.matrix(Data) && NCOL(Data) > 1L
  if (multi && !Multivariate)
    stop("This method requires one response series.", call. = FALSE)
  n <- NROW(Data)
  if (!is.numeric(SplitAt) || length(SplitAt) != 1L || !is.finite(SplitAt) ||
    SplitAt < 1L || SplitAt > .Machine$integer.max || SplitAt != floor(SplitAt))
    stop("SplitAt must be one integer >= 1.", call. = FALSE)
  k <- as.integer(SplitAt)
  if (!is.numeric(ForecastHorizon) || length(ForecastHorizon) != 1L || !is.finite(ForecastHorizon) ||
    ForecastHorizon < 1L || ForecastHorizon > .Machine$integer.max || ForecastHorizon != floor(ForecastHorizon))
    stop("ForecastHorizon must be one integer >= 1.", call. = FALSE)
  h <- as.integer(ForecastHorizon)
  if (k > n) stop("SplitAt exceeds the number of observations.", call. = FALSE)
  if (k < 3L) stop("Supply at least three training observations.", call. = FALSE)
  if (is.null(SeasonalPeriod))
    SeasonalPeriod <- if (stats::is.ts(Data)) stats::frequency(Data) else 1L
  if (!is.numeric(SeasonalPeriod) || length(SeasonalPeriod) != 1L || !is.finite(SeasonalPeriod) ||
    SeasonalPeriod < 1L || SeasonalPeriod > .Machine$integer.max || SeasonalPeriod != floor(SeasonalPeriod))
    stop("SeasonalPeriod must be one integer >= 1.", call. = FALSE)
  season <- as.integer(SeasonalPeriod)
  if (!is.numeric(Level) || !length(Level) || any(!is.finite(Level)) ||
    any(Level <= 0 | Level >= 100) || anyDuplicated(Level))
    stop("Level must contain distinct percentages between 0 and 100.",
      call. = FALSE
    )
  Level <- sort(Level)
  if (is.null(Time)) Time <- if (stats::is.ts(Data)) as.numeric(stats::time(Data)) else seq_len(n)
  if (is.character(Time)) Time <- as.Date(Time)
  if (inherits(Time, "POSIXlt")) Time <- as.POSIXct(Time)
  if (!(is.numeric(Time) || inherits(Time, c("Date", "POSIXct"))) ||
    length(Time) != n || any(!is.finite(as.numeric(Time))) ||
    any(diff(as.numeric(Time)) <= 0))
    stop("Time must contain one ordered, unique timestamp per observation.",
      call. = FALSE
    )
  train.time <- Time[seq_len(k)]
  step <- forecastStep(TimeStep)
  if (is.null(step)) step <- forecastInferStep(train.time)
  if (Regular && is.null(step))
    stop("This method requires regular training observations. Supply the correct TimeStep.",
      call. = FALSE
    )
  if (!is.null(step)) {
    expected <- forecastDates(train.time[1], k - 1L, step, train.time)
    if (Regular && !isTRUE(all.equal(
      as.numeric(expected),
      as.numeric(train.time[-1L])
    )))
      stop("Training timestamps do not agree with TimeStep.", call. = FALSE)
  }
  available <- min(h, n - k)
  if (!is.null(FutureTime)) {
    if (is.character(FutureTime)) FutureTime <- as.Date(FutureTime)
    if (!identical(class(Time), class(FutureTime)) &&
      !(is.numeric(Time) && is.numeric(FutureTime)))
      stop("FutureTime and Time must have the same time class.", call. = FALSE)
    if (length(FutureTime) != h || any(!is.finite(as.numeric(FutureTime))) ||
      any(diff(c(as.numeric(Time[k]), as.numeric(FutureTime))) <= 0))
      stop("FutureTime must contain ForecastHorizon ordered timestamps after the origin.",
        call. = FALSE
      )
    forecast.time <- FutureTime
  } else if (!Regular && available == h) {
    # Irregular methods predict at the supplied test dates, even if training was regular.
    forecast.time <- Time[k + seq_len(h)]
  } else if (!is.null(step)) {
    forecast.time <- forecastDates(Time[k], h, step, train.time)
  } else if (available == h) {
    forecast.time <- Time[k + seq_len(h)]
  } else {
    stop("Supply FutureTime or TimeStep to extend an irregular calendar.",
      call. = FALSE
    )
  }
  if (available && !isTRUE(all.equal(
    as.numeric(forecast.time[seq_len(available)]),
    as.numeric(Time[k + seq_len(available)])
  )))
    stop("Forecast timestamps do not match the observed test timestamps.",
      call. = FALSE
    )
  if (Regular && !isTRUE(all.equal(
    as.numeric(forecast.time),
    as.numeric(forecastDates(Time[k], h, step, train.time))
  )))
    stop("FutureTime does not agree with the regular forecast interval.",
      call. = FALSE
    )
  data <- if (multi) unclass(as.matrix(Data)) else as.numeric(Data)
  train <- if (multi) data[seq_len(k), , drop = FALSE] else data[seq_len(k)]
  if (any(!is.finite(train)))
    stop("Training data contain missing or infinite values. Prepare the training data first.",
      call. = FALSE
    )
  actual <- if (multi) matrix(NA_real_, h, NCOL(data),
    dimnames = list(NULL, colnames(data))
  ) else rep(NA_real_, h)
  if (available) {
    if (multi) actual[seq_len(available), ] <- data[k + seq_len(available), , drop = FALSE]
    else actual[seq_len(available)] <- data[k + seq_len(available)]
  }
  list(
    Data = data, Train = train, Time = Time, TrainTime = train.time,
    ForecastTime = forecast.time, TestData = actual, n = n, k = k, h = h,
    Season = season, Level = Level, Step = step, Multi = multi, PlotIt = PlotIt
  )
}

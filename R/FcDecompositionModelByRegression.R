FcDecompositionModelByRegression <- function(
  DataFrame = NULL, TimeColumnName = "Time",
  FeatureName = "Absatz", SplitDataAt = NULL, Frequency = NULL, ForecastPeriods = NULL,
  Holidays = NULL, PlotIt = FALSE, xlab = "Time", ylab = "Feature", EquiDist = TRUE,
  MinLowerBound = NULL, MaxUpperBound = NULL, Cycles = list(), Growth = "linear", tz = "UTC", ...,
  DataVec = NULL, Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  TimeStep = NULL, FutureTime = NULL, Level = 80, Predictors = NULL,
  FuturePredictors = NULL, PlotComponents = FALSE, Seed = NULL
) {
  if (!requireNamespace("prophet", quietly = TRUE))
    stop("Install the optional package 'prophet' to use this method.", call. = FALSE)
  if (!is.logical(EquiDist) || length(EquiDist) != 1L || is.na(EquiDist))
    stop("EquiDist must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(PlotComponents) || length(PlotComponents) != 1L || is.na(PlotComponents))
    stop("PlotComponents must be TRUE or FALSE.", call. = FALSE)
  if (!is.list(Cycles)) stop("Cycles must be a list.")
  if (!is.null(DataFrame)) {
    if (!all(c(TimeColumnName, FeatureName) %in% names(DataFrame))) stop("DataFrame does not contain the specified time and response columns.")
    if (!is.null(DataVec) && !is.null(DataFrame[[FeatureName]]) &&
      !isTRUE(all.equal(DataVec, DataFrame[[FeatureName]], check.attributes = FALSE)))
      stop("Use either DataVec or DataFrame response, or give equal values.", call. = FALSE)
    DataVec <- if (is.null(DataVec)) DataFrame[[FeatureName]] else DataVec
    if (!is.null(Time) && !is.null(DataFrame[[TimeColumnName]]) &&
      !isTRUE(all.equal(Time, DataFrame[[TimeColumnName]], check.attributes = FALSE)))
      stop("Use either Time or DataFrame time, or give equal values.", call. = FALSE)
    Time <- if (is.null(Time)) DataFrame[[TimeColumnName]] else Time
  }
  if (is.null(Time)) stop("Prophet requires calendar timestamps in Time.")
  if (is.character(Time)) Time <- as.Date(Time)
  if (!inherits(Time, c("Date", "POSIXct"))) stop("Prophet requires Date or POSIXct timestamps.")
  if (!is.null(SplitAt) && !is.null(SplitDataAt) &&
    !isTRUE(all.equal(SplitAt, SplitDataAt, check.attributes = FALSE)))
    stop("Use either SplitAt or SplitDataAt, or give equal values.", call. = FALSE)
  SplitAt <- if (is.null(SplitAt)) SplitDataAt else SplitAt
  if (is.null(SplitAt)) SplitAt <- length(DataVec)
  if (!is.null(ForecastHorizon) && !is.null(ForecastPeriods) &&
    !isTRUE(all.equal(ForecastHorizon, ForecastPeriods, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or ForecastPeriods, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) ForecastPeriods else ForecastHorizon
  if (is.null(ForecastHorizon)) ForecastHorizon <- if (!is.null(DataFrame)) max(1L, length(DataVec) - SplitAt) else 1L
  if (!is.null(TimeStep) && !is.null(Frequency) &&
    !isTRUE(all.equal(TimeStep, Frequency, check.attributes = FALSE)))
    stop("Use either TimeStep or Frequency, or give equal values.", call. = FALSE)
  TimeStep <- if (is.null(TimeStep)) Frequency else TimeStep
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    1L, FutureTime, Level, PlotIt,
    Regular = EquiDist
  )
  if (length(p$Level) != 1L) stop("Prophet supports one Level per call.")
  # Check predictor rows here so the training split is visible in this model.
  train.predictors <- future.predictors <- NULL
  if (is.null(Predictors)) {
    if (!is.null(FuturePredictors))
      stop("FuturePredictors requires training Predictors.", call. = FALSE)
  } else {
    predictors <- as.data.frame(Predictors)
    if (!(NROW(predictors) %in% c(p$k, p$n)) || !NCOL(predictors) ||
      anyDuplicated(names(predictors)) ||
      any(grepl("^\\.(Response$|Lag[0-9]+$|Season[0-9]+$)", names(predictors))))
      stop("Predictors need one row per training or supplied observation and unique column names.",
        call. = FALSE
      )
    train.predictors <- predictors[seq_len(p$k), , drop = FALSE]
    if (anyNA(train.predictors) || any(vapply(
      train.predictors,
      function(column) is.numeric(column) && any(!is.finite(column)), logical(1)
    )))
      stop("Training Predictors contain missing or infinite values.", call. = FALSE)
    if (is.null(FuturePredictors)) {
      if (NROW(predictors) < p$k + p$h)
        stop("Supply FuturePredictors for all forecast steps.", call. = FALSE)
      future.predictors <- predictors[p$k + seq_len(p$h), , drop = FALSE]
    } else {
      future.predictors <- as.data.frame(FuturePredictors)
      if (NROW(future.predictors) != p$h ||
        !identical(names(future.predictors), names(train.predictors)))
        stop("FuturePredictors needs ForecastHorizon rows and the training predictor columns.",
          call. = FALSE
        )
    }
    if (anyNA(future.predictors) || any(vapply(
      future.predictors,
      function(column) is.numeric(column) && any(!is.finite(column)), logical(1)
    )))
      stop("FuturePredictors contain missing or infinite values.", call. = FALSE)
  }
  x <- list(Train = train.predictors, Future = future.predictors)
  train <- data.frame(ds = p$TrainTime, y = p$Train)
  future <- data.frame(ds = c(p$TrainTime, p$ForecastTime))
  if (!is.null(x$Train)) {
    for (name in names(x$Train)) {
      if (name %in% c("ds", "y", "cap", "floor")) stop("Predictor names must not use Prophet's reserved columns.")
      train[[name]] <- x$Train[[name]]
      future[[name]] <- c(x$Train[[name]], x$Future[[name]])
    }
  }
  if (!is.null(MinLowerBound)) {
    if (length(MinLowerBound) != 1L || !is.finite(MinLowerBound)) stop("MinLowerBound must be one finite number.")
    train$floor <- MinLowerBound; future$floor <- MinLowerBound
  }
  if (!is.null(MaxUpperBound)) {
    if (length(MaxUpperBound) != 1L || !is.finite(MaxUpperBound)) stop("MaxUpperBound must be one finite number.")
    train$cap <- MaxUpperBound; future$cap <- MaxUpperBound
  }
  if (!is.null(MinLowerBound) && !is.null(MaxUpperBound) &&
    MinLowerBound >= MaxUpperBound)
    stop("MinLowerBound must be smaller than MaxUpperBound.")
  if (identical(Growth, "logistic")) {
    for (name in c("cap", "floor")) if (is.null(train[[name]]) && !is.null(DataFrame[[name]])) {
      values <- DataFrame[[name]]
      train[[name]] <- values[seq_len(p$k)]
      if (length(unique(values)) == 1L) future[[name]] <- values[1L]
      else if (length(values) >= p$k + p$h) future[[name]] <- values[seq_len(p$k + p$h)]
      else stop("Supply constant bounds or capacity values for all forecast timestamps.")
    }
    if (is.null(train$cap)) stop("Logistic growth requires MaxUpperBound or a cap column.")
  }
  if (isTRUE(Cycles$Mirrored)) {
    if (!is.null(x$Train) || !inherits(p$TrainTime, "Date") || is.null(p$Step))
      stop("Mirrored cycles require regular Date timestamps without external predictors.")
    count <- NROW(train)
    step <- if (is.numeric(p$Step)) paste(-p$Step, "days") else paste("-1", p$Step)
    earlier <- sort(seq(p$TrainTime[1], by = step, length.out = 3L * count + 1L))
    train <- rbind(train[count:1L, ], train, train[count:1L, ], train)
    train$ds <- c(earlier, p$TrainTime[-1L])
  }
  forcastSeed(Seed, {
    model <- prophet::prophet(
      holidays = Holidays, growth = Growth,
      interval.width = p$Level / 100, ...
    )
    orders <- if (is.null(Cycles$Orders)) c(24, 8, 5) else Cycles$Orders
    priors <- if (is.null(Cycles$PriorScale)) c(0.8, 0.8, 0.8) else Cycles$PriorScale
    if (length(orders) != 3L || length(priors) != 3L) stop("Cycles Orders and PriorScale must each contain three values.")
    names <- c("monthly", "quaterly", "biyearly")
    periods <- c(30.5, 365.25 / 4, 2 * 365.25)
    for (j in seq_along(names)) if (isTRUE(Cycles[[names[j]]])) {
      if (!is.numeric(orders[j]) || length(orders[j]) != 1L || !is.finite(orders[j]) ||
        orders[j] < 1L || orders[j] > .Machine$integer.max || orders[j] != floor(orders[j]))
        stop("Fourier order must be one integer >= 1.", call. = FALSE)
      order <- as.integer(orders[j])
      model <- prophet::add_seasonality(model,
        name = names[j], period = periods[j],
        fourier.order = order, prior.scale = priors[j]
      )
    }
    if (!is.null(x$Train)) for (name in names(x$Train)) model <- prophet::add_regressor(model, name)
    model <- prophet::fit.prophet(model, train)
    raw <- stats::predict(model, future)
    for (name in intersect(c("yhat", "yhat_lower", "yhat_upper"), names(raw))) {
      if (!is.null(MinLowerBound)) raw[[name]] <- pmax(raw[[name]], MinLowerBound)
      if (!is.null(MaxUpperBound)) raw[[name]] <- pmin(raw[[name]], MaxUpperBound)
    }
    index <- p$k + seq_len(p$h)
    lower <- upper <- NULL
    if (all(c("yhat_lower", "yhat_upper") %in% names(raw))) {
      lower <- matrix(raw$yhat_lower[index], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
      upper <- matrix(raw$yhat_upper[index], ncol = 1L, dimnames = list(NULL, paste0(p$Level, "%")))
    }
    observed <- data.frame(ds = p$ForecastTime, y = p$TestData)
    ds <- yhat <- yhat_lower <- yhat_upper <- y <- NULL
    ggObject <- ggplot2::ggplot(raw, ggplot2::aes(x = ds, y = yhat))
    if (!is.null(lower)) ggObject <- ggObject +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = 0.3)
    ggObject <- ggObject +
      ggplot2::geom_line(colour = "darkgreen") +
      ggplot2::geom_point(data = train, ggplot2::aes(x = ds, y = y)) +
      ggplot2::geom_point(data = observed, ggplot2::aes(x = ds, y = y), colour = "red", na.rm = TRUE) +
      ggplot2::labs(x = xlab, y = ylab)
    forecast <- as.numeric(raw$yhat[index])
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    fitted <- as.numeric(raw$yhat[seq_len(p$k)])
    if (length(fitted) != p$k)
      stop("Fitted values do not match the training data.", call. = FALSE)
    if (is.null(lower) != is.null(upper))
      stop("Both prediction bounds are required.", call. = FALSE)
    if (!is.null(lower)) {
      if (!identical(dim(lower), c(p$h, length(p$Level))) ||
        !identical(dim(upper), dim(lower)) || any(!is.finite(lower)) ||
        any(!is.finite(upper)) || any(lower > upper))
        stop("Prediction bounds must be finite, ordered, and match the forecast size.", call. = FALSE)
    }
    out <- list(
      Forecast = forecast, ForecastTime = p$ForecastTime,
      TrainData = p$Train, TrainTime = p$TrainTime,
      Fitted = fitted, Residuals = p$Train - fitted,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = lower, Upper = upper,
      Level = if (is.null(lower)) NULL else p$Level, Model = model,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "Prophet", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        SeasonalPeriodUnit = "days; specified by Prophet options and Cycles"
      ),
      ModelForecast = raw, TrainingData = train,
      TrainingSetForecast = raw[seq_len(p$k), ], ggObject = ggObject
    )
    if (PlotIt) {
      print(ggObject)
      if (PlotComponents) print(prophet::prophet_plot_components(model, raw))
    }
    out
  })
}

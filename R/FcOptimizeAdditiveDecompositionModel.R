FcOptimizeAdditiveDecompositionModel <- function(
  Data = NULL, SeasonalLength = NULL,
  Horizon = NULL, Iterations = 100, PlotIt = FALSE, DataVec = NULL, Time = NULL,
  SplitAt = NULL, ForecastHorizon = NULL, TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Seed = NULL
) {
  if (!is.null(DataVec) && !is.null(Data) &&
    !isTRUE(all.equal(DataVec, Data, check.attributes = FALSE)))
    stop("Use either DataVec or Data, or give equal values.", call. = FALSE)
  DataVec <- if (is.null(DataVec)) Data else DataVec
  if (!is.null(SeasonalPeriod) && !is.null(SeasonalLength) &&
    !isTRUE(all.equal(SeasonalPeriod, SeasonalLength, check.attributes = FALSE)))
    stop("Use either SeasonalPeriod or SeasonalLength, or give equal values.", call. = FALSE)
  SeasonalPeriod <- if (is.null(SeasonalPeriod)) SeasonalLength else SeasonalPeriod
  if (is.null(SeasonalPeriod)) SeasonalPeriod <- 52L
  if (!is.null(ForecastHorizon) && !is.null(Horizon) &&
    !isTRUE(all.equal(ForecastHorizon, Horizon, check.attributes = FALSE)))
    stop("Use either ForecastHorizon or Horizon, or give equal values.", call. = FALSE)
  ForecastHorizon <- if (is.null(ForecastHorizon)) Horizon else ForecastHorizon
  if (is.null(ForecastHorizon)) ForecastHorizon <- 12L
  # Preserve the old Horizon call. The canonical interface fits all supplied data by default.
  if (is.null(SplitAt)) SplitAt <- if (!is.null(Horizon)) length(DataVec) - Horizon else length(DataVec)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  if (!is.numeric(Iterations) || length(Iterations) != 1L || !is.finite(Iterations) ||
    Iterations < 1L || Iterations > .Machine$integer.max || Iterations != floor(Iterations))
    stop("Iterations must be one integer >= 1.", call. = FALSE)
  Iterations <- as.integer(Iterations)
  if (p$k < 3L * p$Season || p$k %% p$Season != 0L)
    stop("The compound model requires at least three complete training cycles and no partial cycle.")
  forcastSeed(Seed, {
    # A positive gradient tolerance lets exact fits stop at numerical precision.
    # The default zero tolerance can report line-search failure at a zero error.
    fits <- lapply(seq_len(Iterations), function(i)
      stats::optim(stats::runif(3L, 0.01, 0.99), forecastAdditiveLoss,
        method = "L-BFGS-B",
        Data = p$Train, SeasonalPeriod = p$Season,
        lower = rep(0.01, 3), upper = rep(0.99, 3),
        control = list(pgtol = 1e-8)
      ))
    good <- vapply(fits, function(x) x$convergence == 0L && is.finite(x$value), logical(1))
    if (!any(good)) stop("Compound model optimization did not converge.")
    fit <- fits[good][[which.min(vapply(fits[good], function(x) x$value, numeric(1)))]]
    a <- fit$par
    out <- suppressWarnings(FcAdditiveDecompositionModel(
      DataVec = DataVec, Time = Time,
      SplitAt = p$k, ForecastHorizon = p$h, SeasonalPeriod = p$Season,
      TimeStep = TimeStep, FutureTime = FutureTime,
      alpha = a[1], beta = a[2], gamma = a[3], PlotIt = PlotIt
    ))
    out$Model$Optimization <- fit
    out$Info$Method <- "Optimized additive compound model"
    out$Accurary <- out$Accuracy
    out$Train <- out$TrainData
    out$Test <- out$TestData
    out$ForecastTrain <- out$Fitted
    out
  })
}

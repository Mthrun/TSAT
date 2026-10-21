FcLSTM <- function(
  DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Seasonality = NULL, Scaled = TRUE, ErrorLoss = "MSE", Epochs = 100, Neurons = 28,
  ActivationFunction = "relu", RecurrentActivation = "sigmoid", Batch_size = 1,
  Time = NULL, PlotIt = FALSE, Silent = TRUE, ..., Lookback = NULL,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL
) {
  if (!requireNamespace("keras", quietly = TRUE))
    stop("Install the optional package 'keras' to use this method.", call. = FALSE)
  if (!requireNamespace("tensorflow", quietly = TRUE))
    stop("Install the optional package 'tensorflow' to use this method.", call. = FALSE)
  if (!keras::is_keras_available())
    stop("Configure a Python environment compatible with the R keras package before using LSTM.")
  if (!is.logical(Scaled) || length(Scaled) != 1L || is.na(Scaled))
    stop("Scaled must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(Lookback) && !is.null(Seasonality) &&
    !isTRUE(all.equal(Lookback, Seasonality, check.attributes = FALSE)))
    stop("Use either Lookback or Seasonality, or give equal values.", call. = FALSE)
  Lookback <- if (is.null(Lookback)) Seasonality else Lookback
  if (is.null(Lookback)) Lookback <- 28L
  if (!is.numeric(Lookback) || length(Lookback) != 1L || !is.finite(Lookback) ||
    Lookback < 1L || Lookback > .Machine$integer.max || Lookback != floor(Lookback))
    stop("Lookback must be one integer >= 1.", call. = FALSE)
  Lookback <- as.integer(Lookback)
  if (!is.numeric(Epochs) || length(Epochs) != 1L || !is.finite(Epochs) ||
    Epochs < 1L || Epochs > .Machine$integer.max || Epochs != floor(Epochs))
    stop("Epochs must be one integer >= 1.", call. = FALSE)
  Epochs <- as.integer(Epochs)
  if (!is.numeric(Neurons) || length(Neurons) != 1L || !is.finite(Neurons) ||
    Neurons < 1L || Neurons > .Machine$integer.max || Neurons != floor(Neurons))
    stop("Neurons must be one integer >= 1.", call. = FALSE)
  Neurons <- as.integer(Neurons)
  if (!is.numeric(Batch_size) || length(Batch_size) != 1L || !is.finite(Batch_size) ||
    Batch_size < 1L || Batch_size > .Machine$integer.max || Batch_size != floor(Batch_size))
    stop("Batch_size must be one integer >= 1.", call. = FALSE)
  Batch_size <- as.integer(Batch_size)
  p <- forecastPrepare(DataVec, Time, SplitAt, ForecastHorizon, TimeStep,
    SeasonalPeriod, FutureTime,
    PlotIt = PlotIt
  )
  samples <- p$k - Lookback
  if (samples < 3L || Batch_size > samples) stop("Lookback or Batch_size leaves too few training samples.")
  center <- if (Scaled) mean(p$Train) else 0
  scale <- if (Scaled) stats::sd(p$Train) else 1
  if (!is.finite(scale) || scale == 0) stop("LSTM requires nonconstant training data.")
  y <- (p$Train - center) / scale
  x <- array(0, c(samples, Lookback, 1L))
  for (j in seq_len(samples)) x[j, , 1L] <- y[j + seq_len(Lookback) - 1L]
  target <- y[Lookback + seq_len(samples)]
  loss <- switch(ErrorLoss,
    MSE = "mean_squared_error",
    MAE = "mean_absolute_error",
    MRD = function(actual, predicted) tensorflow::tf$reduce_mean(tensorflow::tf$sqrt(tensorflow::tf$abs(actual - predicted))),
    SRD = function(actual, predicted) tensorflow::tf$reduce_sum(tensorflow::tf$sqrt(tensorflow::tf$abs(actual - predicted))),
    stop("ErrorLoss must be MSE, MAE, MRD, or SRD.")
  )
  forcastSeed(Seed, {
    if (!is.null(Seed)) tensorflow::set_random_seed(as.integer(Seed), disable_gpu = FALSE)
    model <- keras::keras_model_sequential()
    model <- keras::layer_lstm(model,
      units = Neurons, input_shape = c(Lookback, 1L),
      activation = ActivationFunction, recurrent_activation = RecurrentActivation, ...
    )
    model <- keras::layer_dense(model, units = 1L)
    keras::compile(model, loss = loss, optimizer = "adam")
    history <- keras::fit(model,
      x = x, y = target, epochs = Epochs,
      batch_size = Batch_size, shuffle = FALSE, verbose = if (Silent) 0L else 1L
    )
    fitted <- c(rep(NA_real_, Lookback), as.numeric(stats::predict(model, x, verbose = 0L)) * scale + center)
    past <- y
    fc <- numeric(p$h)
    for (j in seq_len(p$h)) {
      input <- array(tail(past, Lookback), c(1L, Lookback, 1L))
      fc[j] <- as.numeric(stats::predict(model, input, verbose = 0L))[1L]
      past <- c(past, fc[j])
    }
    forecast <- as.numeric(fc * scale + center)
    if (length(forecast) != p$h || any(!is.finite(forecast)))
      stop("The model did not return a finite forecast of the requested size.", call. = FALSE)
    if (length(fitted) != p$k)
      stop("Fitted values do not match the training data.", call. = FALSE)
    result <- list(
      Forecast = forecast, ForecastTime = p$ForecastTime,
      TrainData = p$Train, TrainTime = p$TrainTime,
      Fitted = fitted, Residuals = p$Train - fitted,
      TestData = p$TestData, TestTime = p$ForecastTime,
      Lower = NULL, Upper = NULL,
      Level = NULL, Model = model,
      Accuracy = QualityMeasuresForecasting(p$TestData, forecast,
        TrainingData = p$Train, SeasonalPeriod = p$Season
      ),
      Info = list(
        Method = "LSTM", SplitAt = p$k, Origin = tail(p$TrainTime, 1L),
        ForecastHorizon = p$h, TimeStep = p$Step, SeasonalPeriod = p$Season,
        Scale = "response",
        Lookback = Lookback, ScalingCenter = center, ScalingScale = scale,
        Reproducibility = "Seed sets R and TensorFlow random states; hardware can affect results"
      ),
      FitStats = history, ForecastTrain = fitted
    )
    if (PlotIt) PlotForecast(result)
    result
  })
}

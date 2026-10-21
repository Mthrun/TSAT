# Fit the additive compound model and append its response forecasts.
forecastAdditiveFit <- function(Data, SeasonalLength, NoSteps, alpha, beta, gamma,
                                Silent = FALSE) {
  n <- length(Data)
  m <- SeasonalLength
  # Use two cycles to initialize the level, trend, and seasonal components.
  # Do not score these observations: they are part of the initialization.
  first <- Data[seq_len(m)]
  second <- Data[m + seq_len(m)]
  trend <- (mean(second) - mean(first)) / m
  level <- mean(second) + trend * (m - 1) / 2
  season <- (first - mean(first) + second - mean(second)) / 2 -
    trend * (seq_len(m) - (m + 1) / 2)
  values <- rep(NA_real_, n + NoSteps)
  if (n > 2L * m) for (i in seq.int(2L * m + 1L, n)) {
    # R indices start at one. Use the same seasonal slot in every cycle.
    slot <- (i - 1L) %% m + 1L
    # Record the prediction before the current observation updates the state.
    values[i] <- level + trend + season[slot]
    previous <- level
    level <- alpha * (Data[i] - season[slot]) + (1 - alpha) * (level + trend)
    trend <- beta * (level - previous) + (1 - beta) * trend
    season[slot] <- gamma * (Data[i] - level) + (1 - gamma) * season[slot]
  }
  if (NoSteps > 0L) for (h in seq_len(NoSteps))
    values[n + h] <- level + h * trend + season[(n + h - 1L) %% m + 1L]
  values
}

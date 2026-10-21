# Score one-step predictions after the two initialization cycles.
forecastAdditiveLoss <- function(parameters, Data, SeasonalPeriod) {
  fitted <- suppressWarnings(forecastAdditiveFit(
    Data, SeasonalPeriod, 0L,
    parameters[1L], parameters[2L], parameters[3L], TRUE
  ))
  # Each prediction precedes the update with its observation. Squared error has
  # a finite derivative at a perfect prediction; rooted absolute error does not.
  mean((Data - fitted)^2, na.rm = TRUE)
}

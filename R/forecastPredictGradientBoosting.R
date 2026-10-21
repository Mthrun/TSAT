# Preserve the feature order used to fit the boosting model.
forecastPredictGradientBoosting <- function(new, Model, Features) {
  stats::predict(Model, newdata = as.matrix(new[Features]))
}

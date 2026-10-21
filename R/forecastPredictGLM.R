# Use the response scale. A GLM link value is not a response forecast.
forecastPredictGLM <- function(new, Model) {
  stats::predict(Model, newdata = new, type = "response")
}

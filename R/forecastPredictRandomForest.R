# Return response predictions from the selected random forest backend.
forecastPredictRandomForest <- function(new, Model, Package) {
  if (Package == "randomForest") stats::predict(Model, newdata = new) else
    stats::predict(Model, data = new)$predictions
}

# Apply the training scale to inputs, then restore the response scale.
forecastPredictMLP <- function(new, Model, Center, Scale, Repetition) {
  new <- sweep(sweep(as.matrix(new), 2, Center[-1L]), 2, Scale[-1L], "/")
  as.numeric(neuralnet::compute(Model, new, rep = Repetition)$net.result) *
    Scale[1L] + Center[1L]
}

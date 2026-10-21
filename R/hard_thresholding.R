hard_thresholding = function(Data, Lambda) {
  HardThreshold = quantile(abs(Data), probs = Lambda)
  # Keep the requested quantile. Replacing it with the maximum discards too much.
  Data[abs(Data) < HardThreshold] = 0
  return(Data)
}

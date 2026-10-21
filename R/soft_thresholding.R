soft_thresholding = function(Data, Lambda) {
  threshold <- stats::quantile(abs(Data), probs = Lambda)
  # Shrink the magnitude and restore its sign. Subtracting from a negative
  # coefficient before taking abs() increases its magnitude instead.
  sign(Data) * pmax(abs(Data) - threshold, 0)
}

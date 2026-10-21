SMAPE <- function(X, Y, epsilon = 10^-10, na.rm = FALSE, Silent = TRUE) {
  if (!is.numeric(X) || !is.numeric(Y) || length(X) != length(Y))
    stop("X and Y must be numeric vectors of equal length.")
  if (length(epsilon) != 1L || !is.finite(epsilon) || epsilon < 0)
    stop("epsilon must be one nonnegative number.")
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("na.rm must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  keep <- is.finite(X) & is.finite(Y)
  if (!na.rm && any(!keep)) stop("Supply finite values or set na.rm = TRUE.")
  X <- X[keep]; Y <- Y[keep]
  denominator <- abs(X) + abs(Y)
  # Use the same 0--200 percent convention as the forecast interface.
  # Silent controls messages only. It must not change the return dimensions.
  out <- numeric(length(X))
  use <- denominator > epsilon
  out[use] <- 200 * abs(X[use] - Y[use]) / denominator[use]
  out
}

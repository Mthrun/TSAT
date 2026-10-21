LagVector <- function(x, k) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k != trunc(k))
    stop("k must be one finite integer.")
  if (!is.null(dim(x))) stop("x must be a vector.")
  n <- length(x)
  if (!n || k == 0) return(x)
  # Indexing with NA preserves Date, POSIXct, factor, and vector classes.
  # A lag is a shift with missing values. It must not wrap around.
  if (abs(k) >= n) return(x[rep(NA_integer_, n)])
  index <- seq_len(n) - k
  index[index < 1L | index > n] <- NA_integer_
  x[index]
}

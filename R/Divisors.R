Divisors <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x == 0 || x != trunc(x) || abs(x) > 2^53)
    stop("x must be one nonzero, exactly represented whole number.")
  x <- abs(x)
  # Divisors occur in pairs. Search only through the square root of x.
  candidates <- seq_len(floor(sqrt(x)))
  lower <- candidates[x %% candidates == 0]
  sort(unique(c(lower,x/lower)))
}

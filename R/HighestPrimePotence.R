HighestPrimePotence <- function(x) {
  # Fractions and nonfinite values cannot reach the old loop's stopping condition.
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x == 0 || x != trunc(x) || abs(x) > 2^53)
    stop("x must be one nonzero, exactly represented whole number.")
  x <- abs(x)
  largest <- 1
  divisor <- 2
  while (divisor <= sqrt(x)) {
    power <- 1
    while (x %% divisor == 0) {
      x <- x/divisor
      power <- power*divisor
    }
    largest <- max(largest,power)
    divisor <- divisor+1
  }
  max(largest,x)
}

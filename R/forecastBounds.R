# Form Gaussian prediction bounds from response means and standard errors.
forecastBounds <- function(mean, se, level) {
  q <- stats::qnorm((1 + level / 100) / 2)
  if (is.matrix(mean) && NCOL(mean) > 1L) {
    lower <- upper <- array(NA_real_, c(NROW(mean), NCOL(mean), length(level)),
      dimnames = list(NULL, colnames(mean), paste0(level, "%"))
    )
    for (j in seq_along(level)) {
      lower[, , j] <- mean - q[j] * se
      upper[, , j] <- mean + q[j] * se
    }
  } else {
    lower <- outer(as.numeric(mean), rep(1, length(level))) - outer(as.numeric(se), q)
    upper <- outer(as.numeric(mean), rep(1, length(level))) + outer(as.numeric(se), q)
    colnames(lower) <- colnames(upper) <- paste0(level, "%")
  }
  list(Lower = lower, Upper = upper)
}

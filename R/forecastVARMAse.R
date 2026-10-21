forecastVARMAse <- function(fit, h, integrated) {
  d <- NCOL(fit$data)
  psi <- list(diag(d))
  cumulative <- diag(d)
  covariance <- matrix(0, d, d)
  se <- matrix(NA_real_, h, d)
  for (step in seq_len(h)) {
    lag <- step - 1L
    if (lag > 0L) {
      current <- matrix(0, d, d)
      for (j in seq_len(min(lag, fit$ARorder))) {
        block <- fit$Phi[, (j - 1L) * d + seq_len(d), drop = FALSE]
        current <- current + block %*% psi[[lag - j + 1L]]
      }
      if (lag <= fit$MAorder)
        current <- current - fit$Theta[, (lag - 1L) * d + seq_len(d), drop = FALSE]
      psi[[lag + 1L]] <- current
      cumulative <- cumulative + current
    }
    coefficient <- if (integrated) cumulative else psi[[lag + 1L]]
    covariance <- covariance + coefficient %*% fit$Sigma %*% t(coefficient)
    se[step, ] <- sqrt(pmax(diag(covariance), 0))
  }
  se
}

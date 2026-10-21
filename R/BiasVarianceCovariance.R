BiasVarianceCovariance = function(y_hat, y) {
  # forecasting ensemble of M models from the perspective of regression: bias^2 +mean(var)*1/M+(1-1/M)mean(covar)
  # sodass yhad[1:N,1:M] und y[1:n]
  # Sources
  # Brown, G., Wyatt, J., Harris, R., & Yao, X. (2005). Diversity creation methods: a survey and categorisation. Information fusion, 6(1), 5–20.

  # Brown, Gavin, et al. “Managing diversity in regression ensembles.” Journal of machine learning research 6.9 (2005).

  # Calculate the three terms here. Each local helper was used only once.
  M <- ncol(y_hat)
  squared.bias <- mean((colMeans(y_hat) - mean(y))^2)
  average.variance <- mean(apply(y_hat, 2, stats::var) / M)
  covariance <- cov(y_hat)
  diag(covariance) <- 0
  average.covariance <- sum(covariance) * (1 / (M * (M - 1)))
  list(
    SquaredBias = squared.bias, AverageVariance = average.variance,
    AverageCov = average.covariance
  )
}

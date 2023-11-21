BiasVarianceCovariance = function(y_hat, y) {

  #forecasting ensemble of M models from the perspective of regression: bias^2 +mean(var)*1/M+(1-1/M)mean(covar)
  #sodass yhad[1:N,1:M] und y[1:n]
#Sources
#Brown, G., Wyatt, J., Harris, R., & Yao, X. (2005). Diversity creation methods: a survey and categorisation. Information fusion, 6(1), 5–20.
  
# Brown, Gavin, et al. “Managing diversity in regression ensembles.” Journal of machine learning research 6.9 (2005).
  
    avg_sqr_bias = function(y_hat, y) {
      return(mean((colMeans(y_hat) - mean(y))^2))
    }
    avg_var = function(y_hat) {
      M <- ncol(y_hat)
      return(mean(apply(y_hat, 2, var) / M))
    }
    avg_cov = function(y_hat) {
      M <- ncol(y_hat)
      cov_matrix <- cov(y_hat)
      diag(cov_matrix) <- 0
      cov_term <- sum(cov_matrix) * (1 / (M * (M - 1)))
      return(cov_term)
    }
    return(list(SquaredBias=avg_sqr_bias(y_hat, y), AverageVariance=avg_var(y_hat), AverageCov=avg_cov(y_hat)))
  }


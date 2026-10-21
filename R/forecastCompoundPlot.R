forecastCompoundPlot <- function(result) {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  graphics::par(pty = "m")
  x <- c(result$TrainTime, result$ForecastTime)
  y <- c(result$TrainData, result$TestData)
  graphics::plot(x, y,
    type = "l", xlab = "Time", ylab = "Response",
    ylim = range(c(y, result$Fitted, result$Forecast), finite = TRUE)
  )
  graphics::points(x, c(result$Fitted, result$Forecast), col = "red")
  graphics::abline(v = as.numeric(tail(result$TrainTime, 1L)), col = "magenta")
}

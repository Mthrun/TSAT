\name{WhiteNoiseTest}
\alias{WhiteNoiseTest}
\title{Test for serial correlation}
\description{Apply the Ljung-Box or Box-Pierce test to a numeric series.}
\usage{
WhiteNoiseTest(TimeSeries, lags = 1, type = "Ljung-Box", PlotIt = TRUE) 
}
\arguments{
  \item{TimeSeries}{Finite, nonconstant numeric vector.}
  \item{lags}{Positive number of lags. Must be smaller than the series length.}
  \item{type}{Ljung-Box or Box-Pierce. Legacy values c, nc, and ct select Ljung-Box with a warning.}
  \item{PlotIt}{If TRUE, display a histogram and a fitted normal reference curve.}
}
\details{
The null hypothesis is zero autocorrelation at the tested lags. A small p-value gives evidence against that hypothesis. A large p-value does not prove white noise, independence, or normality. The previous unit-root test did not test this hypothesis. The normal reference curve is a visual aid only. For fitted model residuals, use stats::Box.test directly when a model-specific fitdf correction is needed.
}
\value{
An invisible htest object from stats::Box.test.
}
\examples{
set.seed(8)
y <- rnorm(100)
out <- WhiteNoiseTest(y,lags=5,PlotIt=FALSE)
stopifnot(all.equal(out$p.value,Box.test(y,lag=5,type="Ljung-Box")$p.value))
}

\keyword{ts}

\name{MovingAverage}
\alias{MovingAverage}
\title{Trailing moving average}
\description{Average the current observation and its preceding observations.}
\usage{
MovingAverage(Data, lag, PlotIt = FALSE) 
}
\arguments{
  \item{Data}{Numeric vector, time series, or matrix. Matrix columns are separate series.}
  \item{lag}{Positive integer. Maximum number of observations in each window.}
  \item{PlotIt}{If TRUE, compare the input and filtered values in a plot.}
}
\details{
The first windows use only the available history. Missing values are omitted within each window. An empty window returns NaN. No future observation is used.
}
\value{
Filtered values with the same length or matrix dimensions as Data. A univariate time series retains its time index.
}
\examples{
x <- 1:10
y <- MovingAverage(x,3)
stopifnot(y[1] == 1, y[10] == 9)
}

\keyword{ts}

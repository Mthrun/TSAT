\name{PlotForecast}
\alias{PlotForecast}
\title{Plot a Forecast Result}
\description{
Show the training response, forecast, available test observations, and
prediction intervals from a TSAT forecast result.
}
\usage{PlotForecast(result)}
\arguments{
\item{result}{A result from a TSAT forecast function. Required fields are
\code{TrainData}, \code{TrainTime}, \code{Forecast}, \code{ForecastTime},
and \code{TestData}. If intervals are present, supply \code{Lower},
\code{Upper}, and \code{Level}.}
}
\details{
The training response is a black line. The forecast is a red line with
points. Black points show observed test values. Blue bands show the supplied
prediction intervals. A vertical line marks the last training observation.
Multiple response columns use separate panels.

This function displays existing results. It does not fit the model or
recalculate intervals. It restores the previous graphics settings on exit.
The compound model and Prophet retain their own plots in their forecast
functions. This function can also show their common result fields.
}
\value{The supplied result, returned invisibly.}
\seealso{\code{\link{ForecastingInterface}}, \code{\link{KalmanForecastModel}}}
\examples{
fit <- KalmanForecastModel(DataVec = as.numeric(Nile),
  SplitAt = 80, ForecastHorizon = 5, PlotIt = FALSE)
PlotForecast(fit)
}

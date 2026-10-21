\name{RolingForecastTemplate}
\alias{RolingForecastTemplate}
\title{Evaluate forecasts from a data frame}
\description{Internal adapter for the legacy data-frame interface. Use \code{\link{RollingForecast}} in new calls.}
\usage{
RolingForecastTemplate(Dataframe, ForecastingFunction = FcRandomForest,
  Horizon = 7, Steps = 371:7, TimeColumnInd = 1, PlotMAE = FALSE,
  tz = "UTC", ...)
}
\arguments{
\item{Dataframe}{Data frame with time, response, and optional predictor columns. The first column after removal of the time column is the response.}
\item{ForecastingFunction}{A function that accepts the common forecast inputs.}
\item{Horizon}{Positive integer. Number of forecast steps per origin.}
\item{Steps}{Number of observations after each forecast origin. Each value must be at least \code{Horizon}.}
\item{TimeColumnInd}{Index of the time column.}
\item{PlotMAE}{If \code{TRUE}, display mean absolute error by forecast origin.}
\item{tz}{Legacy time-zone argument. Supply timestamps in the required time zone before the call.}
\item{\dots}{Additional model arguments passed through \code{RollingForecast}.}
}
\details{
The training set expands at each origin.
Predictors after an origin must be known at that origin.
Model failures remain visible in the result tables.
The function name retains its original spelling.
}
\value{
A list with \code{RollingForecasts}, \code{MAE}, and \code{Accuracy}.
\code{RollingForecasts} contains one evaluation table per origin.
\code{MAE} contains the mean absolute error at each origin.
\code{Accuracy} summarizes each lead time.
}
\seealso{\code{\link{RollingForecast}}}
\examples{
frame <- data.frame(Time = seq_along(Nile), Response = as.numeric(Nile))
out <- TSAT:::RolingForecastTemplate(frame,
  ForecastingFunction = KalmanForecastModel, Horizon = 3, Steps = c(20, 10))
stopifnot(length(out$RollingForecasts) == 2L)
}
\keyword{ts}
\keyword{internal}

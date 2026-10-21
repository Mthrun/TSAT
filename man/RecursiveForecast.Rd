\name{RecursiveForecast}
\alias{RecursiveForecast}
\title{Make Recursive Response Forecasts}
\description{
Predict one response value at a time. Append each prediction to the response
history before the next step.
}
\usage{
RecursiveForecast(TrainData, ForecastHorizon, PredictFunction,
  Lags = 1L, FuturePredictors = NULL, ...)
}
\arguments{
\item{TrainData}{A nonempty, finite numeric response vector. Supply training
observations only.}
\item{ForecastHorizon}{A positive integer. The number of future observations.}
\item{PredictFunction}{A function whose first argument is one data frame row.
It must return one finite numeric response value. It can use a fitted model
supplied through \code{...} or stored in its environment.}
\item{Lags}{Distinct positive integers within the training history.
Lag 1 is the previous response. Use \code{NULL} or \code{integer()} to
use only the supplied predictors.}
\item{FuturePredictors}{An optional data frame with one row per forecast step.
The values must be known at the forecast origin. Column names must be unique.
Do not use names that conflict with selected lag columns.}
\item{...}{Additional named arguments passed to \code{PredictFunction}.}
}
\details{
Each prediction row contains the future predictors followed by response
lag columns named \code{.Lag1}, \code{.Lag2}, and so on.
For later steps, lag columns can contain earlier predictions.
The function never reads observed responses after the forecast origin.

Fit the model before calling this function. Use \code{\link{LagVector}} to
prepare lag columns for training. Remove initial rows whose lag values are
not available. Use the same lag names and predictor columns for training
and prediction. Apply any input scaling inside \code{PredictFunction} and
return predictions on the response scale.
}
\value{A numeric vector with \code{ForecastHorizon} response predictions.}
\seealso{\code{\link{LagVector}}, \code{\link{FcGeneralizedLinearModels4TS}},
\code{\link{RollingForecast}}}
\examples{
# Repeat a one-step rule. Each step uses the preceding prediction.
one.step <- function(new, increment) new$.Lag1 + increment
values <- RecursiveForecast(1:10, 3, one.step, increment = 2)
stopifnot(identical(values, c(12, 14, 16)))

# Combine a known future input with the previous response.
with.input <- function(new) new$.Lag1 + new$input
values <- RecursiveForecast(1:10, 3, with.input,
  FuturePredictors = data.frame(input = c(1, 2, 3)))
stopifnot(identical(values, c(11, 13, 16)))
}

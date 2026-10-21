\name{RollingForecast}
\alias{RollingForecast}
\title{Evaluate forecasts at multiple origins}
\description{Fit a forecast model at each supplied origin and evaluate each lead time.}
\usage{
RollingForecast(DataVec, ForecastingFunction = FcAutoARIMA, Origins,
  ForecastHorizon = 1L, Time = NULL, Window = NULL, Predictors = NULL,
  ContinueOnError = TRUE, \dots)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{ForecastingFunction}{Forecast function with the common named input and result fields. Use a function such as \code{FcAutoARIMA} or \code{KalmanForecastModel}.}
  \item{Origins}{Distinct indices of the last training observations. Each origin must leave at least \code{ForecastHorizon} observed test rows.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{Window}{Optional training-window length. If omitted, expand the training set from the first observation. Otherwise use at most this many recent observations.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{ContinueOnError}{If \code{TRUE}, record a failed model with missing predictions and its error message. If \code{FALSE}, stop at the first model error.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
}
\details{
Each fit receives only its training response and training predictors. Future timestamps and future predictors can be supplied because they must be known at the origin. The procedure evaluates observed test data after prediction.

A fixed window moves with the origin. Without a window, the training set expands. MASE uses a separate training scale at each origin. Model failures remain visible in the result table. Additional arguments go to \code{ForecastingFunction}.
}
\value{
A list with \code{Forecasts}, \code{Accuracy}, and \code{Results}.
\code{Forecasts} has one row per origin, lead, and response series. It includes timestamps, predictions, actual values, errors, scaled absolute errors, status, and an error message.
\code{Accuracy} summarizes each lead and series. It includes the valid count, MAE, RMSE, bias, and MASE.
\code{Results} contains the model result or error for each origin.
}


\seealso{\code{\link{ForecastingInterface}}}
\examples{
out <- RollingForecast(as.numeric(Nile),
  ForecastingFunction = KalmanForecastModel,
  Origins = c(70, 80), ForecastHorizon = 3)
stopifnot(nrow(out$Forecasts) == 6, all(out$Forecasts$Status == "ok"))
}
\keyword{ts}

\name{FcSeasonalArima}
\alias{FcSeasonalArima}
\title{X-13 ARIMA-SEATS forecasts}
\description{Fit a seasonal ARIMA model with X-13 and return future response values.}
\usage{
FcSeasonalArima(DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Frequency = NULL, Time = NULL, PlotIt = FALSE, \dots, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Level = 95)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Frequency}{Legacy name for \code{TimeStep}. The function does not aggregate observations. Aggregate data before the call.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
}
\details{
This method requires monthly or quarterly observations. Use a seasonal period of 12 or 4 and a valid calendar start. A \code{ts} input retains its start. Legacy \code{Frequency = "months"} or \code{"quarters"} can also set the period when it is omitted.

Additional arguments go to \code{seasonal::seas}. The wrapper selects exactly the requested forecast rows. The extra fields \code{ForecastMaxlead} and \code{TaT} retain the X-13 forecast table and the split data.
}
\value{
A named list with the common fields described in \code{\link{ForecastingInterface}}:
\item{Forecast}{Response predictions. A vector of length \code{ForecastHorizon}, or a matrix with one column per series.}
\item{ForecastTime}{One timestamp per forecast row.}
\item{TrainData, TrainTime}{The observed training response and its timestamps.}
\item{Fitted, Residuals}{Training fitted values and observed-minus-fitted residuals. Unsupported fields are \code{NULL}. Initial values can be missing.}
\item{TestData, TestTime}{Observed response values aligned with the forecast rows and their timestamps. Unknown future responses are \code{NA}.}
\item{Lower, Upper, Level}{Prediction bounds and coverage percentages. Unsupported intervals are \code{NULL}. Univariate bounds have one column per level. Multivariate bounds have dimensions horizon by series by level.}
\item{Model}{The fitted model or model parameters.}
\item{Accuracy}{The matrix from \code{\link{QualityMeasuresForecasting}}, with one row per response series. It contains N, MAE, RMSE, Bias, MASE, MAPE, SMAPE, AbsoluteError, RAE, RSE, MRD, and MRD_BIAS. It is \code{NULL} when no finite test comparisons are available.}
\item{Info}{Method name, split index, forecast origin, horizon, time interval, seasonal period, response scale, and method-specific information.}
See Details for additional fields.
}
\references{
\url{https://www.census.gov/ts/x13as/docX13AS.pdf}

\url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R#77-forecast}
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("seasonal", quietly = TRUE)) {
  out <- FcSeasonalArima(AirPassengers, SplitAt = 120, ForecastHorizon = 3)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

\name{FcVARIMA}
\alias{FcVARIMA}
\title{Multivariate VARIMA forecasts}
\description{Fit a vector ARMA model, with optional first differencing, and forecast all series.}
\usage{
FcVARIMA(Data, ARp = 1L, i = 1L, MAq = 0L, ForecastHorizont = NULL,
  PlotIt = FALSE, Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95),
  Refine = TRUE, Silent = TRUE)
}
\arguments{
  \item{Data}{Legacy response input. Use \code{DataVec} for univariate methods. For VARIMA, supply a numeric matrix with one series per column.}
  \item{ARp}{Nonnegative integer. VARMA autoregressive order.}
  \item{i}{Differencing order, zero or one. Differencing uses only the training series.}
  \item{MAq}{Nonnegative integer. VARMA moving-average order.}
  \item{ForecastHorizont}{Legacy name for \code{ForecastHorizon}. If the split is omitted, this legacy argument also selects a test set of that length.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{Refine}{If \code{TRUE}, refine the initial VARMA fit with \code{MTS::refVARMA}.}
  \item{Silent}{Control model messages. The VARIMA wrapper suppresses model output when \code{TRUE}. The LSTM wrapper suppresses training progress when \code{TRUE}. Errors remain visible.}
}
\details{
The method uses \code{MTS::VARMA} and optionally \code{MTS::refVARMA}. With \code{i = 1}, the wrapper integrates forecasts back to the response scale.

Gaussian interval variances use the accumulated impulse-response coefficients, including integration. They treat estimated parameters as known.

The default horizon is 14. The legacy \code{ForecastHorizont} argument selects a test tail when the split is omitted. \code{Train}, \code{Test}, and \code{ModelForecast} retain additional model results.
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
[Tsay, 2013]  Tsay, R. S.: Multivariate time series analysis: with R and financial applications, John Wiley & Sons, ISBN: 978-1-118-61790-8, 2013.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("MTS", quietly = TRUE)) {
  y <- cbind(as.numeric(Nile), rev(as.numeric(Nile)))
  out <- FcVARIMA(y, SplitAt = 80, ForecastHorizon = 3, Refine = FALSE)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

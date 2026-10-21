\name{FcAutoARIMA}
\alias{FcAutoARIMA}
\title{Automatic ARIMA forecasts}
\description{Fit an automatically selected ARIMA model and predict from one forecast origin.}
\usage{
FcAutoARIMA(DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Time = NULL, PlotIt = FALSE, Seasonal = TRUE, PlotBackwardInd = NULL,
  main = "", xlab = "Time", ylab = "Data", \dots, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95),
  Predictors = NULL, FuturePredictors = NULL)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Seasonal}{If \code{TRUE}, allow seasonal ARIMA terms. Set \code{SeasonalPeriod} to identify the cycle.}
  \item{PlotBackwardInd}{Legacy plot argument. Accepted for call compatibility. The common plot displays the supplied training history.}
  \item{main}{Legacy plot title. Accepted for call compatibility.}
  \item{xlab}{Label for the time axis in the model-specific plot. In the ARIMA wrapper, this legacy argument is accepted for call compatibility.}
  \item{ylab}{Label for the response axis in the model-specific plot. In the ARIMA wrapper, this legacy argument is accepted for call compatibility.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
}
\details{
Additional arguments go to \code{forecast::auto.arima}. The fit uses the training observations and training predictors only. Prediction intervals come from \code{forecast::forecast}. The extra field \code{ArimaObject} contains its full forecast object.
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
Hyndman, RJ and Khandakar, Y (2008) "Automatic time series forecasting: The forecast package for R", Journal of Statistical Software, 26(3).

Wang, X, Smith, KA, Hyndman, RJ (2006) "Characteristic-based clustering for time series data", Data Mining and Knowledge Discovery, 13(3), 335-364.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("forecast", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcAutoARIMA(y, SplitAt = 80, ForecastHorizon = 3, Seasonal = FALSE)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

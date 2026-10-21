\name{FcEchoStateANN}
\alias{FcEchoStateANN}
\title{Echo state network forecasts}
\description{Fit an echo state network and predict a fixed number of future observations.}
\usage{
FcEchoStateANN(DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Seasonality = NULL, Scaled = TRUE, Time = NULL, PlotIt = FALSE,
  NoRolling = TRUE, \dots, Lookback = NULL, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Seasonality}{Legacy name for \code{Lookback} in the neural methods. It controls the input history, not the seasonal period.}
  \item{Scaled}{If \code{TRUE}, estimate scaling values from training data only. Apply those values during prediction and return response values on the original scale.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{NoRolling}{Must be \code{TRUE}. Use \code{\link{RollingForecast}} to evaluate several forecast origins.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Additional arguments go to \code{echos::train_esn}. The default input window is 28 observations. The wrapper calls \code{echos::forecast_esn} once with the requested horizon. It does not update the model with test responses.

Use \code{RollingForecast} for multiple origins.

\code{FitStats} stores the model prediction object. Fitted values and prediction intervals are not supplied.
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
Jaeger H. (2003): Adaptive nonlinear system identification with echo state networks. In Advances in Neural Information Processing Systems 15, S. Becker, S. Thrun, K. Obermayer (Eds), (MIT Press, Cambridge, MA, 2003) pp. 593-600 
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("echos", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcEchoStateANN(y, SplitAt = 80, ForecastHorizon = 3,
    Lookback = 4, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

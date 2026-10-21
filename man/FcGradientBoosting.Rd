\name{FcGradientBoosting}
\alias{FcGradientBoosting}
\title{Gradient boosting forecasts}
\description{Fit a tree boosting regression model and predict future response values.}
\usage{
FcGradientBoosting(DataVec, SplitAt = length(DataVec), Time = NULL,
  ForecastHorizon = 1L, Frequency = NULL, FUN = sum, PlotIt = FALSE,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL,
  Predictors = NULL, FuturePredictors = NULL, Lags = NULL, Lookback = NULL,
  Rounds = 100L, Params = list(max_depth = 3L, eta = 0.1, nthread = 1L), \dots)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Frequency}{Legacy name for \code{TimeStep}. The function does not aggregate observations. Aggregate data before the call.}
  \item{FUN}{Legacy aggregation function argument. No aggregation occurs in this wrapper. Prepare the data before the call.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{Lags}{Optional vector of distinct positive lag indices. For example, \code{c(1, 7)} uses the previous observation and the observation seven steps earlier. Later forecast steps use prior predictions when the required response is not yet observed.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{Rounds}{Positive integer. Number of boosting rounds passed to \code{xgboost::xgb.train}.}
  \item{Params}{Named list of XGBoost training parameters. This wrapper requires the squared-error regression objective. The default uses one processor thread.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
}
\details{
The method uses \code{xgboost::xgb.train}. Additional arguments go to that function. This replaces the unfinished forecastxgb wrapper.

With no external predictors or selected lags, use up to eight response lags. Forecasts use prior predictions for unavailable responses.

A seasonal period greater than one adds known seasonal phase indicators and requires two training cycles. External predictors must be numeric.

The wrapper does not aggregate data. \code{Fitted} contains in-sample regression predictions. \code{ForecastStats$mean} contains the forecast vector. Prediction intervals are not supplied.
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
[Chen/Guestrin, 2016]   Chen, Tianqi ans Guestrin, Carlos: XGBoost: A Scalable Tree Boosting System, In Krishnapuram, Balaji Shah, Mohak Smola, Alexander J. Aggarwal, Charu C., Shen, Dou, Rastogi, Rajeev, Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, San Francisco, CA, USA, August 13-17, 2016, ACM, pp. 785–794. arXiv:1603.02754,  doi:10.1145/2939672.2939785, 2016.

\url{https://medium.com/@rakesh.poojary/forecasting-markets-using-extreme-gradient-boosting-xgboost-9c62605c4b83}
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("xgboost", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcGradientBoosting(y, SplitAt = 80, ForecastHorizon = 3, Rounds = 10, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

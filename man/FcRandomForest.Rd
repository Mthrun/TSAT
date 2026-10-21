\name{FcRandomForest}
\alias{FcRandomForest}
\title{Random forest forecasts}
\description{Fit a random forest with external predictors, response lags, or calendar variables.}
\usage{
FcRandomForest(Time = NULL, DF = NULL, formula = NULL, SplitDataAt = NULL,
  Horizon = NULL, Package = "randomForest", AutoCorrelation = NULL,
  AutoCorrelationNo = 1, NoOfTree = 200, PlotIt = FALSE, Holidays = NULL,
  SimilarPoints = FALSE, tz = "UTC", \dots, DataVec = NULL, SplitAt = NULL,
  ForecastHorizon = NULL, Predictors = NULL, FuturePredictors = NULL,
  Lags = NULL, Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Seed = NULL)
}
\arguments{
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{DF}{Legacy data frame. The formula identifies the response column. Without a formula, the first column is the response and other columns are external predictors.}
  \item{formula}{Optional regression formula for \code{DF}. Calendar columns can be used when calendar timestamps are supplied. The wrapper preserves the right-hand side and uses its internal response name.}
  \item{SplitDataAt}{Legacy name for \code{SplitAt}.}
  \item{Horizon}{Legacy forecast horizon. In the random forest and optimized compound wrappers, an omitted split with this argument selects the last \code{Horizon} observations as test data.}
  \item{Package}{Random forest implementation: \code{"randomForest"} or \code{"ranger"}.}
  \item{AutoCorrelation}{Legacy column index for the response in \code{DF}. It enables response lags. Use \code{Lags} in new calls.}
  \item{AutoCorrelationNo}{Legacy number of consecutive response lags.}
  \item{NoOfTree}{Positive integer. Number of trees.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Holidays}{For Prophet, a holiday data frame accepted by \code{prophet}. For random forest, an optional data frame with a \code{Time} column. Dates must be known at the forecast origin.}
  \item{SimilarPoints}{Legacy option. Must be \code{FALSE}. The old feature used future response values and is not a valid forecast predictor.}
  \item{tz}{Time zone used for calendar conversion where applicable. The default is UTC.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{Lags}{Optional vector of distinct positive lag indices. For example, \code{c(1, 7)} uses the previous observation and the observation seven steps earlier. Later forecast steps use prior predictions when the required response is not yet observed.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Additional arguments go to the selected random forest function.

With calendar timestamps and a formula, available columns include \code{Weekdays}, \code{WeekNo}, \code{Months}, \code{Quarters}, \code{Years}, \code{Holidays}, and \code{Workingdays}. Weekdays use Monday = 1. Working days exclude weekends and supplied holidays. No national holiday calendar is assumed.

The old \code{SimilarPoints} feature is rejected because it used future responses. If no predictors or lags are supplied, use lag one.

\code{Fitted} contains in-sample regression predictions, not out-of-bag predictions. \code{FeatureImportance} stores model importance values. Prediction intervals are not supplied.

New calls with \code{DataVec} default to a horizon of one. Legacy response calls without a horizon use the available test tail, or one step when there is no test tail.
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
[Breiman, 2001]  Breiman, L., Random Forests, Machine Learning 45(1), 5-32, 2001.

[Hastie, 2014]  Hastie, TREVOR, Tibshirani, ROBERT, Friedman, JH: The elements of statistical learning: data mining, inference, and prediction, pages 587ff, 2013.

[Thrun et al., 2019]   Thrun, M., Maerte, J., Boehme, P, and Gehlert, T.: Applying Two Theorems of Machine Learning to the Forecasting of Biweekly Arrivals at a Call Center, Proceedings of ECDA, accepted, Bayreuth, 2019.

}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("randomForest", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcRandomForest(DataVec = y, SplitAt = 80, ForecastHorizon = 3,
    Lookback = 2, NoOfTree = 10, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

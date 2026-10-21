\name{FcExtremeLearningMachines}
\alias{FcExtremeLearningMachines}
\title{Extreme learning machine forecasts}
\description{Fit an extreme learning machine with training data and optional external predictors.}
\usage{
FcExtremeLearningMachines(DataVec, Time = NULL, SplitAt = length(DataVec),
  Predictors = NULL, ForecastHorizon = 1L, No_HiddenLayers = NULL,
  Scaled = TRUE, No_TrainingNetworks = 10, PlotEvaluation = FALSE,
  PlotIt = FALSE, tz = "UTC", \dots, FuturePredictors = NULL, Lags = NULL,
  Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL,
  Seed = NULL)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{No_HiddenLayers}{Hidden-layer size passed as \code{hd} to \code{nnfor::elm}.}
  \item{Scaled}{If \code{TRUE}, estimate scaling values from training data only. Apply those values during prediction and return response values on the original scale.}
  \item{No_TrainingNetworks}{Positive integer. Number of ELM network fits.}
  \item{PlotEvaluation}{If \code{TRUE}, also display the model plot when \code{PlotIt} is \code{TRUE}.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{tz}{Time zone used for calendar conversion where applicable. The default is UTC.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{Lags}{Optional vector of distinct positive lag indices. For example, \code{c(1, 7)} uses the previous observation and the observation seven steps earlier. Later forecast steps use prior predictions when the required response is not yet observed.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Additional arguments go to \code{nnfor::elm}. The prediction call receives training and future regressors in the order required by \code{nnfor}.

Scaling parameters come from the training response only. All returned response values use the original scale. \code{Info} records the selected lags and scaling values. \code{TrainingData} is an alias for \code{TrainData}. Prediction intervals are not supplied.
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
[Huang et al., 2005]  Huang, Guang-Bin; Zhu, Qin-Yu; Siew, Chee-Kheong: "Extreme learning machine: theory and applications". Neurocomputing, Vol. 70 (1), pp. 489–501, 2005
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("nnfor", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcExtremeLearningMachines(y, SplitAt = 80, ForecastHorizon = 3,
    Lookback = 2, No_HiddenLayers = 5, No_TrainingNetworks = 1,
    Seed = 1, sel.lag = FALSE, type = "ridge", difforder = 0)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

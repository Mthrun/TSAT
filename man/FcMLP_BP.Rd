\name{FcMLP_BP}
\alias{FcMLP_BP}
\title{Multilayer perceptron forecasts}
\description{Fit a feed-forward neural network and predict from one forecast origin.}
\usage{
FcMLP_BP(Response = NULL, SplitAt = NULL, Predictor1 = NULL, Predictor2 = NULL,
  Time = NULL, HiddenLayers = 4, Threshold = 0.05, Repetitions = 10,
  ErrorFunction = "sse", PlotEvaluation = FALSE, PlotIt = FALSE, DataVec = NULL,
  ForecastHorizon = NULL, Predictors = NULL, FuturePredictors = NULL,
  Lags = NULL, Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Scaled = TRUE, Seed = NULL, \dots)
}
\arguments{
  \item{Response}{Legacy name for \code{DataVec}.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{Predictor1}{Legacy first predictor vector. Use \code{Predictors} for the new interface.}
  \item{Predictor2}{Legacy second predictor vector. Requires \code{Predictor1}.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{HiddenLayers}{Number of neurons in each hidden layer. See \code{\link[neuralnet]{neuralnet}}.}
  \item{Threshold}{Training stopping threshold for \code{neuralnet}.}
  \item{Repetitions}{Number of network fits. Select the fit with the smallest training error.}
  \item{ErrorFunction}{Training error function passed to \code{neuralnet}. The default is squared error.}
  \item{PlotEvaluation}{If \code{TRUE}, also display the model plot when \code{PlotIt} is \code{TRUE}.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{Lags}{Optional vector of distinct positive lag indices. For example, \code{c(1, 7)} uses the previous observation and the observation seven steps earlier. Later forecast steps use prior predictions when the required response is not yet observed.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Scaled}{If \code{TRUE}, estimate scaling values from training data only. Apply those values during prediction and return response values on the original scale.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
}
\details{
The method uses \code{neuralnet::neuralnet}. Additional arguments go to that function. Scaling uses only the training design matrix. The selected repetition has the smallest training error.

With response lags, the forecast uses recursive prediction. If no predictors or lags are supplied, use lag one. \code{Info} stores scaling values and the selected repetition. Prediction intervals are not supplied.

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
Riedmiller M. and Braun H.: A direct adaptive method for faster backpropagation learning: The RPROP algorithm. Proceedings of the IEEE International Conference on Neural Networks (ICNN), pages 586-591. San Francisco, 1993.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("neuralnet", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcMLP_BP(DataVec = y, SplitAt = 80, ForecastHorizon = 3,
    Lookback = 2, HiddenLayers = 2, Repetitions = 1, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

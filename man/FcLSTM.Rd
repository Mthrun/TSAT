\name{FcLSTM}
\alias{FcLSTM}
\title{LSTM forecasts}
\description{Fit a univariate LSTM network and forecast from one fixed origin.}
\usage{
FcLSTM(DataVec, SplitAt = length(DataVec), ForecastHorizon = 1L,
  Seasonality = NULL, Scaled = TRUE, ErrorLoss = "MSE", Epochs = 100,
  Neurons = 28, ActivationFunction = "relu", RecurrentActivation = "sigmoid",
  Batch_size = 1, Time = NULL, PlotIt = FALSE, Silent = TRUE, \dots,
  Lookback = NULL, TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL,
  Seed = NULL)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Seasonality}{Legacy name for \code{Lookback} in the neural methods. It controls the input history, not the seasonal period.}
  \item{Scaled}{If \code{TRUE}, estimate scaling values from training data only. Apply those values during prediction and return response values on the original scale.}
  \item{ErrorLoss}{LSTM loss: \code{"MSE"}, \code{"MAE"}, \code{"MRD"}, or \code{"SRD"}. MRD is the mean square root of absolute error. SRD is its sum.}
  \item{Epochs}{Positive integer. Number of LSTM training epochs.}
  \item{Neurons}{Positive integer. Number of LSTM units.}
  \item{ActivationFunction}{Activation function for the LSTM units.}
  \item{RecurrentActivation}{Recurrent activation function for the LSTM units.}
  \item{Batch_size}{Positive integer. Number of training samples per batch. Must not exceed the available sample count.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Silent}{Control model messages. The VARIMA wrapper suppresses model output when \code{TRUE}. The LSTM wrapper suppresses training progress when \code{TRUE}. Errors remain visible.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Each training input contains \code{Lookback} past observations. Its target is the next observation. The default input window is 28. Training does not shuffle samples.

Forecasts use prior predictions after the origin. Test response values are not network inputs. Additional arguments go to \code{keras::layer_lstm}.

The method requires the R packages keras and tensorflow and a compatible Python environment. With R keras 2.16.1, the Python environment must include tf-keras and its matching TensorFlow version. Configure that environment before the call.

The example uses one epoch to test the interface; it does not establish forecast quality. \code{FitStats} contains the training history. Prediction intervals are not supplied.
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
Goodfellow, I., Bengio, Y., Courville, A., & Bengio, Y.: Deep learning, (Vol. 1), Cambridge: MIT press, 2016.

Kingma, D. P., Ba, J.: Adam: A method for stochastic optimization, arXiv preprint arXiv:1412.6980, 2014.

Hochreiter, Sepp, Jürgen Schmidhuber: "Long short-term memory.", Neural computation, Vol 9.8, pp. 1735-1780, 1997.

Mörchen, Fabian; Time series knowledge mining. Görich & Weiershäuser, 2006.

Hinton, G. E., Srivastava, N., Krizhevsky, A., Sutskever, I., & Salakhutdinov, R. R.: Improving neural networks by preventing co-adaptation of feature detectors. arXiv preprint arXiv:1207.0580, 2012.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("tensorflow", quietly = TRUE) &&
    requireNamespace("keras", quietly = TRUE) &&
    keras::is_keras_available() &&
    isTRUE(tryCatch(!is.null(tensorflow::tf_version()),
                    error = function(e) FALSE))) {
  y <- sin(seq_len(80)/5)
  out <- FcLSTM(y, SplitAt = 60, ForecastHorizon = 3, Lookback = 4,
    Epochs = 1, Neurons = 2, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

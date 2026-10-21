\name{FcOptimizeAdditiveDecompositionModel}
\alias{FcOptimizeAdditiveDecompositionModel}
\title{Optimized additive compound forecasts}
\description{Estimate the compound model update coefficients from the training data.}
\usage{
FcOptimizeAdditiveDecompositionModel(Data = NULL, SeasonalLength = NULL,
  Horizon = NULL, Iterations = 100, PlotIt = FALSE, DataVec = NULL, Time = NULL,
  SplitAt = NULL, ForecastHorizon = NULL, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Seed = NULL)
}
\arguments{
  \item{Data}{Legacy response input. Use \code{DataVec} for univariate methods. For VARIMA, supply a numeric matrix with one series per column.}
  \item{SeasonalLength}{Legacy name for \code{SeasonalPeriod}.}
  \item{Horizon}{Legacy forecast horizon. In the random forest and optimized compound wrappers, an omitted split with this argument selects the last \code{Horizon} observations as test data.}
  \item{Iterations}{Number of random starts for the compound model optimization. Each start uses bounded L-BFGS-B optimization.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Each optimization start minimizes the mean squared one-step prediction errors. The first two cycles initialize the model and are excluded from the objective. Supply at least three complete training cycles and no partial cycle. The optimizer uses L-BFGS-B with coefficients between 0.01 and 0.99. Multiple starts are not simulated annealing. The default seasonal period is 52 and the default horizon is 12. A legacy Horizon call without SplitAt reserves the last Horizon observations for evaluation. Canonical calls fit all supplied observations by default. The compound plot and legacy result fields are preserved.
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
\references{See \code{\link[stats]{optim}} for the L-BFGS-B optimizer. See \code{\link{FcAdditiveDecompositionModel}} for the compound model references.}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
y <- 100 + seq_len(60)/5 + rep(c(-3, 0, 3, 0), 15)
out <- FcOptimizeAdditiveDecompositionModel(DataVec = y, SplitAt = 48,
  ForecastHorizon = 3, SeasonalPeriod = 4, Iterations = 2, Seed = 1)
stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
          all(is.finite(out$Forecast)))
}
\keyword{ts}

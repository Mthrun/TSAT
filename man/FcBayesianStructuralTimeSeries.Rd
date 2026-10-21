\name{FcBayesianStructuralTimeSeries}
\alias{FcBayesianStructuralTimeSeries}
\title{Bayesian structural time-series forecasts}
\description{Fit a local linear trend and an optional seasonal component with posterior simulation.}
\usage{
FcBayesianStructuralTimeSeries(DataVec, SplitAt = length(DataVec), Time = NULL,
  Frequency = NULL, ForecastHorizon = 1L, nseasons = NULL, niter = 250,
  burn = 100, PlotIt = FALSE, TimeStep = NULL, SeasonalPeriod = NULL,
  FutureTime = NULL, Level = c(80, 95), Seed = NULL)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{Frequency}{Legacy name for \code{TimeStep}. The function does not aggregate observations. Aggregate data before the call.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{nseasons}{Legacy name for \code{SeasonalPeriod}.}
  \item{niter}{Number of posterior simulation iterations. Use a larger value for an analysis than for the small example.}
  \item{burn}{Number of initial simulation iterations to discard. Must be smaller than \code{niter}.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
The model uses \code{bsts::AddLocalLinearTrend}. It adds \code{bsts::AddSeasonal} when \code{SeasonalPeriod > 1}. Prediction bounds are quantiles of posterior predictive samples after \code{burn}. Test response values do not enter prediction.

Check simulation convergence before an analysis. The short example tests the interface only. \code{ModelPrediction} stores the prediction object. \code{UpperLower} stores the upper and lower bounds for the first requested level.
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
[Scott/Varian, 2014]  Scott, S. L., & Varian, H. R.: Predicting the present with bayesian structural time series, International Journal of Mathematical Modelling and Numerical Optimisation, Vol. 5(1-2), pp. 4-23. 2014.


}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("bsts", quietly = TRUE)) {
  y <- as.numeric(Nile)
  out <- FcBayesianStructuralTimeSeries(y, SplitAt = 80,
    ForecastHorizon = 3, niter = 30, burn = 10, Seed = 1)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

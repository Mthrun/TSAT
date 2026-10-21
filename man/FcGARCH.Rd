\name{FcGARCH}
\alias{FcGARCH}
\title{GARCH response and volatility forecasts}
\description{Fit a conditional mean and variance model to the training observations.}
\usage{
FcGARCH(DataVec, VarianceModel = list(model = "sGARCH", garchOrder = c(1, 1)),
  ForecastHorizon = 1L, DistributionModel = "norm", PlotIt = FALSE,
  Summary = FALSE, \dots, Time = NULL, SplitAt = length(DataVec),
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95),
  Difference = TRUE)
}
\arguments{
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{VarianceModel}{Variance model specification passed to \code{rugarch::ugarchspec}.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{DistributionModel}{Conditional innovation distribution passed to \code{rugarch::ugarchspec}.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Summary}{For GLM, print the model summary. For GARCH, display its diagnostic plots only when \code{PlotIt} is also \code{TRUE}.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{Difference}{If \code{TRUE}, fit first differences. Add cumulative predicted differences to the last training value to return the response forecast.}
}
\details{
Additional arguments go to rugarch::ugarchspec. Difference=TRUE fits first differences and integrates the forecast mean back to the response scale. Volatility describes the differences; response intervals are not supplied in this mode. With Difference=FALSE, one-step bounds use the selected innovation distribution. For later steps with normal innovations and an ordinary ARMA mean, bounds use a Gaussian approximation with propagated ARMA forecast variance. Volatility still gives innovation standard deviations. Multi-step bounds are NULL for other distributions, fractional integration, or variance-in-mean models. These cases require simulation of future paths. All bounds treat estimated parameters as known. ModelForecast stores the native forecast. With all parameters fixed, Model is a native filter object.
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
R. F. Engle: Autoregressive Conditional Heteroskedasticity   with Estimates of the Variance of UK. Inflation. In:   Econometrica, Vol.: 50, pp. 987 - 1008, 1982.

T. Bollerslev: Generalized Autoregressive Conditional   Heteroskedasticity. In: Journal of Econometrics, Vol.: 31   No.: 3, pp. 307 - 327, 1986.

Franke, J., Härdle, W., Hafner, C. M.: Statistics of Financial Markets: An Introduction. Springer, Berlin, Heidelberg, New York, 2. Auflage, 2008.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("rugarch", quietly = TRUE)) {
  y <- as.numeric(EuStockMarkets[, 1])
  out <- FcGARCH(y, SplitAt = 150, ForecastHorizon = 3)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

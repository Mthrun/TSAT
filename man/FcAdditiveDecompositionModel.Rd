\name{FcAdditiveDecompositionModel}
\alias{FcAdditiveDecompositionModel}
\title{Additive compound forecasts}
\description{Fit additive level, trend, and seasonal components with supplied update coefficients.}
\usage{
FcAdditiveDecompositionModel(Data = NULL, SeasonalLength = NULL, NoSteps = NULL,
  alpha = 0.2, beta = 0.1, gamma = 0.1, Silent = TRUE, DataVec = NULL,
  Time = NULL, SplitAt = NULL, ForecastHorizon = NULL, TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, PlotIt = FALSE)
}
\arguments{
  \item{Data}{Legacy response input. Use \code{DataVec} for univariate methods. For VARIMA, supply a numeric matrix with one series per column.}
  \item{SeasonalLength}{Legacy name for \code{SeasonalPeriod}.}
  \item{NoSteps}{Legacy name for \code{ForecastHorizon}. The public forecast interface requires a positive horizon.}
  \item{alpha}{Level update coefficient between zero and one.}
  \item{beta}{Trend update coefficient between zero and one.}
  \item{gamma}{Seasonal update coefficient between zero and one.}
  \item{Silent}{Control model messages. The VARIMA wrapper suppresses model output when \code{TRUE}. The LSTM wrapper suppresses training progress when \code{TRUE}. Errors remain visible.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
}
\details{
The method uses an additive level, trend, and seasonal recursion. The first two complete cycles initialize the components. Their fitted values are NA. Later fitted values are one-step predictions calculated before the current observation updates the state. Training must contain at least two complete cycles and no partial cycle. The seasonal index repeats at SeasonalPeriod. Prediction intervals are not supplied. The compound plot is preserved.
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
[Lewandowski, 1984]  Lewandowski, R. "Lewandowski's FORSYS Method." The forecasting Accuracy of Major Time Series Methods (1984): 245-253

[Makridakis, 1982]  Makridakis, S., Andersen, A., Carbone, R., Fildes, R., Hibon, M., Lewandowski, R., ... & Winkler, R. (1982). The accuracy of extrapolation (time series) methods: Results of a forecasting competition. Journal of forecasting, 1(2), 111-153.

[Lewandowski, 1982]  Lewandowski, Rudolf. "Practitioners' forum. Data forecasting by forsys." Journal of Forecasting 1.2 (1982): 205-214.

[Holt, 1957] C. C. Holt (1957) Forecasting seasonals and trends by exponentially weighted moving averages, ONR Research Memorandum, Carnegie Institute of Technology 52. (reprint at http://dx.doi.org/10.1016/j.ijforecast.2003.09.015).

[Winters 1960] P. R. Winters (1960). Forecasting Data by exponentially weighted moving averages. Management Science, 6, 324–342. doi: 10.1287/mnsc.6.3.324.
}
\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
y <- 100 + seq_len(60)/5 + rep(c(-3, 0, 3, 0), 15)
out <- FcAdditiveDecompositionModel(DataVec = y, SplitAt = 48,
  ForecastHorizon = 3, SeasonalPeriod = 4)
stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
          all(is.finite(out$Forecast)))
}
\keyword{ts}

\name{FcGeneralizedLinearModels4TS}
\alias{FcGeneralizedLinearModels4TS}
\title{Generalized linear model forecasts}
\description{Fit a generalized linear model with external predictors or response lags.}
\usage{
FcGeneralizedLinearModels4TS(Response = NULL, SplitAt = NULL, Predictor1 = NULL,
  Predictor2 = NULL, Time = NULL, CorrectionFactor = FALSE, PlotIt = FALSE,
  Summary = FALSE, \dots, DataVec = NULL, ForecastHorizon = NULL,
  Predictors = NULL, FuturePredictors = NULL, Lags = NULL, Lookback = NULL,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL)
}
\arguments{
  \item{Response}{Legacy name for \code{DataVec}.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{Predictor1}{Legacy first predictor vector. Use \code{Predictors} for the new interface.}
  \item{Predictor2}{Legacy second predictor vector. Requires \code{Predictor1}.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{CorrectionFactor}{If \code{TRUE}, multiply the forecast by the ratio of the last training observation to the first forecast. A zero first forecast causes an error. This correction uses no test response values.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{Summary}{For GLM, print the model summary. For GARCH, display its diagnostic plots only when \code{PlotIt} is also \code{TRUE}.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{Lags}{Optional vector of distinct positive lag indices. For example, \code{c(1, 7)} uses the previous observation and the observation seven steps earlier. Later forecast steps use prior predictions when the required response is not yet observed.}
  \item{Lookback}{Number of consecutive past observations supplied to the model. In regression methods, this is an alternative to \code{Lags}. It does not specify a seasonal cycle.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
}
\details{
Additional arguments go to \code{stats::glm}. Predictions use \code{type = "response"}, including for nonidentity links.

If no predictors or lags are supplied, use lag one. Predictions after the origin use prior predictions for unavailable lagged responses.

The optional correction factor uses the last training observation. \code{CorrectionFactor} is also returned. Prediction intervals are not supplied.

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

\author{
Michael Thrun
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
y <- as.numeric(Nile)
out <- FcGeneralizedLinearModels4TS(DataVec = y, SplitAt = 80,
  ForecastHorizon = 3, Lookback = 2)
stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
          all(is.finite(out$Forecast)))
}
\keyword{ts}

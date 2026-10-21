\name{FcDecompositionModelByRegression}
\alias{FcDecompositionModelByRegression}
\title{Prophet decomposition forecasts}
\description{Fit Prophet trend, seasonal, holiday, and optional external-regressor components.}
\usage{
FcDecompositionModelByRegression(DataFrame = NULL, TimeColumnName = "Time",
  FeatureName = "Absatz", SplitDataAt = NULL, Frequency = NULL,
  ForecastPeriods = NULL, Holidays = NULL, PlotIt = FALSE, xlab = "Time",
  ylab = "Feature", EquiDist = TRUE, MinLowerBound = NULL, MaxUpperBound = NULL,
  Cycles = list(), Growth = "linear", tz = "UTC", \dots, DataVec = NULL,
  Time = NULL, SplitAt = NULL, ForecastHorizon = NULL, TimeStep = NULL,
  FutureTime = NULL, Level = 80, Predictors = NULL, FuturePredictors = NULL,
  PlotComponents = FALSE, Seed = NULL)
}
\arguments{
  \item{DataFrame}{Legacy Prophet data frame with time and response columns. Logistic growth can also use \code{cap} and \code{floor} columns.}
  \item{TimeColumnName}{Name of the time column in \code{DataFrame}.}
  \item{FeatureName}{Name of the response column in \code{DataFrame}.}
  \item{SplitDataAt}{Legacy name for \code{SplitAt}.}
  \item{Frequency}{Legacy name for \code{TimeStep}. The function does not aggregate observations. Aggregate data before the call.}
  \item{ForecastPeriods}{Legacy name for \code{ForecastHorizon}.}
  \item{Holidays}{For Prophet, a holiday data frame accepted by \code{prophet}. For random forest, an optional data frame with a \code{Time} column. Dates must be known at the forecast origin.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
  \item{xlab}{Label for the time axis in the model-specific plot. In the ARIMA wrapper, this legacy argument is accepted for call compatibility.}
  \item{ylab}{Label for the response axis in the model-specific plot. In the ARIMA wrapper, this legacy argument is accepted for call compatibility.}
  \item{EquiDist}{If \code{TRUE}, require regular timestamps. If \code{FALSE}, allow irregular Prophet timestamps. Supply \code{FutureTime} when an interval cannot be inferred.}
  \item{MinLowerBound}{Optional finite lower bound. Set the Prophet floor and clip returned predictions and bounds at this value.}
  \item{MaxUpperBound}{Optional finite upper bound. Set the Prophet capacity and clip returned predictions and bounds at this value.}
  \item{Cycles}{Optional Prophet cycle settings. Logical entries \code{monthly}, \code{quaterly} (legacy spelling), and \code{biyearly} enable cycles of 30.5, 365.25/4, and 2*365.25 days. \code{Orders} and \code{PriorScale} contain three values each. \code{Mirrored = TRUE} adds reflected training history; see Details.}
  \item{Growth}{Prophet growth model, such as \code{"linear"} or \code{"logistic"}. Logistic growth requires capacity values.}
  \item{tz}{Time zone used for calendar conversion where applicable. The default is UTC.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{Predictors}{Optional data frame or matrix of external predictors. Supply one row per training observation or one row per supplied response observation. Values used for prediction must be available at the forecast origin.}
  \item{FuturePredictors}{Optional predictor values for all forecast rows. Supply exactly \code{ForecastHorizon} rows and the same column names as \code{Predictors}. If omitted, use the corresponding rows after \code{SplitAt} in \code{Predictors}.}
  \item{PlotComponents}{If \code{TRUE}, also display the Prophet component plots when \code{PlotIt} is \code{TRUE}.}
  \item{Seed}{Optional nonnegative integer for reproducible random operations. The function restores the R random state after the call. TensorFlow can also depend on the runtime and hardware.}
}
\details{
Additional arguments go to \code{prophet::prophet}. Custom cycle periods use days, not observation counts. Built-in seasonal options retain their Prophet meaning.

\code{Cycles$Mirrored = TRUE} adds synthetic reflected training history. It requires regular Date timestamps and no external predictors. Use this option only when its assumption is appropriate.

Logistic capacity values must cover all forecast dates. Bounds explicitly supplied by the user also clip predictions and intervals. With \code{uncertainty.samples = 0}, interval fields are \code{NULL}.

The specific plot and component plots are preserved. \code{ModelForecast} contains the full prediction table. \code{ggObject} contains the forecast plot. \code{TrainingData} and \code{TrainingSetForecast} retain Prophet tables. Legacy data-frame calls with an omitted horizon forecast the test tail.
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
About the package behind it
\url{https://facebook.github.io/prophet/}

Examplary published approach of an (manual) compund model:

Aubert, A. H., Thrun, M. C., Breuer, L., Ultsch, A.: Knowledge discovery from high-frequency stream nitrate

concentrations: hydrology and biology contributions, Scientific Reports, Nature, Vol. 6(31536), pp. 1-8, 2016.

}
\author{
Michael Thrun for the easy-to-use wrapper, algorithm of prophet is used (see url)
}
\seealso{\code{\link{ForecastingInterface}}}
\examples{
if (requireNamespace("prophet", quietly = TRUE)) {
  y <- as.numeric(Nile)
  time <- seq(as.Date("2000-01-01"), by = "day", length.out = length(y))
  out <- FcDecompositionModelByRegression(DataVec = y, Time = time,
    SplitAt = 80, ForecastHorizon = 3, Seed = 1,
    yearly.seasonality = FALSE, weekly.seasonality = FALSE,
    daily.seasonality = FALSE, uncertainty.samples = 20)
  stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
            all(is.finite(out$Forecast)))
}
}
\keyword{ts}

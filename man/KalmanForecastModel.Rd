\name{KalmanForecastModel}
\alias{KalmanForecastModel}
\title{Kalman forecasts with structural time-series models}
\description{Fit a linear Gaussian structural model and predict future observations with the Kalman filter.}
\usage{
KalmanForecastModel(Data = NULL, Horizont = NULL, \dots, DataVec = NULL,
  Time = NULL, SplitAt = NULL, ForecastHorizon = NULL,
  ModelType = c("trend", "level", "BSM"), TimeStep = NULL,
  SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95), PlotIt = FALSE)
}
\arguments{
  \item{Data}{Legacy response input. Use \code{DataVec} for univariate methods. For VARIMA, supply a numeric matrix with one series per column.}
  \item{Horizont}{Legacy name for \code{ForecastHorizon}. The Kalman default horizon is 12 observations.}
  \item{\dots}{Additional arguments for the model function identified in Details. Do not repeat arguments that the wrapper sets explicitly.}
  \item{DataVec}{Numeric response vector. A univariate \code{ts} object is also accepted. Values after \code{SplitAt} are used only for evaluation.}
  \item{Time}{Ordered timestamps, with one timestamp per observation. Use numeric indices, \code{Date}, or \code{POSIXct}. ISO date strings are accepted. If omitted, use the \code{ts} time index or consecutive observation indices. Prophet requires calendar timestamps.}
  \item{SplitAt}{Index of the last training observation. The forecast starts at the next observation. The usual default uses all supplied observations. See Details for legacy defaults.}
  \item{ForecastHorizon}{Positive integer. Number of observations to predict after \code{SplitAt}. The result always contains this number of forecast rows.}
  \item{ModelType}{Model selection. See Details for the supported values and their assumptions.}
  \item{TimeStep}{Time interval. Use a positive number or a calendar unit: \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, \code{"year"}, \code{"hour"}, \code{"min"}, or \code{"sec"}. Numeric intervals use the units of \code{Time}: days for \code{Date}, seconds for \code{POSIXct}, and index units otherwise. If omitted, infer a regular interval from training timestamps.}
  \item{SeasonalPeriod}{Positive integer. Number of observations in one seasonal cycle. The default uses the frequency of a \code{ts} input, or one for a numeric vector. This setting does not select an input window or aggregate data.}
  \item{FutureTime}{Optional timestamps for all forecast rows. Supply exactly \code{ForecastHorizon} values after the forecast origin. They must agree with any observed test timestamps and with the regular interval required by the method.}
  \item{Level}{Prediction interval coverage, in percent. Each value must be between zero and 100. X-13, MAPA, and Prophet accept one value per call. Other methods can accept several values.}
  \item{PlotIt}{If \code{TRUE}, display the forecast plot. If \code{FALSE}, do not display plots. Plot settings do not change the forecast values.}
}
\details{

\code{ModelType = "level"} selects a local level model. It is suitable when the series has a changing level without an explicit slope or seasonal cycle.
\deqn{y_t = \mu_t + \epsilon_t, \qquad \mu_{t+1} = \mu_t + \eta_t.}

\code{ModelType = "trend"} is the default. It adds a changing slope.
\deqn{y_t = \mu_t + \epsilon_t, \qquad \mu_{t+1} = \mu_t + b_t + \eta_t, \qquad b_{t+1} = b_t + \zeta_t.}

\code{ModelType = "BSM"} adds a seasonal component to the local linear trend model.
\deqn{y_t = \mu_t + s_t + \epsilon_t, \qquad s_{t+1} = -\sum_{j=0}^{m-2}s_{t-j} + \omega_t.}
Here, \eqn{m} is \code{SeasonalPeriod}. BSM requires an integer period of at least two and at least two complete training cycles. For example, use \eqn{m=12} for monthly observations with an annual seasonal cycle.

The disturbance terms are independent, zero-mean Gaussian variables. Each has a time-invariant variance. \code{stats::StructTS} estimates those variances by maximum likelihood on training data only. A fitted variance can be zero. Additional arguments, such as \code{init}, \code{fixed}, and \code{optim.control}, go to \code{StructTS}. A nonzero optimizer convergence code causes an error.

The method requires regular observations and nonconstant finite training data. It does not fit external regressors. It does not select a model type automatically. Select the model with data knowledge and rolling forecast evaluation. Do not compare raw likelihoods across these model types because their diffuse initialization differs.

\code{stats::predict.StructTS} propagates the final filtered state. The forecast mean estimates the future observation. \code{PredictionVariance} includes future observation noise and state uncertainty. Gaussian prediction bounds use that variance. They do not include parameter-estimation uncertainty. They are prediction intervals, not confidence intervals for the latent mean.

\code{Fitted} contains one-step prediction means from the previous filtered state. The first value is missing. \code{Components} contains contemporaneous filtered level, slope, and seasonal estimates, as available. These component estimates use the observation at that time. They are distinct from \code{Fitted}. No smoothed future states enter the forecasts.

\code{Info$Variances} contains the fitted disturbance variances. \code{Model} retains the full StructTS object. The legacy fields \code{pred} and \code{var} contain forecast means and prediction variances. The default horizon is 12.

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
Harvey, A. C. (1989). Forecasting, Structural Time Series Models and the Kalman Filter. Cambridge University Press.

Durbin, J. and Koopman, S. J. (2012). Time Series Analysis by State Space Methods, second edition. Oxford University Press.

R Core Team. \code{\link[stats]{StructTS}}, \code{\link[stats]{predict.StructTS}}, and \code{\link[stats]{KalmanLike}} documentation.
}

\seealso{\code{\link{ForecastingInterface}}}
\examples{
y <- as.numeric(Nile)
out <- KalmanForecastModel(DataVec = y, SplitAt = 80,
  ForecastHorizon = 3, ModelType = "level")

seasonal <- KalmanForecastModel(DataVec = log(AirPassengers),
  SplitAt = 120, ForecastHorizon = 3, ModelType = "BSM")
stopifnot(length(seasonal$Forecast) == 3)
stopifnot(NROW(out$Forecast) == 3, length(out$ForecastTime) == 3,
          all(is.finite(out$Forecast)))
}
\keyword{ts}

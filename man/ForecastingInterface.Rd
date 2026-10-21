\name{ForecastingInterface}
\alias{ForecastingInterface}
\title{Common forecasting input and result rules}
\description{The forecast wrappers use common meanings for their time, split, horizon, and result fields.}
\details{
Use named arguments in new calls. Existing argument positions and documented aliases remain available where their meanings are safe. Result types have changed: \code{Forecast} now contains response predictions. Native prediction objects use separate fields. Code that accessed a native object through \code{Forecast} must use the documented replacement field.

\code{SplitAt = k} uses observations one through k for fitting. \code{ForecastHorizon = h} predicts observations k+1 through k+h. A forecast can cover observed test values and unknown future values. The wrapper returns exactly h rows and marks unknown actual values as \code{NA}.

\code{TimeStep} is the observation interval. \code{SeasonalPeriod} is the number of observations per seasonal cycle. \code{Lookback} is the length of an input history. \code{Lags} identifies selected past observations. These settings are distinct. Calendar aggregation is a separate operation.

A numeric response uses observation indices when \code{Time} is omitted. A \code{ts} input supplies its time index and seasonal frequency. Calendar timestamps retain their class. Monthly Date and POSIXct sequences retain month-end dates or the intended day where it exists. Calendar days preserve local clock time across daylight saving changes. Numeric POSIXct intervals represent elapsed seconds. A custom or irregular calendar requires explicit timestamps.

Most methods require regular training data. Prophet can accept irregular data with \code{EquiDist = FALSE}.

Use training data only to fit the model. Estimate scaling values from training data only. Select model parameters from training data only. Future predictors must be available at the forecast origin. Recursive forecasts use predictions for unavailable response lags. Use \code{\link{RollingForecast}} to evaluate several origins.

Errors have the sign observed minus predicted. MAE and RMSE use response units. Bias is the mean signed error.

MAPE and SMAPE use percent. MAPE is missing if any evaluated actual value is zero. A zero actual and zero forecast contribute zero to SMAPE.

MASE divides MAE by the mean absolute seasonal difference in the training response. MASE is missing when its training scale is zero or unavailable. The calculation does not add random noise.

The accuracy matrix comes from \code{\link{QualityMeasuresForecasting}} and has one row per response series. It includes the common error measures and the additional measures documented by that function. \code{N} counts valid comparisons. With no observed comparisons, \code{Accuracy} is \code{NULL}.

A model failure stops a direct forecast call. Missing optional packages cause a clear error. Rolling evaluation can record failed fits and continue. Unsupported fitted values or prediction intervals are \code{NULL}. Initial fitted values can be \code{NA} when a model needs past observations.

\code{PlotIt = FALSE} suppresses plot display. Plot settings do not change numerical forecasts. The compound model and Prophet keep their specific plotting procedures. Additional model outputs remain available in named fields.

The examples use small data sets and short training settings. They test the interface, not forecast quality. Optional-model examples run only when their required packages are installed. LSTM also requires a working Python TensorFlow runtime. R CMD check executes examples; R CMD build creates the source package.

Use \code{\link{PrepareTS4Forecast}} to convert regular training data to an R time series. Use \code{\link{RecursiveForecast}} with a fitted prediction rule to extend response lags. Use \code{\link{PlotForecast}} to display a saved forecast result.
}

\seealso{\code{\link{KalmanForecastModel}}, \code{\link{FcAutoARIMA}}, \code{\link{RollingForecast}}}
\keyword{ts}

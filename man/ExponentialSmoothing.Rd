\name{ExponentialSmoothing}
\alias{ExponentialSmoothing}
\title{Exponential smoothing forecasts}
\description{Fit an exponential smoothing or structural model to the training observations.}
\usage{
ExponentialSmoothing(Time = NULL, Data = NULL, Type = "StateSpaceModel",
  PlotIt = FALSE, \dots, DataVec = NULL, SplitAt = NULL, ForecastHorizon = 12L,
  TimeStep = NULL, SeasonalPeriod = NULL, FutureTime = NULL, Level = c(80, 95))
}
\arguments{
  \item{Time}{Optional ordered numeric, Date, or POSIXct timestamps.}
  \item{Data}{Legacy numeric response input.}
  \item{Type}{Model: StateSpaceModel, SSOE, exponentialSmoothing, HoltWinters, or StructTS.}
  \item{PlotIt}{If TRUE, display the forecast.}
  \item{\dots}{Arguments for forecast::ets, smooth::es, forecast::ses, stats::HoltWinters, or KalmanForecastModel, according to Type.}
  \item{DataVec}{Numeric response. Values after SplitAt are used only for evaluation.}
  \item{SplitAt}{Last training index. The default uses all supplied data.}
  \item{ForecastHorizon}{Positive number of forecast observations.}
  \item{TimeStep}{Regular numeric interval or calendar unit. See ForecastingInterface.}
  \item{SeasonalPeriod}{Observations per seasonal cycle. Default: ts frequency, or one.}
  \item{FutureTime}{Optional timestamps for all forecast rows.}
  \item{Level}{Prediction coverage percentages between zero and 100.}
}
\details{
StateSpaceModel uses automatic ETS selection. exponentialSmoothing uses simple exponential smoothing. HoltWinters disables seasonality by default when SeasonalPeriod is one. SSOE uses smooth::es with no additional holdout and returns no prediction bounds. StructTS uses the Kalman forecast wrapper. All branches use only the training response. PlotIt does not change fitting or output. See \code{ForecastingInterface}.
}
\value{
Common forecast list described in \code{\link{ForecastingInterface}}. The fitted native model is in Model.
}
\examples{
if (requireNamespace("forecast",quietly=TRUE)) {
  out <- ExponentialSmoothing(DataVec=as.numeric(Nile),SplitAt=80,ForecastHorizon=3)
  stopifnot(length(out$Forecast) == 3, all(is.finite(out$Forecast)))
}
}

\keyword{ts}

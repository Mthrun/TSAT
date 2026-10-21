\name{RollingWindow_MAPA}
\alias{RollingWindow_MAPA}
\title{Evaluate MAPA forecasts at consecutive origins}
\description{Internal adapter for the legacy MAPA evaluation matrices. Use \code{\link{RollingForecast}} in new calls.}
\usage{
RollingWindow_MAPA(Data, Time, Horizon = 14, Window = 365,
  Period = 1, Aggregation = "mean")
}
\arguments{
\item{Data}{Numeric response vector.}
\item{Time}{One ordered timestamp per observation.}
\item{Horizon}{Positive integer. Number of forecast steps per origin.}
\item{Window}{Number of consecutive forecast origins. This legacy argument does not limit the training length.}
\item{Period}{Number of observations per seasonal cycle.}
\item{Aggregation}{MAPA combination operator. The default is \code{"mean"}.}
}
\details{
The training set expands at each origin. The last origin leaves \code{Horizon} observed test values.
Each model receives only the training response. A model failure stops the call.
Use \code{\link{RollingForecast}} with its \code{Window} argument to limit the training length.
}
\value{
A list with \code{Error}, \code{Forecast}, \code{RWData}, and \code{RWTime} matrices.
Rows identify origins. Columns identify forecast steps.
\code{Error} is observed minus predicted.
\code{ForecastOrigin} contains the last training timestamp for each row.
\code{Setting} contains the input data and settings.
}
\author{QMS}
\seealso{\code{\link{FcMAPA}}, \code{\link{RollingForecast}}}
\examples{
if (requireNamespace("MAPA", quietly = TRUE)) {
  out <- TSAT:::RollingWindow_MAPA(as.numeric(Nile), seq_along(Nile),
    Horizon = 3, Window = 2, Period = 1)
  stopifnot(identical(dim(out$Forecast), c(2L, 3L)))
}
}
\keyword{ts}
\keyword{internal}

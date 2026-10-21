\name{IdentifyOutliers}
\alias{IdentifyOutliers}
\title{Identify observations outside robust limits}
\description{Compare each observation with limits based on robust location and scale.}
\usage{
IdentifyOutliers(Datavec, Time, n_sigmas = 3, p = 0.1, lowInnerPercentile = 25,
  PlotIt = FALSE)
}
\arguments{
  \item{Datavec}{Finite numeric response vector.}
  \item{Time}{Optional time vector. The default uses observation indices.}
  \item{n_sigmas}{Number of robust standard deviations for each limit.}
  \item{p}{Tail proportion passed to DataVisualizations::Meanrobust.}
  \item{lowInnerPercentile}{Inner percentile passed to DataVisualizations::Stdrobust.}
  \item{PlotIt}{If TRUE, display the density and time-series plots.}
}
\details{
The function uses DataVisualizations for robust estimation. The limits are the robust mean plus or minus n_sigmas times the robust standard deviation.
}
\value{
Logical vector. TRUE identifies an observation outside the limits.
}
\examples{
if (requireNamespace("DataVisualizations",quietly=TRUE)) {
  out <- IdentifyOutliers(c(1:20,100))
  stopifnot(length(out) == 21, tail(out,1))
}
}

\keyword{ts}

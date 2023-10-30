\name{ParallelTSlinePlots}
\alias{ParallelTSlinePlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots many time series in parallel from a matrix of Data
}
\description{
It is a shortcut wrapper for \code{\link[latticeExtra]{horizonplot}} in order to omit the manual creation of \code{ts} objects
}
\usage{
ParallelTSlinePlots(Data, Time, DailyRegular = TRUE, scales = list(y = "same"), cols, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] d mulitvariate timeseries of n cases
}
  \item{Time}{
Optional, only if time series are regular, if not please do not set
}
  \item{DailyRegular}{
Optional, If TRUE: Time series is regular with \code{as.date} objects, in other case its regular as defined by Time. Default is TRUE
}
  \item{scales}{
Optional, same is best variant. It is preferred to normalize data before using this function. Please see \code{\link[latticeExtra]{horizonplot}} for other options
}
  \item{cols}{
Optional, color scale as character vector. Default is \code{\link[DataVisualizations]{HeatmapColors}} sequence
}
  \item{\dots}{
Optional arguments passed on to \code{\link[latticeExtra]{horizonplot}} 
}
}
\details{
If \code{DailyRegular=FALSE}, then the x axis will be in UNIX seconds since 1970-01-01.
}

\references{
Idea taken from \url{https://machinelearningmastery.com/how-to-load-visualize-and-explore-a-complex-multivariate-multistep-time-series-forecasting-dataset/}

[Moerchen, 2006]  Moerchen, Fabian. Time series knowledge mining. Goerich & Weiershaeuser, 2006.
}
\note{
Beware in reference the authors talks about temporal structures using this visualization in Python. That is incorrect.
For temporal structures please see [Moerchen, 2006].
}
\author{
Michael Thrun
}
\seealso{
\code{\link[latticeExtra]{horizonplot}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ParallelTSlinePlots}% use one of  RShowDoc("KEYWORDS")
\keyword{Parallel Time Series Line Plots}% __ONLY ONE__ keyword per line
\keyword{horizonplot}% __ONLY ONE__ keyword per line
\keyword{Visualization}% __ONLY ONE__ keyword per line
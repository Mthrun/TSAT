\name{InterpolateOutliers}
\alias{InterpolateOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InterpolateOutliers
}
\description{
Interpolates Outliers setting them NaN and then applying spline interpolation
}
\usage{
InterpolateOutliers(Time, Datavector, OutliersTime, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:N] vector of time, code{POSIXlt} or \code{POSIXct} are accepted
}
  \item{Datavector}{
[1:N] vector of values
}
  \item{OutliersTime}{
[1:k] vector of time of outliers
}
  \item{PlotIt}{
plots the results if true
}
}
\details{
assumption is that outliers should be ignore in timeseries analysis
}
\value{
[1:N] interpolated vector of values
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{InterpolateOutliers}% use one of  RShowDoc("KEYWORDS")
\keyword{Outliers}% __ONLY ONE__ keyword per line

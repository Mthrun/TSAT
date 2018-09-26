\name{InterpolateMissingValues}
\alias{InterpolateMissingValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InterpolateMissingValues
}
\description{
Interpolates Missing valies setting them NaN and then applying spline interpolation
}
\usage{
InterpolateMissingValues(Time, Datavector, option = "stine",

PlotIt = TRUE)
}

\arguments{
  \item{Time}{
[1:N] vector of time, code{POSIXlt} or \code{POSIXct} are accepted
}
  \item{Datavector}{
[1:N] vector of values
}
  \item{option}{
see \code{\link[imputeTS]{na.interpolation}}
}
  \item{PlotIt}{
Default: FALSE, TRUE: Evaluates output of function versus input by plots
}
}
\details{
Wrapper for \code{\link{InterpolateOutliers}}. Missing values are all values which are not \code{is.finite(Datavector)==TRUE}.
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
\keyword{MissingValues}% __ONLY ONE__ keyword per line


\seealso{
\code{\link{InterpolateOutliers}}
}
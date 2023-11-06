\name{RelativeDifferenceBetweenTS}
\alias{RelativeDifferenceBetweenTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RelativeDifferenceBetweenTS
}
\description{
RelativeDifference  for Time series
}
\usage{
RelativeDifferenceBetweenTS(TS1, TS2, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS1}{
[1:n] time series data vector
}
  \item{TS2}{
[1:n] time series data vector
}
  \item{na.rm}{
TRUE: interpolate NaN
}
}
\seealso{
\code{\link{TemporalRelativeDifference}}
}
\value{
Relative difference [1:n] between two TS
}

\author{
Michael Thrun
}

\keyword{RelativeDifferenceBetweenTS}% use one of  RShowDoc("KEYWORDS"
\keyword{RelativeDifference}
\concept{relative difference }
\name{LagVector}
\alias{LagVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lag a Vector
}
\description{
Compute a lagged version of a numeric vector, shifting the time base back or forward by a given number of observations.
}
\usage{
LagVector(x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
[1:n] vector
}
  \item{k}{
The number of lags (in units of observations): back shifting: positiv value, forward shifting: negative value
}
}
\details{
for time series please use \code{lag}
}
\value{
Shifted vector [1:n] with k NaN values at the beginning (k positive) or the end (k negative)
}

\author{
Michael Thrun
}

\seealso{
\code{\link{lag}}
}
\examples{
LagVector(1:100,5)
LagVector(1:100,-5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lag}% use one of  RShowDoc("KEYWORDS")
\keyword{LagVector}% __ONLY ONE__ keyword per line

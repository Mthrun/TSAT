\name{DTWdistance}
\alias{DTWdistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DTW Distance
}
\description{
Dynamic Time Warping distance based an Non-Euclidean pairwise distances
}
\usage{
DTWdistance(Data, DistanceFunction = proxy::dist, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] matrix with d Timeseries of the length n
}
  \item{DistanceFunction}{
a function which calculates the pairwise distances in the same was as \code{\link[proxy]{dist}}
}
  \item{\dots}{
Further Arguments for DistanceFunction
}
}
\details{
Currently only implemented for time series of same length. Distances in \code{DistanceFunction} should be calculated the same ways as in a two-dimensional data matrix with the output of the function beeing object of class \code{\link{dist}}.
}
\value{
[1:d,1:d] symmetric matrix of distances
}
\references{
Toni Giorgino. Computing and Visualizing Dynamic Time Warping Alignments in R: The dtw Package. Journal of Statistical Software, 31(7), 1-24. http://www.jstatsoft.org/v31/i07/ 
}
\author{
Michael Thrun
}


\seealso{
\code{\link[dtw]{dtw}}
}
\examples{
\donttest{
dd=DTWdistance(Data,DistanceFunction = proxy::dist,method='Manhattan')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dtw}% use one of  RShowDoc("KEYWORDS")
\keyword{distance}% __ONLY ONE__ keyword per line

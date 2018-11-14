\name{ConvertNumerical2TSobject}
\alias{ConvertNumerical2TSobject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a Numerical to a Time Series Object
}
\description{
Converts a numerical vector or matrix to a time series object of the stats package
}
\usage{
ConvertNumerical2TSobject(NumericVectorOrMatrix, Time, Frequency = "days")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NumericVectorOrMatrix}{
[1:n,1:d] matrix or vector with n cases and d variables
}
  \item{Time}{
[1:n] Time object of class \code{Date}
}
  \item{Frequency}{
either \code{days}, \code{weeks}, \code{months} or \code{quarters} or \code{years}
}
}
\details{
shortcuts because several analytic methods require \code{\link{ts}} objects
}
\value{
\code{\link{ts}} objects with [1:n,1:d]
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
}
\author{
Michael Thrun
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{ts}} 
}
\examples{
tsobject=ConvertNumerical2TSobject(WeeklySalesUniformRandom$Sales,WeeklySalesUniformRandom$Time,"weeks")
\donttest{
plot(tsobject)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ConvertNumerical2TSobject}% use one of  RShowDoc("KEYWORDS")
\keyword{ts}% __ONLY ONE__ keyword per line
\keyword{time series}
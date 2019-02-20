\name{DiffFilter}
\alias{DiffFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Difference Filter of 1. Order
}
\description{
Detrending through the computation of differences. 
}
\usage{
DiffFilter(x,BackTransformation=FALSE)
}
\arguments{
  \item{x}{
[1:n] numerical vector
}
  \item{BackTransformation}{
Transforms the difference between values back to the original state given the first element as initial value, see example
}
}

\value{
dx(1:n) = x(i)-x(lag(1)), mit  x(-1) = x(1)
}
\details{
Trend correction is often proposed in order to avoid false regressions. The first value of differences is the first value of the numerical vector x in order to enable a transformation back, see example.
}
\author{
Michael Thrun
}
\references{
temporal data mining, Prof. Dr. Ultsch
}

\examples{
DiffFilter(c(4,5,2.87,5.2))
}
\keyword{Difference Filter}% use one of  RShowDoc("KEYWORDS")

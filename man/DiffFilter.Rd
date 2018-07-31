\name{DiffFilter}
\alias{DiffFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Difference Filter of 1. order
}
\description{
Detrending through the computation of differences. 
}
\usage{
DiffFilter(x)
}
\arguments{
  \item{x}{
[1:n] numerical vector
}
}

\value{
dx(1:n) = x(i)-x(lag(1)), mit  x(-1) = x(1)
}
\details{
Trend correction is often proposed in order to avoid false regressions. 
}
\author{
Michael Thrun
}
\references{
temporal data mining, Prof. Dr. Ultsch
}

\keyword{Difference Filter}% use one of  RShowDoc("KEYWORDS")

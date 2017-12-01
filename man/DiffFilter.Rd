\name{DiffFilter}
\alias{DiffFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Difference Filter
}
\description{
Difference Filter  1. Ord.
}
\usage{
DiffFilter(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
timeseries values [1:n]
}
}

\value{
dx(1:n) = x(i)-x(lag(1)), mit  x(-1) = x(1)
}

\author{
Michael Thrun
}
\references{
temporal data mining, Prof. Dr. Ultsch
}

\keyword{Difference Filter}% use one of  RShowDoc("KEYWORDS")

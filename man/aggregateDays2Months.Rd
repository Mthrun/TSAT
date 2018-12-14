\name{aggregateDays2Months}
\alias{aggregateDays2Months}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregate Days to Months
}
\description{
Aggregation of many Days over several years to Months is possible with this function
}
\usage{
aggregateDays2Months(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] \code{POSIXlt} or \code{POSIXct}
}
 \item{Data}{
[1:n,1:d] matrix or dataframe, d can be also 1 , then vector
}
  \item{FUN}{
aggegate by a function like \code{sum} or \code{mean}
}
  \item{Header}{
colnames for data
}
}
\details{
Aggregation is not testet for d>1
}
\value{
dataframe[1:m,1:(d+1)] with m<n and first column beeing the time in \code{as.Date} format
}
\author{
Michael Thrun
}

\keyword{aggregateDays2Weeks}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

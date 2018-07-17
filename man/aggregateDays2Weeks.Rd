\name{aggregateDays2Weeks}
\alias{aggregateDays2Weeks}

\title{
Aggregate Days to Weeks
}
\description{
Aggregation of many Days over several years to Weeks is possible with this function
}
\usage{
aggregateDays2Weeks(Time, Datavector, FUN)
}

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
}
\details{
A week is defined here by the ISO 8601 system, which uses a reoccurring leap week. Aggegation works even if several different years are used.
}
\value{
dataframe[1:m,1:(d+1)] with m<n and first column beeing the time in \code{as.Date} format
}

\author{
Michael Thrun
}

\keyword{aggregateDays2Weeks}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

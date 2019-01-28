\name{aggregateDays2Weeks}
\alias{aggregateDays2Weeks}

\title{
Aggregate Days to Weeks
}
\description{
An either univariate or multivariate time series consisting of \code{Time} of many Days over several years, and \code{Data} is aggregated to weeks.
}
\usage{
aggregateDays2Weeks(Time, Data, FUN, Header,\dots)
}

\arguments{
  \item{Time}{
[1:n] vector of \code{POSIXlt} or \code{POSIXct} or \code{as.Date} objects
}
 \item{Data}{
[1:n,1:d] matrix or dataframe, d can be also 1, then vector
}
  \item{FUN}{
aggegate by a function like \code{sum} or \code{mean}
}
  \item{Header}{
colnames for data
}
  \item{\dots}{
further arguments passed on to \code{FUN}
}
}
\details{
A week is defined here by the ISO 8601 system, which uses a reoccurring leap week. 
Especially this means that every week starts with mondays and all values from monday to sunday are aggreagted to monday. In other words, a time stamp returned by this function has all values aggregated starting from the time stamp until the 7th day (a sunday) in advance.
If the timeseres does not begin with a monday, all values before monday are aggregated to the monday before the first monday of the time series. The internal functions of R doing this aggregation assume a regular daily time series. If this is not the case, a warning is given back and a regular time series is generated using \code{\link{GenerateRegularDailyTS}} with the Parameter \code{na.rm='zero'}. Depending on the aggregation function \code{FUN} it could result in an incorrect aggregation.

Aggegation works even if several different years are used.
}
\value{
Dataframe[1:m,1:(d+1)] with m<n, where m are the number of wees, and the first column beeing the time in \code{as.Date} format.
}

\author{
Michael Thrun
}
\note{
If the error "no rows to aggregate" arrises, it may be beause the data contains nans. in such a case add
\code{na.rm=TRUE} and \code{na.action=na.pass} to the function input parameters.
}
\keyword{aggregateDays2Weeks}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

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
[1:n], a date-time object. Must be a POSIXct, POSIXlt, Date, chron, yearmon, yearqtr, zoo, zooreg, timeDate, xts, its, ti, jul, timeSeries, or fts object.
}
  \item{Datavector}{
numerical vector [1:n]
}
  \item{FUN}{
aggegate by a function like \code{sum} or \code{mean}
}
}
\details{
A week is defined here by the ISO 8601 system, which uses a reoccurring leap week. Aggegation works even if several different years are used.
}
\value{
A list with
\item{WeeklyTime}{\code{as.Date} object [1:m], m<n}
\item{WeeklyData}{[1:m], numerical vector, m<n}

}

\author{
Michael Thrun
}

\keyword{aggregateDays2Weeks}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

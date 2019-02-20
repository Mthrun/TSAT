\name{aggregateDays2FiskalMonths}
\alias{aggregateDays2FiskalMonths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregate Days to Fiskal Months
}
\description{
Aggregation of many Days over several years to fiskal months is possible.
A fiskal month is defined by the following. Every first month in a quarter has five weeks the following two months have
exactly four weeks. The first month in the year starts with a monday (and every following month).
A month has only full weeks included in it.
}
\usage{
aggregateDays2FiskalMonths(Time,Data,FUN,Header, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector of \code{POSIXlt} or \code{POSIXct} or \code{as.Date} obects
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
The rownames of the data frame depict either the first week (usually) of the month the data is aggregated by  or, in the case of the first time interval until a full month of data exists, the last week where data was existed (see example).
The fiskal month is often used to compare weekly and monthly forecasts consistently.
}
\value{
dataframe[1:m,1:(d+1)] with m<n and first column beeing the time in \code{as.Date} format
}
\author{
Michael Thrun
}
\examples{
#Generating Data
data(Sales)
Time=seq(from=as.Date('1970-01-01'),by='1 day',length.out = 2*length(Sales))
#Aggregation
Monthly=aggregateDays2FiskalMonths(Time,c(Sales,Sales),sum)
str(Monthly)
}

\keyword{aggregateDays2FiskalMonths}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

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
aggregateDays2Months(Time,Data,FUN,Header,Period="month", \dots)
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
  \item{Period}{
string defining the period, but only \code{"month"} was tested
}
  \item{\dots}{
further arguments passed on to \code{FUN}
}
}
\details{
A shortcut for aggregation using \pkg{dplyr}
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
Monthly=aggregateDays2Months(Time,c(Sales,Sales),sum)
str(Monthly)
}

\keyword{aggregateDays2Months}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

\name{FilterWeekdays}
\alias{FilterWeekdays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filter Weekdays
}
\description{
Filter Days of Week Out of Daily time series 
}
\usage{
FilterWeekdays(Time, Datavector=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] Time object, either code{POSIXlt} or \code{POSIXct} are required with daily frequency
}
  \item{Datavector}{
 Optional, default \code{NULL}, [1:n] either Key in case of a dataframe or vector of data
}
}
\details{
Days of Week: Monday to Friday
Saturday and Sunday are deleted
}
\value{
List with
\item{FilteredTime}{[1:m], m<=n, Time object}
\item{FilteredData}{[1:m], m<=n, Filtered Datavector}
}

\author{
Michael Thrun
}

\keyword{FilterWeekdays}% use one of  RShowDoc("KEYWORDS")


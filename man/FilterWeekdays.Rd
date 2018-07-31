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
FilterWeekdays(Time, Datavector=NULL,EnglishOrGerman=FALSE,PlotIt=FALSE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] Time object, either code{POSIXlt} or \code{POSIXct} are required with daily frequency
}
  \item{Datavector}{
 Optional, default \code{NULL}, [1:n] either Key in case of a dataframe or vector of data
}
  \item{EnglishOrGerman}{
Default: FALSE: German names of weekdays, English names of weekday, see \code{\link{weekdays}}
}
  \item{PlotIt}{
Default: FALSE, TRUE: Evaluates output of function versus input by plots, works only if Datavector given
}
}
\details{
Days of Week: Monday to Friday are deleted
Saturday and Sunday is given back.
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


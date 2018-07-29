\name{FilterHolidays}
\alias{FilterHolidays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FilterHolidays
}
\description{
Filters Holidays out of daily data
}
\usage{
FilterHolidays(Time, Datavector=NULL, HolidaysTime = TSAT::GermanHolidays$Time,PlotIt=FALSE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:N] vector of time, code{as.Date} objectets or \code{POSIXct} are accepted
}
  \item{Datavector}{
 Optional, default \code{NULL}, [1:N] vector of values or a Key in case of a data frame
}
  \item{HolidaysTime}{
Optional, default: German Holidays, [1:N] vector of time, code{as.Date} objectets or \code{POSIXct} are accepted
}
  \item{PlotIt}{
Default: FALSE, TRUE: Evaluates output of function versus input by plots, works only if Datavector given
}
}
\details{
deletes holidays out of input 
}
\value{
List with
\item{FilteredTime}{[1:m], m<=n, n=m+k, Time object}
\item{FilteredData}{[1:m], m<=n, n=m+k Filtered Datavector}
\item{HolidaysTimes}{[1:k], k<=n, n=m+k Time object of filtered Holidays}
\item{HolidaysData}{[1:k], k<=n, n=m+k Datavector of filtered Holidays}
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FilterHolidays}% use one of  RShowDoc("KEYWORDS")
\keyword{holidays}% __ONLY ONE__ keyword per line

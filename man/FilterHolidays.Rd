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
FilterHolidays(Time, Datavector=NULL,HolidaysTime = TSAT::GermanHolidays$Time,

PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector of time, \code{as.Date} or \code{POSIXct} objects are accepted
}
  \item{Datavector}{
 Optional, [1:n] vector of values or a Key in case of a data frame. Default is \code{NULL}
}
  \item{HolidaysTime}{
[1:m] vector of time of holidays, as.Date or POSIXct objects are accepted. Default are german holidays, see \link[GermanHolidays]{GermanHolidays}
}
  \item{PlotIt}{
Optional, TRUE: Evaluates output of function versus input by plots, works only if Datavector given. Default is FALSE
}
}
\details{
deletes holidays out of input 
}
\value{
List with
\item{FilteredTime}{[1:m] vector with time objects of the m=n-k dates, which are not holidays}
\item{FilteredData}{[1:m] vector with data corresponding to the m=n-k dates, which are not holidays}
\item{HolidaysTimes}{[1:k] vector with time objects of the k holidays}
\item{HolidaysData}{[1:k] vector with data corresponding to the k holidays}
where k,m<=n and n=m+k
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FilterHolidays}% use one of  RShowDoc("KEYWORDS")
\keyword{holidays}% __ONLY ONE__ keyword per line

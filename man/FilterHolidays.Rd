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
FilterHolidays(Time, Datavector=NULL, HolidaysTime = TSAT::GermanHolidays$Time)
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
}
\details{
deletes holidays out of input 
}
\value{
List with
\item{FilteredTime}{[1:m], m<=n, Time object}
\item{FilteredData}{[1:m], m<=n, Filtered Datavector}
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FilterHolidays}% use one of  RShowDoc("KEYWORDS")
\keyword{holidays}% __ONLY ONE__ keyword per line

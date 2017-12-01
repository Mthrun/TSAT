\name{EventCountsPerTime}
\alias{EventCountsPerTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Event Counts per Time
}
\description{
Counts the Events defined by the beginning (middle or end) time of the event in a resolution window of time
}
\usage{
EventCountsPerTime(BeginningTime,Resolution='months',

PlotIt=TRUE,format=c('y-m-d','h:m:s'),split=" ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BeginningTime}{
  [1:n] character of time from years to seconds will be translated to chon object or chron object or chron object
}
  \item{Resolution}{string Option: seconds, minutes, days,hours,weekdays,months,quarters or years
}
  \item{PlotIt}{default(True) plots histogram
}
  \item{format}{format of Time as vector of years to days and hours to seconds
}
  \item{split}{ character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting.
}

}

\value{
[1:n,2] Time of Resultation ind columns 1 and Counts in Columns 2
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{event}% use one of  RShowDoc("KEYWORDS")
\keyword{Counts}% __ONLY ONE__ keyword per line
\keyword{time}
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}
\keyword{time series}
\keyword{categorial time series}

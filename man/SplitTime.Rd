\name{SplitTime}
\alias{SplitTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits Time regarding a given Resolution
}
\description{
Splits Time regarding a given Resolution
}
\usage{
SplitTime(Time,Resolution='years',format=c('y-m-d','h:m:s'),split=" ")
}

\arguments{
  \item{Time}{
  [1:n] character of time from years to seconds will be translated to chon object or chron object or chron object
}
  \item{Resolution}{string Option: seconds, minutes, days,hours,weekdays,months,quarters or years
}
  \item{format}{format of Time as vector of years to days and hours to seconds
}
  \item{split}{ character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting.
}

}
\value{
list[1:k] splittet time
}

\author{
Michael Thrun
}


\keyword{chron}% use one of  RShowDoc("KEYWORDS")
\keyword{years}% __ONLY ONE__ keyword per line
\keyword{split time}% __ONLY ONE__ keyword per line
\keyword{split}% __ONLY ONE__ keyword per line
\keyword{time}% __ONLY ONE__ keyword per line
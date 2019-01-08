\name{GetFullWeeksPerMonth}
\alias{GetFullWeeksPerMonth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetFullWeeksPerMonth
}
\description{
GetFullWeeksPerMonth
}
\usage{
GetFullWeeksPerMonth(Time, DaysOrWeeks = TRUE, IgnoreWeekend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
\code{as.Date} object
}
  \item{DaysOrWeeks}{
TRUE: daily data
FALSE: weekly data is assumed
}
  \item{IgnoreWeekend}{
TRUE: Full week is if even saturday and sunday lies in month
FALSE: saturday and sunday do not need to lie in the month
}
}
\details{
none
}
\value{
values of month 1-12, zero if week does not lie fully in one month
}

\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GetFullWeeksPerMonth}% use one of  RShowDoc("KEYWORDS")

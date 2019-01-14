\name{GetWorkingDays}
\alias{GetWorkingDays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetWorkingDays
}
\description{
Gets Working Days
}
\usage{
GetWorkingDays(Time, HolidaysTime,	GermanBridgeDay=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector of \code{as.Date} objects
}
  \item{HolidaysTime}{
You may put you own holiday definiton as a vector of \code{as.Date} objects, or nothing than german holidays are used
}
  \item{GermanBridgeDay}{TRUE: includes german bridge days of TSAT::GermanHolidays object,   only valid if HolidaysTime is missing 
}
}
\details{
in German-speaking countries: day between a weekend and a statutory holiday which many employees like to take off to make their 'weekend' last four instead of two days.

}
\value{
[1:n,1:4] Dataframe with
Year, Month, WorkinDays and Time as columns 
}

\author{
Michael Thrun
}
\note{
\code{HolidaysTime=as.Date('1970-01-01')} will calculate working days without holidays
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GermanHolidays}}
}
\examples{
#ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GetWorkingDays}% use one of  RShowDoc("KEYWORDS")
\keyword{WorkingDays}% __ONLY ONE__ keyword per line

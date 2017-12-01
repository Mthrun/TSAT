\name{EventDurationAndTimeDifference}
\alias{EventDurationAndTimeDifference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Duration of Events and Time Difference between Events
}
\description{
High level function that calculates time difference between events (if more than one) and duration of events, as well as the Counts of events.
}
\usage{
EventDurationAndTimeDifference(Time,EventNameorValue,EventArrayOrEvent,

units='mins',PeriodBetweenEvents=0,formating="(\%y-\%m-\%d \%H:\%M:\%S)"

,Timezone='UTC',Silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{[1:n]
		Time, a chron object or a POSIXlt object or a character vector,  Devicespezifische Zeit, 
		if not chron object, please adjust format neceassry
		It hast to be convertible to character
}
  \item{EventNameorValue}{character or value to search for
}
  \item{EventArrayOrEvent}{[1:n,1:d] Array (character or numericalö matrix with d>1) or vector (d=1) of string or Values where the event has to be searched in, list cannot be interpreted, dataframes are converted to matrices.
  It is assumed that the time row defines the time of the corresponding row of \code{EventArrayOrEvent}
}

  \item{formating}{default="(\%y-\%m-\%d \%H:\%M:\%S)", else check \code{strptime} function
}
  \item{units}{ character string of units of time for output: "secs", "mins", "hours", "days"
}
  \item{PeriodBetweenEvents}{numerical value in time units specified in \code{units}, 
  time betweed two events which is assumed that it interrupts 
  one event into several events (e.g. due to data quality) but is in reality only one event.
  default =0, in that case the approximation of events by filtering is turned of
}
  \item{Timezone}{
  sometimes durations and time difference cannot be calculated if timezone is not chosen,
                         default: 'UTC'
  }
    \item{Silent}{default (FALSE), if TRUE, error warnings regarding events disabled
}
}
\details{
sometimes there issues in the quality of data, than its usefull to define a period of time between two events, where one assumes that both events reprent only one event, in that case chose for \code{PeriodBetweenEvents} a value above zero 
}
\value{
list with following values

  \item{Time}{[1:n]                 Time as posixCCT
}
  \item{Counts}{value k of events found, zero if no event found
}
  \item{Duration}{[1:k]             Duration of k Events found, NA if no event found and NaN if Event does not end or not computable
  (EventArrayOrEvent[n,d]=EventNameorValue)
}

  \item{Difference}{[1:k-1]       Diference of Time between k events found, NA if only one event found and NaN if not computable
}
  \item{BeginIndsFound}{[1:k]         Beginning-Indizes for Beginning of k Events found (not column depended), NULL if no event found
}
  \item{EndOfEventsInds}{[1:k]        Ending- Indizes for Beginning of k Events found (not column depended), NULL if no event found, NaN if Event has no end in the given \code{Time}
}

}


\author{
Michael Thrun
}
\note{
for details on \code{formating} and \code{Timezone} check the \code{\link{strptime}} function.
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{event}% use one of  RShowDoc("KEYWORDS")
\keyword{duration}% __ONLY ONE__ keyword per line
\keyword{time difference}% __ONLY ONE__ keyword per line
\keyword{time distance}% __ONLY ONE__ keyword per line
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}
\keyword{time series}
\keyword{categorial time series}
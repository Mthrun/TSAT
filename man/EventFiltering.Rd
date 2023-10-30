\name{EventFiltering}
\alias{EventFiltering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Event Filtering
}
\description{
Filters Events in Data
}
\usage{
EventFiltering(EventNameorValue,EventArrayOrEvent,Silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{EventNameorValue}{
  Character or value to search for
}
\item{EventArrayOrEvent}{
  [1:n,1:d] Array (character or numerical matrix) or vector of string or values where the event has to be searched in, list cannot be interpreted, dataframes are converted to matrices
}
\item{Silent}{
  If TRUE, error warnings regarding events disabled. Default FALSE
}
}

\value{
[1:n] Numerical Vector with ones and zeros, ones indicate found events
}

\examples{
data(YearlyInterestRatesData)

# Filter all events, with value 22.000
res = EventFiltering(22.000, as.matrix(YearlyInterestRates))
# Indices with one values are indicative of found events in this entry/row of event vector/matrix
print(res)

# Data filtered for events
filteredEvents = YearlyInterestRates[which(res == 1),]
}

\author{
Michael Thrun
}

\keyword{Event}% use one of  RShowDoc("KEYWORDS")
\keyword{Filtering}% __ONLY ONE__ keyword per line
\keyword{Event Filtering}% __ONLY ONE__ keyword per line
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}
\keyword{time series}
\keyword{categorial time series}

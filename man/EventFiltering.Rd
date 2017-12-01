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
  \item{EventNameorValue}{character or value to search for
}
  \item{EventArrayOrEvent}{Array (character or numericalö matrix) or vector of string or Values where the event has to be searched in, list cannot be interpreted, dataframes are converted to matrices.
}
    \item{Silent}{default (FALSE), if TRUE, error warnings regarding events disabled
}
}

\value{
Feature[1:n]          Numerical Vector with ones and zeros, ones indivate found events
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

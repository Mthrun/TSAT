\name{EventCounts}
\alias{EventCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Counts Events of Vector
}
\description{
Counts Events of character or numerical Vector
}
\usage{
EventCounts(Events,PlotIt=FALSE,main="Histogram of Events",xlab='Event Names')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Events}{
[1:n] vector of events to be counted, character or numerical
}
  \item{PlotIt}{
defaukt: FALSE, if TRUE: Plots histogram
}
  \item{main}{
title of plot
}
  \item{xlab}{
xlabel of plot
}
}

\value{
[1:k,1:3] k breaks of (k different events) with counts as second column and event names as third column 
}

\author{
Michael Thrun
}

\keyword{event}% use one of  RShowDoc("KEYWORDS")
\keyword{Counts}% __ONLY ONE__ keyword per line
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}


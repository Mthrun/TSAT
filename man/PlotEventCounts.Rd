\name{PlotEventCounts}
\alias{PlotEventCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Event versus Counts
}
\description{
Plots a histogram with the number of counts of given events.
}
\usage{
PlotEventCounts(Counts,Names=NULL,main='Event Counts',

xlab='Events',  ylab='Frequencies',order=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Counts}{[1:n]
}
  \item{Names}{If given, [1:n]
}
  \item{main}{string for title of plot
}
  \item{xlab}{string for xlabel
}
  \item{ylab}{string for ylabel
}
  \item{order}{default FALSE, If TRUE ordered by by counts, decreasing
}
}

\author{
Michael Thrun
}
\keyword{event}% use one of  RShowDoc("KEYWORDS")
\keyword{Counts}% __ONLY ONE__ keyword per line
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}
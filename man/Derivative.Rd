\name{Derivative}
\alias{Derivative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Derivative
}
\description{
Derivative
}
\usage{
Derivative(Data,FilterWindowSize,PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{timeseries values
}
  \item{FilterWindowSize}{
  TE wird moving Average gefiltert mit dieser Fensterweite, default ==5 laenge des Moving average filters
  
}
  \item{PlotIt}{ default (FALSE), if TRUE, comparison plot is generated
}
}
\details{
...
}
\value{
empirical 1st derivative with # moving window filter
}
\references{
temporal data mining, Prof. Dr. Ultsch
}
\author{
Michael Thrun
}
\note{
Because of the way endpoints are handled, the result of derivate (using a matlab smooth approach) differs from the result returned by the signal:filter function.
}
\keyword{Derivative}% use one of  RShowDoc("KEYWORDS")

\name{MovingAverage}
\alias{MovingAverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MovingAverage
}
\description{
Moving Average -  A lowpass filter
}
\usage{
MovingAverage(Data, lag,PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{[1:n,1]              time series, in colunns
}
  \item{lag}{ nr or previous data points to average
}
  \item{PlotIt}{ default (FALSE), if TRUE, comparison plot is generated
}
}

\value{
FilteredData[1:n,1]   the filtered data
}

\author{
Michael Thrun
}
\note{
Because of the way endpoints are handled, the result of signal::filter differs from the result returned by the smooth function.
}

\examples{
data("ElectricityBRD")
averagedVals = MovingAverage(ElectricityBRD$Mrd_KWh, 12, PlotIt=TRUE)
}

\keyword{MA}% use one of  RShowDoc("KEYWORDS")
\keyword{Moving Average}% __ONLY ONE__ keyword per line

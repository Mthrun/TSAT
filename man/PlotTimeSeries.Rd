\name{PlotTimeSeries}
\alias{PlotTimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotTimeSeries
}
\description{
Plots time series as a ploty object for interactive purposes like zooming or investigating specific points of time with mouse
}
\usage{
PlotTimeSeries(X, Y, xlab = "X", ylab = "Y",

main = "Timeseries by Plotly", col = "black", SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{[1:n] vector, both lines require the same xvalues, e.g. the time of the time series, \code{POSIXlt} or \code{POSIXct} are accepted}
  \item{Y}{
[1:n] vector of first line
}
  \item{xlab}{
Optional, string for xlabel. Default is "X"
}
  \item{ylab}{
Optional, string for ylabel. Default is "Y"
}
  \item{main}{
Optional, title of plot. Default is "Timeseries by Plotly"
}
  \item{col}{
Optional, color of two lines. Default is "black"
}
  \item{SaveIt}{
Optional, TRUE if you want to save plot as html in \code{getwd()} directory. Default is FALSE
}

}
\details{
Enables to visualize the time series interactively, using plotly

}
\value{
\code{plotly} object
}

\author{
Michael Thrun
}
\note{
Wrapper in order to unify Input and Output
}

\examples{
data(ElectricityBRD)
PlotTimeSeries(ElectricityBRD$Mrd_KWh, ElectricityBRD$Time, "Mrd KWh", "Time", "Electricity production in BRD")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PlotTimeSeries}% use one of  RShowDoc("KEYWORDS")
\keyword{plot.ts}% __ONLY ONE__ keyword per line
\keyword{Visualization}

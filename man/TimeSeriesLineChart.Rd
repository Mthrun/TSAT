\name{TimeSeriesLineChart}
\alias{TimeSeriesLineChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time Series Line Chart
}
\description{
A plotly line chart to visualize a time series
}
\usage{
TimeSeriesLineChart(X,Y,xlab='Time',

y1lab='Values of Time Series',y2lab='Approximated Curve',

main='Time Series',cols=c('black','red'),

SaveIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{[1:n] vector, both lines require the same xvalues, e.g. the time of the time series, \code{POSIXlt} or \code{POSIXct} or code{as.Date} are accepted}
  \item{Y}{
[1:n] vector of histroical time series, usually test data
}
  \item{xlab}{
Optional, string for xlabel
}
  \item{y1lab}{
Optional, string for first ylabel
}
  \item{y2lab}{
Optional, string for second ylabel
}
  \item{main}{
Optional, title of plot
}
  \item{cols}{
Optional, color of two lines
}
  \item{SaveIt}{
Optional, default FALSE; TRUE if you want to save plot as html in \code{getwd()} directory
}
}
\details{
enables to visualize to lines and points in one plot overlaying them using ploty

}
\value{
\code{plotly} object
}
\author{
Michael Thrun
}
\seealso{
Adjusted version of \code{\link[DataVisualizations]{DualAxisLineChart}}
}
\examples{
#ToDo
}
\keyword{AxisLineChart}% use one of  RShowDoc("KEYWORDS")
\keyword{Line Chart}% __ONLY ONE__ keyword per line
\keyword{Axis Line Chart}
\keyword{Time Series Line Chart}
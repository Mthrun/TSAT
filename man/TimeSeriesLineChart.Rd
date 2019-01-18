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
TimeSeriesLineChart(Time,Values,Resolution='AsIs',xlab='Time',

y1lab='Values of Time Series',y2lab='Approximated Curve',

main='Time Series',cols=c('black','red'),

LegendPos='topright',Save=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{[1:n] vector, both lines require the same xvalues, e.g. the time of the time series, \code{POSIXlt} or \code{POSIXct} or code{as.Date} are accepted}
  \item{Values}{
[1:n] vector of histroical time series, usually test data
}
  \item{xlab}{
Optional, string for xlabel
}
  \item{Resolution}{
either "AsIs" - nothing changes
"Weekly"
or
"Monthly"
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
  \item{LegendPos}{
Position of Legend in Plot, Either 'topleft', 'topright', bottomleft or 'bottomright'
}

  \item{Save}{
Optional, default FALSE; TRUE if you want to save plot as html in \code{getwd()} directory
}
  \item{\dots}{
  if
"Weekly"
or
"Monthly"
selected, further aggregation arguments for \code{\link{aggregateDays2Weeks}}
or  \code{\link{aggregateDays2Months}}, \{FUN} has to be set, e.g. FUN=sum
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
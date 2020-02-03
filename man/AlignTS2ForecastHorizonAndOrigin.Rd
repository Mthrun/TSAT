\name{AlignTS2ForecastHorizonAndOrigin}
\alias{AlignTS2ForecastHorizonAndOrigin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Align Time Series to Forecast Horizon And Origin
}
\description{
Extends or shortens time series w.t.r to Forecasting
}
\usage{
AlignTS2ForecastHorizonAndOrigin(Time, Resolution = "weeks", Origin, Horizon, DataVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector of time relevant for seq, usually an as.Date object
}
  \item{Resolution}{
 "weeks", "months", "days" or others accepted strings by seq
 or "FiskalWeekStartOfMonth"
}
  \item{Origin}{
Forecastorigin as Indext w.t.r to Time; Origin=0 means that from the given Time a "future" is generated
}
  \item{Horizon}{
positive number, Forecasthorizon as number of steps to be forecasted
}
  \item{DataVector}{
Optional, [1:n] vector of data ordered the same way as time
}
}
\details{
assumes regular time series with ordered time steps.

Origin=0 means that a new timestamps in the "future" are generated starting from max(Time)+1

FiskalWeekStartOfMonth: Means that in a weekly 5,4,4 pattern timestamps of months are generating mapping each start of month to the first fiskal week in that month. It assumes that this pattern is already given in Time and that each week starts on monday with the first week of the year starting on the first monday of that year. Hence, Forecasthorizon is chosen in months.
}
\value{
LIST of
 \item{Time }{with[1:(Origin+Horizon)]}
 \item{DataVector }{with [1:(Origin+Horizon)]}
}

\author{
Michael Thrun
}



\seealso{
\code{\link{seq}}
}
\examples{
#todo
}

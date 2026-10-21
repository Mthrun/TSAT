\name{PrepareTS4Forecast}
\alias{PrepareTS4Forecast}
\title{Prepare an R Time Series for Forecasting}
\description{Convert a regular training response to a \code{ts} object.}
\usage{PrepareTS4Forecast(Data, Time = NULL, SeasonalPeriod = NULL)}
\arguments{
\item{Data}{A finite numeric vector or matrix. Matrix columns are response series.}
\item{Time}{One ordered, unique timestamp per row. Supply numeric, Date, POSIXct,
POSIXlt, or ISO date strings. The timestamps must form a regular sequence.
The default uses the time index of a \code{ts} input, or observation indices.}
\item{SeasonalPeriod}{The number of observations per cycle. Supply a positive
integer. The default uses the frequency of a \code{ts} input, or 1.}
}
\details{
This function converts data. It does not fit a model, split the data, or make
forecasts. Supply the training observations only.

For calendar input, monthly and quarterly series start in the calendar year
and period of the first observation. Other seasonal periods start at the
calendar year. For numeric input, the first timestamp sets the start.
The \code{ts} index uses \code{SeasonalPeriod} as its frequency. It cannot
retain Date or POSIXt timestamps. Keep the original calendar separately.
}
\value{A \code{ts} object, or an \code{mts} object for multiple response columns.}
\seealso{\code{\link{FcAutoARIMA}}, \code{\link{ForecastingInterface}}}
\examples{
dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
training <- PrepareTS4Forecast(100 + seq_len(24), dates, SeasonalPeriod = 12)
stopifnot(frequency(training) == 12, all(start(training) == c(2020, 1)))
}

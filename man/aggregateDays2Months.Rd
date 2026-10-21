\name{aggregateDays2Months}
\alias{aggregateDays2Months}
\title{Aggregate time-series observations}
\description{Group observations by time and apply a summary function.}
\usage{
aggregateDays2Months(Time, Data, FUN, Header, Period = "month", tz = "UTC",      \dots) 
}
\arguments{
  \item{Time}{One timestamp per data row. Weekly fiscal input must use Monday Date values.}
  \item{Data}{Numeric vector, matrix, or data frame.}
  \item{FUN}{Function that returns one numeric value per group and column.}
  \item{Header}{Optional column names. Supply one name per data column, or names for all output columns. A single name is repeated with unique suffixes for matrix input.}
  \item{Period}{Calendar grouping unit, such as month, quarter, or year.}
  \item{tz}{Time zone used when converting timestamps to dates.}
  \item{\dots}{Additional arguments for FUN, such as na.rm=TRUE.}
}
\details{
Apply the same grouping rule to every data column. Weekly groups start on Monday. Missing observations are passed to FUN. No missing period is filled with zero.
}
\value{
Data frame. The first column gives group dates. Other columns contain the aggregated data.
}
\examples{
time <- as.Date("2020-01-01")+0:99
out <- aggregateDays2Months(time,rep(1,length(time)),sum)
stopifnot(sum(out$Data) == length(time))
}

\keyword{ts}

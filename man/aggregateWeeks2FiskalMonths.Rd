\name{aggregateWeeks2FiskalMonths}
\alias{aggregateWeeks2FiskalMonths}
\title{Aggregate time-series observations}
\description{Group observations by time and apply a summary function.}
\usage{
aggregateWeeks2FiskalMonths(Time, Data, FUN, Header, \dots) 
}
\arguments{
  \item{Time}{One timestamp per data row. Weekly fiscal input must use Monday Date values.}
  \item{Data}{Numeric vector, matrix, or data frame.}
  \item{FUN}{Function that returns one numeric value per group and column.}
  \item{Header}{Optional column names. Supply one name per data column, or names for all output columns. A single name is repeated with unique suffixes for matrix input.}
  \item{\dots}{Additional arguments for FUN, such as na.rm=TRUE.}
}
\details{
Use ISO week years and a repeating 5, 4, 4 week pattern. Add ISO week 53 to fiscal month 12. The fiscal year starts on the Monday of ISO week one. Time labels identify fiscal year and month; they are not calendar boundaries. Missing periods are not filled with zero. Daily values are aggregated directly, so partial weeks do not distort means. Row names give the first observed week in each group.
}
\value{
Data frame. The first column gives group dates. Other columns contain the aggregated data.
}
\examples{
time <- seq(as.Date("2020-01-06"),by="week",length.out=53)
out <- aggregateWeeks2FiskalMonths(time,rep(1,length(time)),sum)
stopifnot(sum(out$Data) == length(time))
}

\keyword{ts}

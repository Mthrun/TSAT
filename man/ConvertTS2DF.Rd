\name{ConvertTS2DF}
\alias{ConvertTS2DF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ConvertTS2DF
}
\description{
Converts a R time series to an R data.frame
}
\usage{
ConvertTS2DF(TSobject, Resolution = "Monthly", TimeZone = "UCT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TSobject}{
object of \code{\link{ts}}
}
  \item{Resolution}{
Monthly or Quaterly
}
  \item{TimeZone}{
see strptime
}
}

\value{
dataframe with Time and Data columns
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael Thrun
}

\examples{
data("AirPassengers")
ConvertTS2DF(AirPassengers)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ConvertTS2DF}% use one of  RShowDoc("KEYWORDS")
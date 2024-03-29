\name{aggregateTime2Days}
\alias{aggregateTime2Days}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
aggregate Time to Days
}
\description{
Aggregates a Time vector of POSIX format to Days
}
\usage{
aggregateTime2Days(Time, Data, FUN, Header,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] \code{POSIXlt} or \code{POSIXct}
}
  \item{Data}{
[1:n,1:d] matrix or dataframe, d can be also 1 , then vector
}
  \item{FUN}{
aggregation function, e.g. \code{mean} or \code{sum}
}
  \item{Header}{
Names of Outputfeatures minus Time column
}
  \item{\dots}{
further arguments passed on to \code{FUN}
}
}
\details{
all possible timeframes with higher frequency then days are accepted, aggregation shortcut wrapper
}
\value{
dataframe[1:m,1:(d+1)] with m<n and first column being the time in \code{as.Date} format
}

\author{
Michael Thrun
}

\examples{
#to be filled
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aggregateTime2Days}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line

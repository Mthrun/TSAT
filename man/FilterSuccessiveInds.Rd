\name{FilterSuccessiveInds}
\alias{FilterSuccessiveInds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filters consecutive Indizes
}
\description{
Filters consecutive Indizes
}
\usage{
FilterSuccessiveInds(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of Inds
}
}
\details{
Zahlenproblem c(1,2,3,4,9,14,15,16) filtern zu Anfang c(1,9,14) und c(4,1,3) Laenge
}
\value{
# Output List V of 
#  V$length[1:m]      length of consecutive inds
#  V$values[1:m]      start ind of consecutive inds
}
\references{
see seqle in \url{https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers}
}

\keyword{seqle}% use one of  RShowDoc("KEYWORDS")
\keyword{Successive}% __ONLY ONE__ keyword per line
\keyword{CEP}
\keyword{Complex event processing}
\keyword{event processing}
\keyword{time series}
\keyword{categorial time series}
\name{Long2WideTableByTime}
\alias{Long2WideTableByTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Long2WideTableByTime
}
\description{
Special pivot function for the case of Time
}
\usage{
Long2WideTableByTime(Time, Id, NumericVector, Freq = "days")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:m] time vector
}
  \item{Id}{
[1:m] id vector defining the features with \code{d} different features
}
  \item{NumericVector}{
[1:m] data vector of values fo the feautures (content of long table)
}
  \item{Freq}{
frequency of \code{Time}
}
}
\details{
The feautures are together a long table. Currently only use for a special case, requires probably further development and should be only used with care.
}
\value{
[1:n,1:d] data matrix (wide table) of n cases and d features
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael Thrun
}


\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{wide table}% use one of  RShowDoc("KEYWORDS")
\keyword{long table}% __ONLY ONE__ keyword per line
\keyword{pivot}% __ONLY ONE__ keyword per line
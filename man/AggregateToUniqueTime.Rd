\name{aggregateToUniqueTime}
\alias{aggregateToUniqueTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregate to Unique Time
}
\description{
Aggregation to Unique Time by any function
}
\usage{
aggregateToUniqueTime(Time, DataVectorOrMatrix, Fun = sum, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector
}
  \item{DataVectorOrMatrix}{
[1:n,1:d] vector (d==1) or matrix (d>1)
}
  \item{Fun}{
aggregation function like \code{\link{sum}}
}
  \item{\dots}{
further arguments passed on to \code{Fun}
}
}
\details{
Generalized Aggregation to Unique Time by any function defined by \code{sum}
}
\value{
Lixt with
  \item{Aggregation}{
[1:m,1:d], m<=n of \code{DataVectorOrMatrix} mapped to \code{UniqueTime}
}
  \item{UniqueTime}{
[1:m], m<=n  unique elements of \code{Time}
}
}

\author{
Michael Thrun
}



\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{AggregateToUniqueTime}% use one of  RShowDoc("KEYWORDS")
\keyword{aggregate}% __ONLY ONE__ keyword per line
\keyword{Aggregation}% __ONLY ONE__ keyword per line
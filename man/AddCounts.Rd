\name{AddCounts}
\alias{AddCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds 2 counts
}
\description{
adds two counts by searching for same numrical key or character vector
}
\usage{
AddCounts(Count1,Names1,Count2,Names2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Count1}{
  numerical vector[1:n]
}
  \item{Names1}{
  key or charactical vector [1:n]
}
  \item{Count2}{
    numerical vector[1:m]
}
  \item{Names2}{
    key or charactical vector [1:m]
}
}

\value{
matrix[1:k,1:2] with cbind(Names=NamesCombined,Counts=CountsCombined)
}

\author{
Michael Thrun
}

\keyword{Counts}% use one of  RShowDoc("KEYWORDS")
\keyword{Adds}% __ONLY ONE__ keyword per line

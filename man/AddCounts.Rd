\name{AddCounts}
\alias{AddCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds 2 counts
}
\description{
Adds two counts by searching for same numerical key or character vector
}
\usage{
AddCounts(Count1,Names1,Count2,Names2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Count1}{
  [1:n] numerical vector
}
  \item{Names1}{
  [1:n] key or charactical vector
}
  \item{Count2}{
  [1:m] numerical vector
}
  \item{Names2}{
  [1:m] key or charactical vector 
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

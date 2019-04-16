\name{HighestPrimePotence}
\alias{HighestPrimePotence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The highest prime potence of a whole number.
}
\description{
Compute the highest prime potence, i.e. the maximal $p^a$ in the prime factor decomposition of a non-zero number.
}
\usage{
HighestPrimePotence(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A non-zero whole number where we want to compute the highest prime factor from.
  }
}
\details{
Iteratively divide x by smaller increasing numbers until it has absolute value 1.
}
\value{
Maximum prime potence.
}
\author{
Julian Maerte
}
\examples{
HighestPrimePotence(200) # = 25
}
\keyword{PrimePotence}

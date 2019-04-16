\name{Divisors}
\alias{Divisors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
All divisors of a non-zero whole number.
}
\description{
Compute a vector containing all divisors of a non-zero whole number in natural order.
}
\usage{
Divisors(x)
}
\arguments{
  \item{x}{
A non-zero whole number.
  }
}
\details{
Checking every number between 1 and x to divide x evenly.
}
\value{
Sorted vector with all divisors of x.
}
\author{
Julian Maerte
}
\examples{
Divisors(200) # 1   2   4   5   8  10  20  25  40  50 100 200
}
\keyword{Divisors}

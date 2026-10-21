\name{SMAPE}
\alias{SMAPE}
\title{Symmetric absolute percentage errors}
\description{Calculate one symmetric absolute percentage error per observation pair.}
\usage{
SMAPE(X, Y, epsilon = 10^-10, na.rm = FALSE, Silent = TRUE) 
}
\arguments{
  \item{X}{Numeric forecast vector.}
  \item{Y}{Numeric observed vector of the same length.}
  \item{epsilon}{Nonnegative threshold for a near-zero sum of absolute values.}
  \item{na.rm}{If TRUE, remove nonfinite pairs. Otherwise, reject them.}
  \item{Silent}{Logical compatibility argument. It does not change the output shape.}
}
\details{
The error is \eqn{200 |Y-X|/(|X|+|Y|)}. Its range is zero to 200 percent. A denominator at or below epsilon gives zero. Take the mean of the returned vector to obtain SMAPE. This convention agrees with ForecastingInterface. Earlier versions used half this scale.
}
\value{
Numeric vector of errors, in percent.
}
\examples{
stopifnot(all.equal(SMAPE(1,2),200/3), SMAPE(0,0) == 0)
}

\keyword{ts}

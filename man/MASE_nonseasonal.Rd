\name{MASE_nonseasonal}
\alias{MASE_nonseasonal}
\title{Mean absolute scaled error}
\description{Divide mean absolute forecast error by mean absolute first differences.}
\usage{
MASE_nonseasonal(Y, For, epsilon = 10^-5, NonDuplicatedForecasts = FALSE,
  TrainingData = NULL)
}
\arguments{
  \item{Y}{Finite numeric observed vector.}
  \item{For}{Finite numeric forecast vector of the same length.}
  \item{epsilon}{Nonnegative threshold below which the scale is treated as zero.}
  \item{NonDuplicatedForecasts}{Deprecated. All observations are retained. TRUE produces a warning.}
  \item{TrainingData}{Optional finite training vector for the naive error scale.}
}
\details{
Use TrainingData for forecast evaluation. If omitted, the scale uses Y for compatibility. Repeated values remain separate observations. The result is NA when the scale is zero or fewer than two scale observations are available. No random noise is added.
}
\value{
One numeric score. Values below one indicate lower error than the naive training scale.
}
\examples{
stopifnot(MASE_nonseasonal(c(4,5),c(3,4),TrainingData=1:3) == 1)
stopifnot(is.na(MASE_nonseasonal(rep(1,5),rep(1,5))))
}

\keyword{ts}

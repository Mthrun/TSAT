\name{CommonForecastingErrors}
\alias{CommonForecastingErrors}
\title{Common forecast error measures}
\description{Calculate seven error measures from aligned observations and forecasts.}
\usage{
CommonForecastingErrors(TestdataY, ForecastingF, epsilon = 10^-4, na.rm = TRUE,
  stepsize = 1, digits, TrainingData = NULL)
}
\arguments{
  \item{TestdataY}{Numeric observed vector.}
  \item{ForecastingF}{Numeric forecast vector of the same length.}
  \item{epsilon}{Nonnegative threshold for near-zero MAPE denominators and the MASE scale.}
  \item{na.rm}{If TRUE, omit nonfinite evaluation pairs.}
  \item{stepsize}{Positive seasonal lag for the MASE scale.}
  \item{digits}{Optional number of decimal places. Use standard R rounding.}
  \item{TrainingData}{Optional finite training response for the MASE scale. If omitted, use the original TestdataY sequence.}
}
\details{
MAPE and SMAPE are percentages. MAPE is NA if any retained observation has absolute value at or below epsilon. SMAPE uses \eqn{200 |Y-F|/(|Y|+|F|)} and defines a zero pair as zero. MASE divides mean absolute error by the mean absolute training difference at the specified lag. An undefined scale gives NA. Missing scale observations also give NA. BIAS is the directional root-error measure from \code{RootDeviance}; it is zero for a perfect forecast. Earlier versions returned MAPE as a fraction and SMAPE on half the current scale.
}
\value{
Named numeric vector: MAE, MAPE, SMAPE, MASE, RMSE, BIAS, and MRD.
}
\examples{
out <- CommonForecastingErrors(c(-100,-200),c(-90,-180),TrainingData=1:10)
stopifnot(out["MAPE"] == 10)
}

\keyword{ts}

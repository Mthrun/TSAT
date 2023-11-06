\name{CommonForecastingErrors}
\alias{CommonForecastingErrors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Common Forecasting Errors
}
\description{
Calculate common forecasting errors
}
\usage{
CommonForecastingErrors(TestdataY,ForecastingF,

epsilon=10^-4,na.rm=TRUE,stepsize=1,digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TestdataY}{
[1:n] numerical vector of test data
}
  \item{ForecastingF}{
[1:n] numerical vector of forecast
}
  \item{epsilon}{
Optional, epsilon defining when zero values should be approximated. Default is 10^-4
}
  \item{na.rm}{
Optional, removing missing values. Default is TRUE
}
  \item{stepsize}{
Optional, for mase. See \code{\link[Metrics]{mase}}
}
  \item{digits}{
Optional, number of digits for the output values to which the results will be scientifically rounded to (and not by R logic)
}
}
\details{
MAE: Mean Absolute Error given as 
\eqn{\frac{1}{T} \sum_{t=1}^{T} |Y_t - F_t|}

MAPE: Mean Absolute Percentage Error given as 
\eqn{\frac{1}{T} \sum_{t=1}^{T} |\frac{Y_t - F_t}{Y_t}|}

SMAPE: Symmetric Mean Absolute Percentage Error given as 
\eqn{\frac{100}{T} * \frac{1}{T} \sum_{t=1}^{T} \frac{|Y_t-X_t|}{|X_t|+|Y_t|}}. 
See also \link{SMAPE}

MASE: Mean Absolute Scaled Error given as 
\eqn{\frac{T}{T-s}*\frac{|Y_t - F_t|}{\sum_{t=s}^{T}|Y_t - Y_{t-s}|}}
where s is the stepsize. See also \code{\link[Metrics]{mase}}

RMSE: Root Mean Square Error given as 
\eqn{\sqrt{\sum_{t=1}^{T} \frac{(Y_t - F_t)^2}{T}}}. See also \code{\link[Metrics]{rsme}}

BIAS: Value between -1 and 1 given as 
\eqn{1 - \frac{4}{\pi}\text{arctan}(\frac{b}{a})}, 
where \eqn{b} is the sum of all errors \eqn{Y_t - F_t < 0} 
and \eqn{a} is the sum of all errors \eqn{Y_t - F_t \geq 0}.
See also \code{\link{RootDeviance}}

MRD: Mean Root Deviance given as 
\eqn{ \frac{1}{T}\sum_{t=1}^{T}\sqrt{Y_t - F_t}}.
See also \code{\link{RootDeviance}}

}
\value{
Named vector with forecasting error values for:

'MAE','MAPE','SMAPE','MASE','RMSE','BIAS','MRD'
}
\references{
to be filled
}
\author{
Michael Thrun
}


\examples{
Y=runif(10)
FOR=runif(10)
CommonForecastingErrors(Y,FOR)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CommonForecastingErrors}% use one of  RShowDoc("KEYWORDS")


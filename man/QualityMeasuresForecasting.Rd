\name{QualityMeasuresForecasting}
\alias{QualityMeasuresForecasting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality Measures for Forecasting
}
\description{
Conventional Quality Measures for Forecasting
}
\usage{
QualityMeasuresForecasting(Y,FOR,Indices,Mase_Season=1,NonDuplicatedForecasts=FALSE,Silent=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
1:n vector of ordered and regular time series values
}
  \item{FOR}{
1:n vector of ordered and regular forecast values in the same odering as Y
}
  \item{Indices}{
default all values, sometimes its usefull to restrict the evaluation
}
  \item{Mase_Season}{
Seasonlaity for MASE, i.e. stepsize, Default(1) means against naiv model
}
  \item{Silent}{
TRUE: No Warnings or errors are given back
}
}
\details{
bias computes the average amount by which actual is greater than predicted.

rae divides sum(ae(actual, predicted)) by sum(ae(actual, mean(actual))), meaning that it provides the absolute error of the predictions relative to a naive model that predicted the mean for every data point.
 
rse divides sse(actual, predicted) by sse(actual, mean(actual)), meaning that it provides the squared error of the predictions relative to a naive model that predicted the mean for every data point.
 
 MRD = MRD(x,y) - Mean of the root errors. (Sum of the root errors / length(x)) with length = sqrt(a^2 + b^2) - Length of the complex number with real part the errors where X is over Y and imaginary part the errors where X is under Y.
 
 MRD_bias  Is a number bound between -1 and 1. It is 0 iff Y has the same rooted error to the upper as it has to the lower side of X. Positivity means Y deviates more to the lower side of X and conversly negativity reads as Y deviates more the upper side of X.
}
\value{
vector of 
'RMSE','MAE','MAPE','SMAPE','MASE','AbsoluteError','Bias','RAE','RSE','MRD','MRD_BIAS'
}

\author{
Michael Thrun
}

\examples{
Y=runif(10)
FOR=runif(10)
QualityMeasuresForecasting(Y,FOR)
}

\keyword{Forecasting}% use one of  RShowDoc("KEYWORDS")
\keyword{Quality}% __ONLY ONE__ keyword per line

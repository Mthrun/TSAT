\name{FcGeneralizedLinearModels4TS}
\alias{FcGeneralizedLinearModels4TS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Generalized Linear Models for time series
}
\description{
Generalized linear models for the forecast of time series in the multivariate case
}
\usage{
FcGeneralizedLinearModels4TS(Response, SplitAt, Predictor1, Predictor2 = NULL, 

CorrectionFactor=FALSE, PlotIt = TRUE,Time, Summary = FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Response}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{SplitAt}{
scalar 'k' with k<n, index of row where the \code{DataFrame} is divided into test and train data
}
  \item{Predictor1}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{Predictor2}{
[1:n] vector with an value of each time j in [1,n]. Can also be \code{NULL} if not used
}
  \item{Time}{
Optional, [1:n] time vector in case of \code{PlotIt=TRUE}
}
  \item{CorrectionFactor}{
Scalar, if TRUE: the predicted time series will begin at the same point as the last value of the training set. All other values will be mulitplied by a factor accordingly.
}
  \item{PlotIt}{
Optional,FALSE (default), do nothing. TRUE: plots an evaluation of the forecast on the test data FALSE: Plots data model and prediction with
}
  \item{Summary}{
Output of \code{glm} is evaluated further
}
  \item{\dots}{
Further arguments passed to \code{glm}
}
}
\details{
Assumption: \code{Response} and 'Predictors' have the same time intervals, the same starting time and the same length.
}
\value{
list with
\item{Forecast}{[k:n], the forecast, of the time interval [k,n] which was not used in the model}
\item{TestSet}{[k:n], the part of Response not used in the model}
\item{Accuracy}{ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix}
\item{Errors}{Residuals: TestSet-Forecast}
\item{Model}{Output of \code{glm}}

}

\author{
Michael Thrun
}
\note{
Wrapper for \code{glm} with some additional features
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{glm}% use one of  RShowDoc("KEYWORDS")
\concept{time series}% __ONLY ONE__ keyword per line
\keyword{GeneralizedLinearModels4TS}% __ONLY ONE__ keyword per line
\concept{Generalized linear model}% __ONLY ONE__ keyword per line
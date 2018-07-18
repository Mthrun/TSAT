\name{GeneralizedLinearModels4TS}
\alias{GeneralizedLinearModels4TS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GeneralizedLinearModels4TS
}
\description{
Generalized linear models for the forecast of time series in the multivariate case
}
\usage{
GeneralizedLinearModels4TS(Response, SplitDataAt, Predictor1, Predictor2 = NULL, Time, PlotIt = TRUE, Summary = FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Response}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{SplitDataAt}{
scalar 'k' with k<n, index of row where the \code{DataFrame} is divided into test and train data
}
  \item{Predictor1}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{Predictor2}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{Time}{
Optional, [1:n] time vector in case of \code{PlotIt=TRUE}
}
  \item{PlotIt}{
Optional, TRUE: Plots data model and prediction with
}
  \item{Summary}{
Output of \code{glm} is evaluted further
}
  \item{\dots}{
Further arguments passed to \code{glm}
}
}
\details{
Assumption: \code{Response} and 'Predictors' have the same time intervalls, the same starting time and the same length.
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
\keyword{time series}% __ONLY ONE__ keyword per line
\keyword{GeneralizedLinearModels4TS}% __ONLY ONE__ keyword per line
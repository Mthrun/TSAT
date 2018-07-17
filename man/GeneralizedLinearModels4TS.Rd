\name{GeneralizedLinearModels4TS}
\alias{GeneralizedLinearModels4TS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GeneralizedLinearModels4TS
}
\description{
Generalized linear models for the prediction of time series in the multivariate case
}
\usage{
GeneralizedLinearModels4TS(Response, SplitDataAt, Predictor1, Predictor2 = NULL, Time, PlotIt = TRUE, Summary = FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Response}{
%%     ~~Describe \code{Response} here~~
}
  \item{SplitDataAt}{
%%     ~~Describe \code{SplitDataAt} here~~
}
  \item{Predictor1}{
%%     ~~Describe \code{Predictor1} here~~
}
  \item{Predictor2}{
%%     ~~Describe \code{Predictor2} here~~
}
  \item{Time}{
%%     ~~Describe \code{Time} here~~
}
  \item{PlotIt}{
%%     ~~Describe \code{PlotIt} here~~
}
  \item{Summary}{
%%     ~~Describe \code{Summary} here~~
}
  \item{\dots}{
Further arguments passed to \code{glm}
}
}
\details{
to be filled
}
\value{
list with
\item{Predicted}{}

\item{ME}{}
\item{Errors}{}
\item{Model}{}

}

\author{
Michael Thrun
}
\note{
Wrapper for \code{glm} and \code{predict} with some additional features
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{glm}% use one of  RShowDoc("KEYWORDS")
\keyword{time series}% __ONLY ONE__ keyword per line
\keyword{GeneralizedLinearModels4TS}% __ONLY ONE__ keyword per line
\name{CrossCorr}
\alias{CrossCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ccf
}
\description{
Cross-correlation or cross-covariance of two univariate series of observations
}
\usage{
CrossCorr(TimeSeries1, TimeSeries2, nLags = 2,type = "correlation",PlotIt=FALSE,main,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries1}{
see output of \code{ccf}
}
  \item{TimeSeries2}{
see output of \code{ccf}
}
  \item{nLags}{
see output of \code{ccf}
}
  \item{type}{
character string giving the type of acf to be computed. Allowed values are "correlation" (the default), "covariance" 
}
  \item{PlotIt}{
Optional, if TRUE, output is ploted
}
  \item{main}{
Optional, title of plot
}
  \item{\dots}{
further arguments passed on to output of \code{ccf}
}
}
\details{
Covariance is a measure indicating the extent to which two random variables change in tandem.	Correlation is a scalar measure that indicates how strongly two variables are statistically linearly related which does not mean that they are related by causality.
}
\value{
output of \code{ccf}
}

\author{
Michael Thrun
}
\note{Covoriance is not normalized, Instead it is in the multiplication of units of both time series.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ccf}% use one of  RShowDoc("KEYWORDS")

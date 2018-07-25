\name{CrossCorr}
\alias{CrossCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross-correlation or cross-covariance of two univariate series of observations
}
\description{
Wrapper for the \code{\link[stats]{ccf}} function with additional features
}
\usage{
CrossCorr(TimeSeries1, TimeSeries2, nLags = 2,type = "correlation",PlotIt=FALSE,main,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries1}{
[1:n] vector or time series, see also input of \code{\link[stats]{ccf}}
}
  \item{TimeSeries2}{
[1:n] vector or time series, see also input of \code{\link[stats]{ccf}}
}
  \item{nLags}{
positive scalar, the limit number of lags (in units of observations) to be inspected forward an backward,  see also input of \code{ccf}
}
  \item{type}{
character string giving the type of acf to be computed. Allowed values are "correlation" (the default), "covariance" 
}
  \item{PlotIt}{
Optional, if TRUE, output is ploted as a dashboard with values as bars against various lags, for the lag with highest cross correlation or covariance additonaly the spearman rank corrrelation with a scatter plot is shown.
}
  \item{main}{
Optional, title of plot
}
  \item{\dots}{
further arguments passed on to output of \code{\link[stats]{ccf}}
}
}
\details{
Covariance is a measure indicating the extent to which two random variables change in tandem.	Correlation is a scalar measure that indicates how strongly two variables are statistically linearly related which does not mean that they are related by causality. For \code{nLags}: back shifting: positiv values, forward shifting: negative values.
}
\value{
output of \code{\link[stats]{ccf}} in \code{\link[base]{invisible}} mode
}

\author{
Michael Thrun
}
\note{Covoriance is not normalized, Instead it is in the multiplication of units of both time series.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ccf}% use one of  RShowDoc("KEYWORDS")

\name{CrossCorr}
\alias{CrossCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross-Correlation of Time-Dependent Observations
}
\description{
\code{CrossCorr} is the function for the cross-correlation or cross-covariance of two univariate series of observations using numerical vectors. The function is a Wrapper for the \code{\link{ccf}} function with additional features.

Beware, \code{CrossCorr} assumes implicitly that both time series have the same frequency and thus accepts simple numerical vectors.
}
\usage{
CrossCorr(FirstTimeSeries, SecondTimeSeries, 

nLags = 2,type = "correlation",PlotIt=FALSE,main,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FirstTimeSeries}{
[1:n] vector or time series, see also input of \code{\link{ccf}}
}
  \item{SecondTimeSeries}{
[1:n] vector or time series, see also input of \code{\link{ccf}}
}
  \item{nLags}{
Positive scalar, the limit number of lags (in units of observations) to be inspected forward and backward in \code{FirstTimeSeries} relative to the fixed \code{SecondTimeSeries}, see also input of \code{ccf}. Default is 2
}
  \item{type}{
Character string giving the type of acf to be computed. Allowed values are "correlation" (the default), "covariance" 
}
  \item{PlotIt}{
Optional, if TRUE, output is plotted as a dashboard with values as bars against various lags, for the lag with highest cross correlation or covariance additionally the spearman rank correlation with a scatter plot is shown. Default is FALSE
}
  \item{main}{
Optional, title of plot.
}
  \item{\dots}{
Further arguments passed on to output of \code{\link{ccf}}
}
}
\details{
Procedure ist as follows: the timeseries is moved forward from \code{FirstTimeSeries[t+1]} to \code{FirstTimeSeries[t+nLags]} and backward from \code{FirstTimeSeries[t-1]} to \code{FirstTimeSeries[t-nLags]} and every lagged time series \code{FirstTimeSeries} is compared to \code{SecondTimeSeries}.

A cross correlation at lag \code{nlag} means that the value nlag-time steps ahead of the \code{FirstTimeSeries} correlates with the value of the current time step of the \code{SecondTimeSeries}. 

Covariance is a measure indicating the extent to which two random variables change in tandem.	Correlation is a scalar measure that indicates how strongly two variables are statistically linearly related which does not mean that they are related by causality. For \code{nLags}: back shifting: positiv values, forward shifting: negative values.

}
\value{
output of \code{\link{ccf}} in \code{\link[base]{invisible}} mode
}

\author{
Michael Thrun
}
\note{Covoriance is not normalized, Instead the normalization is in the multiplication of units of both time series.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ccf}% use one of  RShowDoc("KEYWORDS")

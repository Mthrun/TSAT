\name{CointegrationOfTwoTS}
\alias{CointegrationOfTwoTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Investigate the Cointegration between two time series
}
\description{
If TS1 and TS2 are cointegrated then a linear combination must be stationary.
}
\usage{
CointegrationOfTwoTS(TS1, TS2, alpha = 1, type='c',PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS1}{
[1:n] vector of data
}
  \item{TS2}{
[1:n] vector of data
}
  \item{alpha}{
linear factor, do not search for right alpha with PLS statistics.
}
  \item{type}{
a character string describing the type of \code{\link[fUnitRoots]{UnitrootTests}} (the unit root regression). Valid choices are "nc" for a regression with no intercept (constant) nor time trend, and "c" for a regression with an intercept (constant) but no time trend, "ct" for a regression with an intercept (constant) and a time trend. The default is "c".
}
  \item{PlotIt}{
TRUE: plot distribution analysis of \code{CointegrationOrder1} ( which seems to be the residuals?)
}
}
\details{
"In the econometric literature, a time series zt is said to be an integrated process of
order 1, that is, an I(1) process, if (1−B)zt is stationary and invertible. In general, a
univariate time series zt is an I(d) process if (1−B)dzt is stationary and invertible,
where d > 0. The order d is referred to as the order of integration or the multiplicity
of a unit root. A stationary and invertible time series is said to be an I(0) process." [Tsay, 2013, p.295]

In short, A cointegration relationship exists between two (or more) time series when there is a long-term balance between two (or more) transient (integrated) variables. Cointegration is applied to detrend time series (non-stationary time series). It represents an alternative to a detrending through the computation of differences (\code{\link{DiffFilter}}). Trend correction is often proposed in order to avoid false regressions. 
}
\value{
List of

\item{CointegrationOrder1}{  CointegrationOrder1=TS1-alpha*TS2}
\item{adf_fUnitRoots}{Check cointegration with \code{\link[fUnitRoots]{UnitrootTests}}}
\item{adg_tseries}{Check cointegration with \code{\link[tseries]{adf.test}}}

}
\references{
[Tsay, 2013]  Tsay, Ruey S: Multivariate time series analysis: with R and financial applications, John Wiley & Sons, 2013.

[Box and Tiao, 1977]  Box, G. E. P. and Tiao, G. C. (1977): A canonical analysis of multiple time serie, Biometrika,
64, pp. 355–366, 1977.
}
\author{
Michael Thrun
}
\note{
  regression analysis fails if ts are non stationary
  
  the simplest unit-root nonstationary time series is the univariate random walk
  
  linear combination of several unit-root nonstationary time series can
become a stationary series [Box and Tiao, 1977].
}

\seealso{
\code{\link[tseries]{po.test}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CointegrationOfTwoTS}% use one of  RShowDoc("KEYWORDS")
\keyword{Cointegration}% __ONLY ONE__ keyword per line

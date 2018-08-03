\name{WhiteNoiseTest}
\alias{WhiteNoiseTest}
\alias{Test4WhiteNoise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for White Noise
}
\description{
test fails to reject the null hypothesis of no white noise if p-value > 0.05
}
\usage{
WhiteNoiseTest(TimeSeries, lags = 1, type = "c", PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{
[1:n] vector of data, e.g. residuals of time series
}
  \item{lags}{
number of lags to investigate in the statistical test
}
  \item{type}{
a character string describing the type of \code{\link[fUnitRoots]{UnitrootTests}} (the unit root regression). Valid choices are "nc" for a regression with no intercept (constant) nor time trend, and "c" for a regression with an intercept (constant) but no time trend, "ct" for a regression with an intercept (constant) and a time trend. The default is "c".

}
  \item{PlotIt}{
 if each sample has a normal distribution with zero mean, the signal is said to be Gaussian white noise [Diebold, 2007].
}
}
\details{
white noise can be described as a random process, e.g. Brownian Movement, Random Walk.
The simplest unit-root nonstationary time series is the univariate random walk [Tsay, 2013].
Therefore, using distribution analysis and a unit root test, this function can serve as a indication for white noise, because unit root is a feature of white noise.
If the mean is around zero (red line visible in plot) and the pvalue is small than white noise can be assumed. It is a trying taks to generally to proof white noise. Thus, if one of the two approaches (statistical versus visual) do not agree, than the result is unclear and the residuals should be testet with other approaches.
}
\value{
output of \code{\link[fUnitRoots]{UnitrootTests}} in mode \code{\link{invisible}}
}
\references{
[Tsay, 2013]  Tsay, Ruey S: Multivariate time series analysis: with R and financial applications, John Wiley & Sons, 201.

[Diebold, 2007]   Diebold, Frank: Elements of Forecasting (Fourth ed.), 2007
}
\author{
Michael Thrun
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fUnitRoots]{UnitrootTests}}
temporal Data Mining, A.Ultsch
}
\examples{

 x = rnorm(1000) 
 WhiteNoiseTest(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{WhiteNoise}% use one of  RShowDoc("KEYWORDS")
\keyword{unitroot}% __ONLY ONE__ keyword per line

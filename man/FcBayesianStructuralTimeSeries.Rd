\name{FcBayesianStructuralTimeSeries}
\alias{FcBayesianStructuralTimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Bayesian Structural Time Series
}
\description{
Forecasting with Bayesian Structural Time Series
}
\usage{
FcBayesianStructuralTimeSeries(DataVec, SplitAt, Time, Frequency = "months", ForecastHorizon, nseasons = 12, niter = 250, burn = 100, PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataVec}{
[1:n] numerical vector of time series data.
}
  \item{SplitAt}{
Index of row where the DataVec is divided into test and train data. If not given n is used
}
  \item{Time}{
[1:n] character vector of Time in the length of data
}
  \item{Frequency}{
Either \code{days}, \code{weeks}, \code{months} or \code{quarters} or \code{years}, see \code{\link{ConvertNumerical2TSobject}}.
}
  \item{nseasons}{
Optional, number of seasons. See \code{\link[bsts]{AddSeasonal}}
}
  \item{niter}{
Optional, number of Iterations. See \code{\link[bsts]{bsts}}}

  \item{burn}{
Optional, Number of MCMC iterations to be discarded as burn-in. See \code{\link[bsts]{predict.bsts}}
}
  \item{PlotIt}{
FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
}
}
\details{
Wrapper for functions of the bsts package.
}
\value{
List with
\item{Forecast}{[1:ForecastHorizon] forecasted values}
\item{ForecastTime}{[1:ForecastHorizon] Time values of forecasts}
\item{UpperLower}{[1:2,1:ForecastHorizon] matrix of upper and lower bounds of the credible interval for the prediction, see \code{\link[bsts]{predict.bsts}}}
\item{ModelPrediction}{Object of class predict of bsts package, see \code{\link[bsts]{predict.bsts}}}
\item{Model}{Model object of class bsts, see \code{\link[bsts]{bsts}}}

%% ...
}
\references{
[Scott/Varian, 2014]  Scott, S. L., & Varian, H. R.: Predicting the present with bayesian structural time series, International Journal of Mathematical Modelling and Numerical Optimisation, Vol. 5(1-2), pp. 4-23. 2014.


}
\author{
Michael Thrun
}
\note{
doku to be written...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[bsts]{bsts}}
}
\examples{

data("AirPassengers")
AirPassengers=TSAT::ConvertTS2DF(AirPassengers)
ff=FcBayesianStructuralTimeSeries(log(AirPassengers$Data),AirPassengers$Time,SplitDataAt=144-12,PlotIt=TRUE)

##New Data
data("AirPassengers")
AirPassengers=TSAT::ConvertTS2DF(AirPassengers)

ff=FcBayesianStructuralTimeSeries(log(AirPassengers$Data),AirPassengers$Time,SplitDataAt=144,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{bsts}
\keyword{Forecast}
\concept{Bayesian Structural Time Series}

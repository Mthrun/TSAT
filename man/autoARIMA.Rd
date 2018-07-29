\name{autoARIMA}
\alias{autoARIMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic ARIMA
}
\description{
automatic autoregressive and moving average modelling with difference filter
}
\usage{
autoARIMA(Data, Time, ForecastHorizon = 20, PlotIt = T,

PlotBackwardInd, main = "", xlab = "Time", ylab = "Data", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n] Datavector
}
  \item{Time}{
[1:n] \code{POSIXlt} vector
}
  \item{ForecastHorizon}{
Number of Forecast units of time
}

  \item{PlotIt}{
Optional, if TRUE: Plots result
}
  \item{PlotBackwardInd}{
Optional, How many units to plot back in time?
}
  \item{main}{
see \code{\link{plot}}
}
  \item{xlab}{
see \code{\link{plot}}
}
  \item{ylab}{
see \code{\link{plot}}
}
  \item{\dots}{
further arguments passed on to see \code{\link{plot}}
}
}
\details{
- Autoregressive part (AR) with Lag 4 w.r.t signal term

- Difference Filter (I): 1 Lag, because noon stationary TS (time dependent expectation value of time series)

- Moving Average (MA) with Lag 4 w.r.t noise term

- requires homoscedastic time series - variance does not depent on time
}
\value{
List of 
\item{Forecast}{[1:ForecastHorizon] of Data}
\item{Model}{Model, the output of \code{\link[forecast]{auto.arima}}}
in mode \code{\link{invisible}}
}
\references{
Hyndman, RJ and Khandakar, Y (2008) "Automatic time series forecasting: The forecast package for R", Journal of Statistical Software, 26(3).

Wang, X, Smith, KA, Hyndman, RJ (2006) "Characteristic-based clustering for time series data", Data Mining and Knowledge Discovery, 13(3), 335-364.
}
\author{
Michael Thrun
}
\note{
wrapper for \code{\link[forecast]{auto.arima}}
}

\seealso{
\code{\link[forecast]{auto.arima}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arma}% use one of  RShowDoc("KEYWORDS")
\keyword{arima}% __ONLY ONE__ keyword per line

\name{FcAutoARIMA}
\alias{FcAutoARIMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forcasting with Automatic ARIMA
}
\description{
Automatic autoregressive and moving average modelling with difference filter
}
\usage{
FcAutoARIMA(DataVec, SplitAt, ForecastHorizon, Time, PlotIt = TRUE,


                 Seasonal=TRUE,PlotBackwardInd, main = "", xlab = "Time", ylab =
                 
                 
                 "Data", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataVec}{
[1:n] numerical vector of regular (equidistant) time series data.
}
  \item{SplitAt}{
Integer, splits data in test and train at this index. If not given n is used
}
  \item{ForecastHorizon}{
Number of Forecast units of time
}
  \item{Time}{
[1:n] character vector of Time in the length of data
}
  \item{PlotIt}{
Optional, if TRUE: Plots result
}
  \item{Seasonal}{
Optional, if FALSE: no seasonality in data an one step forecast only
}
  \item{PlotBackwardInd}{
Optional, how many units to plot back in time?
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

- requires homoscedastic time series - variance does not depend on time

If \code{n-SplitAt=ForecastHorizon} then forecast is completely on test data.

If \code{ForecastHorizon>n-SplitAt} then a real forecast is made of \code{ForecastHorizon-(n-SplitAt)} steps.

If \code{SplitAt=n} then the complete data set is used in estimating the forecast.
}
\value{
List of 
\item{Forecast}{[1:ForecastHorizon] of test data Y, test time and forecast FF}
\item{ArimaObject}{Forecast object, the output of \code{\link[forecast]{forecast}}}
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

\examples{
data("TempMelbourneAustralia")
res = FcAutoARIMA(TempMelbourneAustralia$Temp, SplitAt = length(TempMelbourneAustralia$Temp)-250,ForecastHorizon = 250, Time=TempMelbourneAustralia$Time)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arma}% use one of  RShowDoc("KEYWORDS")
\keyword{arima}% __ONLY ONE__ keyword per line

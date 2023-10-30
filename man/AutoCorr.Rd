\name{AutoCorr}
\alias{AutoCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AutoCorrelation
}
\description{
Calculates the AutoCorrelation function
}
\usage{
AutoCorr(TimeSeries, nLags = 2, PlotIt = FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{
A numeric time series object, or a numeric vector/matrix.
}
  \item{nLags}{
Number of lags. Default is 2. 
}
  \item{PlotIt}{
Boolean, TRUE if auto correlation plot should be printed, FALSE else. Default is FALSE.  
}
  \item{\dots}{
further arguments passed on to output of \code{\link{ccf}}
}
}
\details{
Calculation is based on the sample covariance. For further details see \code{\link{acf}}.
}
\value{
List with
\item{acf }{A three dimensional array containing the estimated acf.}
\item{type }{"Correlation".}
\item{n.used }{The number of observations in the time series.}
\item{lag }{A three dimensional array containing the lags at which the acf is estimated.}
\item{series }{Name of time series}
\item{snames }{The series names for a multivariate time series. NULL for univariate time series.}
}

\author{
Michael Thrun
}

\examples{
data("ElectricityBRD")
res = AutoCorr(ElectricityBRD$Mrd_KWh, nLags = 5, PlotIt = TRUE, main="Electricity production in BRD in Mrd KWh")
}

\keyword{acf}
\keyword{AutoCorrelation}
\concept{Time Series}


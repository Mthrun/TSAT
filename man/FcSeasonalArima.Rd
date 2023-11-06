\name{FcSeasonalArima}
\alias{FcSeasonalArima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Seasonal Arima
}
\description{
Automatic Forecasting using the seasonal arima method based on the X-13-ARIMA-SEATS standard.
}
\usage{
FcSeasonalArima(DataVec, SplitAt, ForecastHorizon, Frequency, Time, PlotIt = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataVec}{
[1:n] [1:n] numerical vector or a time series object. If a time series object is given, the argument "Frequency" does not need to be given.
}
  \item{SplitAt}{
Index of row where the DataVec is divided into test and train data. If not given n is used
}
  \item{ForecastHorizon}{
Scalar defining the timesteps to forecast ahead. Beware, it is NOT the internal forecast horizont.
}
  \item{Frequency}{
Either \code{days}, \code{weeks}, \code{months} or \code{quarters} or \code{years}, see \code{\link{ConvertNumerical2TSobject}}. Can be ignored if DataVec is a time series object.
}
  \item{Time}{
[1:n] character vector of Time in the length of data. Can be ignored if DataVec is a time series object.
}
  \item{PlotIt}{
FALSE (default), do nothing. TRUE: plots the forecast versus test data of time series data.
}
  \item{\dots}{
Further specification using the \code{\link[seasonal]{seas}} interface of the X-13-ARIMA-SEATS standard. Please see also references for further details
}
}
\details{
X-13-ARIMA-SEATS is the standard of the seasonal adjustment software by the US Census Bureau. This function is an easy-to-use wrapper of the function \code{\link[seasonal]{seas}} calling the interface.

If \code{n-SplitAt=ForecastHorizon} then forecast is completely on test data.

If \code{ForecastHorizon>n-SplitAt} then a real forecast is made of \code{ForecastHorizon-(n-SplitAt)} steps.

If \code{SplitAt=n} then the complete data set is used in estimating the forecast.
}
\value{
List with
  \item{Forecast}{
[1:Horizont] object of \code{\link[stats]{ts}}
}
  \item{Accuracy}{
Accuracy of TestSet against Forecast, if \code{TestandTrain==TRUE}
}
  \item{TaT}{
TrainingSet and TestSet, if \code{TestandTrain==TRUE}
}
  \item{Model}{
Output of \code{\link[seasonal]{seas}}
}
  \item{ForecastMaxlead}{
Output of \code{\link[seasonal]{series}}, which is an extended forecast if upper and lower boundary.
}
}
\references{
\url{https://www.census.gov/ts/x13as/docX13AS.pdf}

\url{https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R#77-forecast}
}
\author{
Michael Thrun
}

\note{
The default internal forecast horizont is one year (unless  a  SEATS seasonal adjustment is requested - then the default is three years of forecasts) and 120 is the maximum. Setting \code{Horizont} lower results in 3 year forecast saved in \code{ForecastMaxlead}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[seasonal]{seas}}, \code{\link[seasonal]{series}}
}
\examples{
# Example using a time series object for DataVec
data("AirPassengers")
ForecastHorizon=12
FcSeasonalArima(AirPassengers,SplitAt=length(AirPassengers)-ForecastHorizon,
                ForecastHorizon=ForecastHorizon)

# Example using a numerical vector for DataVec
data("ElectricityBRD$Mrd_KWh")
ForecastHorizon=12
FcSeasonalArima(ElectricityBRD$Mrd_KWh,SplitAt = length(ElectricityBRD$Mrd_KWh) - ForecastHorizon, 
                Frequency='months', ForecastHorizon = ForecastHorizon, 
                Time=ElectricityBRD$Time)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SeasonalArima}% use one of  RShowDoc("KEYWORDS")
\keyword{sarima}% __ONLY ONE__ keyword per line
\keyword{Forecast}

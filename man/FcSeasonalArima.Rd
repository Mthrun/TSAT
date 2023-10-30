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
FcSeasonalArima(TimeSeries, Horizon, TestandTrain = TRUE, PlotIt = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{
[1:n] object of \code{\link[stats]{ts}}
}
  \item{Horizon}{
Forecast horizon, has to be set. Beware, it is NOT the internal forecast horizont.
}
  \item{TestandTrain}{
TRUE: Data is divided by \code{Horizont}

FALSE: Real Forecast is generated
}
  \item{PlotIt}{
Plots the result
}
  \item{\dots}{
Further specification using the \code{\link[seasonal]{seas}} interface of the X-13-ARIMA-SEATS standard. Please see also references for further details
}
}
\details{
X-13-ARIMA-SEATS is the standard of the seasonal adjustment software by the US Census Bureau. This function is an easy-to-use wrapper of the function \code{\link[seasonal]{seas}} calling the interface.
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
output of \code{\link[seasonal]{seas}}
}
  \item{ForecastMaxlead}{
output of \code{\link[seasonal]{series}}, which is an extended forecast if upper and lower boundary.
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
FcSeasonalArima(AirPassengers,12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SeasonalArima}% use one of  RShowDoc("KEYWORDS")
\keyword{sarima}% __ONLY ONE__ keyword per line
\keyword{Forecast}

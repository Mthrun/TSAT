\name{MAPA_daily2FiskalWeek}
\alias{MAPA_daily2FiskalWeek}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
wrapper for MAPA in one special use case of given daily data
}
\description{
MAPA does not work on daily data well. Hence, data is aggregated to monthly data, forecasted and then disaggregated to weekly data
of a Fiskal week (5-4-4 week months).
}
\usage{
MAPA_daily2FiskalWeek(DailyTime, DailyData, SplitAt, ForecastHorizon = 12, ForecastHorizonMonthly = 12, FiskalMonthSeason = 12, FUN = sum, Confidence = c(0.95), na.rm = TRUE, ...)
}

\arguments{
  \item{DailyTime}{
%%     ~~Describe \code{DailyTime} here~~
}
  \item{DailyData}{
%%     ~~Describe \code{DailyData} here~~
}
  \item{SplitAt}{
Integer for splitting time of a monthly frequency
}
  \item{ForecastHorizon}{
Forecast horizon in weeks used after mapa modelling to generate output,

numer of weeks can not be higher than the weeks given in the months of 
\code{ForecastHorizonMonthly}
}
  \item{ForecastHorizonMonthly}{
Forecast horizon in months used in mapa, 
}
  \item{FiskalMonthSeason}{
%%     ~~Describe \code{FiskalMonthSeason} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{Confidence}{
%%     ~~Describe \code{Confidence} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{\dots}{
Further arguments for \code{\link[MAPA]{mapa}}
}
}
\details{
Multiple Aggregation Prediction Algorithm is used. Function assumes a regular time series of daily data and a split at the monthly level.
}
\value{
List of
\item{MAPA_F}{data frame, 1:ForecastHorizon, testdata, testtime and forecast FF}
\item{MAPA_Object}{}
\item{Conf}{data frame, 1:ForecastHorizon of Confidence Intervall}
}
\references{
Kourentzes N., Petropoulos F., Trapero J.R. (2014) Improving forecasting by estimating time series structural components across multiple frequencies. International Journal of Forecasting, 30(2), 291–302.

Kourentzes N., Petropoulos F. (2015) Forecasting with multivariate temporal aggregation: The case of promotional modelling. International Journal of Production Economics.
}
\author{
Michael Thrun
}
\note{
Other frequencies are under development
}


\seealso{
\code{\link[MAPA]{mapa}}, ~~~
}
\examples{
## TODO
}

\keyword{MAPA}% use one of  RShowDoc("KEYWORDS")
\keyword{Fiskal}% __ONLY ONE__ keyword per line
\keyword{weekly}
\keyword{forecasting}
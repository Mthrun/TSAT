\name{FcBayesianStructuralTimeSeries}
\alias{FcBayesianStructuralTimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Bayesian Structural Time Series
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FcBayesianStructuralTimeSeries(Data, Time, Frequency = "months", nseasons = 12, niter = 250, SplitDataAt = 12, ForecastPeriods = 12, burn = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{Time}{
%%     ~~Describe \code{Time} here~~
}
  \item{Frequency}{
%%     ~~Describe \code{Frequency} here~~
}
  \item{nseasons}{
%%     ~~Describe \code{nseasons} here~~
}
  \item{niter}{
%%     ~~Describe \code{niter} here~~
}
  \item{SplitDataAt}{
%%     ~~Describe \code{SplitDataAt} here~~
}
  \item{ForecastPeriods}{
%%     ~~Describe \code{ForecastPeriods} here~~
}
  \item{burn}{
%%     ~~Describe \code{burn} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

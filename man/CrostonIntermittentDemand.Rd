\name{CrostonIntermittentDemand}
\alias{CrostonIntermittentDemand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Croston Intermittent Demand
}
\description{
The Croston model answers the question How much demand will we have on average per period?´
This \code{CrostonIntermittentDemand} returns forecasts and other information for Croston's forecasts.
}
\usage{
CrostonIntermittentDemand(NumericVector, Time,

ForecastHorizon,SplitAt,

Frequency = "days", ModelType = "sbj", 

PlotIt = FALSE, ...)
}
\arguments{
  \item{NumericVector}{
[1:n] data vector
}
  \item{Time}{
[1:n] Time object of class \code{Date}}
  \item{Frequency}{
either \code{days}, \code{weeks}, \code{months} or \code{quarters} or \code{years}
}
  \item{ForecastHorizon}{
Forecast horizon, has to be set. 
}
  \item{SplitAt}{
Data is divided by \code{SplitAt} into training and test set

}
  \item{ModelType}{
Croston's method variant: 1. "croston" Croston's method; 2. "sba" Syntetos-Boylan approximation; 3. "sbj" Shale-Boylan-Johnston, see \code{\link[tsintermittent]{crost}}.
}
  \item{PlotIt}{
Plots the result
}
  \item{\dots}{
further optional parameters for \code{\link[tsintermittent]{crost}}
}
}
\details{

 The Croston method is suitable if demand appears at random, with many or even most time periods having no demand; where demand does occur, the historical data is randomly distributed, independently or almost independently of the demand interval. Such demand patterns are known as "lumpy demand" or intermittent, irregular, random or sporadic demand. The approach is  based on Croston's (1972) method for intermittent demand forecasting, also described in Shenstone and Hyndman (2005).
 
Croston's method involves using simple exponential smoothing (SES) on the non-zero elements of the time series and a separate application of SES to the times between non-zero elements of the time series. The smoothing parameters of the two applications of SES are assumed to be equal and are denoted by alpha. Optimisation of the types of croston methods is described in [Kourentzes, 2014].

}
\value{
List with
  \item{Forecast}{
[1:Horizont] numeric vector, forecast for test set times
}
  \item{Accuracy}{
Accuracy of TestSet against Forecast
}
  \item{TestSet}{
[Horizon+1:n] TestSet
}
  \item{Model}{
output of  \code{\link[tsintermittent]{crost}}
}
  \item{TrainingSet}{
[1:Horizon] Trainingset
}
}
\references{
Croston, J. (1972) "Forecasting and stock control for intermittent demands", Operational Research Quarterly, 23(3), 289-303.

Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying Croston's method for intermittent demand forecasting". Journal of Forecasting, 24, 389-402.

 N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. http://dx.doi.org/10.1016/j.ijpe.2014.06.007.
}
\author{
Michael Thrun
}


\seealso{
\code{\link[tsintermittent]{crost}},\code{\link[TSAT]{ConvertNumerical2TSobject}}
}
\examples{
data(Sales)
#set arbitrary dates
y=seq(from=as.Date('1970-01-01'),by='days',to=as.Date('1970-04-01'))[1:length(x)]
out=CrostonIntermittentDemand(Sales,y,Frequency = 'days',ForecastHorizon = 12,SplitAt=49,PlotIt = TRUE)
plot(out$TestSet,main='Plotting Testset Evaluation',xlab='Time',ylab='Sales')
points(time(out$TestSet),out$Forecast)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IntermittentDemand}% use one of  RShowDoc("KEYWORDS")
\keyword{Croston}% __ONLY ONE__ keyword per line
\keyword{Intermittent}% __ONLY ONE__ keyword per line
\keyword{Sales}% __ONLY ONE__ keyword per line
\name{RandomForestForecast}
\alias{RandomForestForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest for Forecast
}
\description{
RandomForestForecast
}
\usage{
RandomForestForecast(DF, formula = NULL, Horizon, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DF}{
dataframe
}
  \item{formula}{
either a formula describing praediktors or NULL
}
  \item{Horizon}{
forecast horizon
}
  \item{PlotIt}{
plots results
}
}
\details{
if NULL than autocorralation defined by Horizon is used as predictor 
}
\value{
List with
  \item{Forecast}{
vector
}
  \item{Model}{
output of  \code{\link[randomForest]{randomForest}}
}
  \item{Accuracy}{
output of  \code{\link[forecast]{accuracy}}
}
  \item{TestData}{
data.frame
}
  \item{TrainData}{
data.frame
}
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

}
\author{
Michael Thrun
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
 \code{\link[randomForest]{randomForest}}
 }
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{RandomForestForecast}% use one of  RShowDoc("KEYWORDS")
\keyword{randomForest }% __ONLY ONE__ keyword per line
\keyword{Forecast}
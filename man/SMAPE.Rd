\name{SMAPE}
\alias{SMAPE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symmetric mean absolute percentage error
}
\description{
Calculates the realtive difference between X (forecast) and y (historical data) [Armstrong,1985]. 
}
\usage{
SMAPE(X, Y, epsilon = 10^-10,na.rm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
either a value or numerical vector of [1:n]
}
  \item{Y}{
either a value or numerical vector of [1:n]
}
  \item{epsilon}{
Optional, If both x and y are approximatly zero the output is also zero
}
  \item{na.rm}{
Optional, function does not work with non finite values. If these cases should be automatically removed, set parameter TRUE
}
}
\details{
This function was taken from DatabionicSwarm::RelativeDifferences and slightly adjusted: 

The nominator is contrary to [Ultsch, 2008] in absolute values of \code{X} and \code{Y} resulting in the problem that SMAPE ist not symmetric regarding different forecasts since over- and under-forecasts are not treated equally (see example for further details). 

Contrary to other approaches in this cases the range of values lies between [-100,100] in percent. The approach is only valid for positive and negative values of\code{X} and \code{Y}.
The realtive difference \code{R} is defined with \deqn{SMAPE=100/n * \frac{abs(Y-X)}{(abs(X)+abs(Y))}}

Negative value indicate that \code{X}  is higher than \code{Y} and positive values that \code{X} is lower than \code{Y}.
}
\value{
SMAPE
}
\references{
[Ultsch,2008]    Ultsch, A.: Is Log Ratio a Good Value for Measuring Return in Stock Investments? GfKl 2008, pp, 505-511, 2008.
[Armstrong,1985]  Armstrong, J. S.: Long-range Forecasting: From Crystal Ball to Computer, 2nd. ed. Wiley, ISBN 978-0-471-82260-8, 1985.
}
\author{
Michael Thrun
}
\note{
Contrary to the relative differences, SMAPE is not symmetric.
}
\seealso{
DatabionicSwarm::RelativeDifferences
%\code{\link[DatabionicSwarm]{RelativeDifferences}}
}
\examples{
#usage with vectors, x forecast, y test data
x=c(100,110)
y=c(90,100)
SMAPE(x,y)

#example why it is not symmetric
SMAPE(x[1],y[1])

SMAPE(x[2],y[2])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{RelativeDifference}% use one of  RShowDoc("KEYWORDS")
\keyword{SMAPE}% use 
\keyword{difference}% __ONLY ONE__ keyword per line
\keyword{relative}% __ONLY ONE__ keyword per line
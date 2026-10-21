\name{TemporalForce}
\alias{TemporalForce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Temporal Relative Difference
}
\description{
Calculates the second order relative change force that represents the turning point of the timeseries
}
\usage{
TemporalForce(Data, na.rm = FALSE, Eps = 1e-04, Time, PlotIt = FALSE,      Silent = FALSE) 
}

\arguments{
  \item{Data}{
numerical matrix of [1:n,1:d], d>=1
}
  \item{na.rm}{
 FALSE; do nothing, TRUE: all nan to zero
}
  \item{Eps}{
Default: 0.0001, Force for which data is varying around eps is set to zero
}
  \item{PlotIt}{
 TRUE: plots data
}
  \item{Time}{
 [1:n] vector defining time and used for plotting, typically as.Date object
}  
\item{Silent}{
  TRUE: not warnings or messages
}
}
\details{
If vector is given as input, vector is given back. If matrix is given as input, PlotIt is forced to be set as FALSE.

The Force is defined as

Force=(Today-2*Yesterday+DayeforeYesterday)/Yesterday

}
\value{
 Force is either numerical matrix of [1:n,1:d] or numerical vector [1:n]. If Time is given and roce is a vector, the vector is named by the character conversion of the time given

}

\author{
Michael thruh
}

\seealso{
\code{\link{RelativeDifferenceBetweenTS}}

\code{\link{TemporalRelativeDifference}}
}
\examples{
data("ElectricityBRD")
V=TemporalForce(ElectricityBRD$Mrd_KWh,
PlotIt=TRUE,Time=(ElectricityBRD$Time))

V=TemporalForce(cbind(ElectricityBRD$Mrd_KWh,
ElectricityBRD$Mrd_KWh))
}
\references{
[Kulkarni and Hairdar, 2009]  Kulkarni, S. and Hairdar, I.: Forecasting Model for Crude Oil Price Using Artificial Neural Networks and Commoditiy Futures Prices, International Journal of Computer Science and Information Security (IJCSIS), Vol. 2(1), 2009.
}
\keyword{RelativeDifference}
\keyword{RelDiff}
\concept{relative difference }
\concept{temporal relative difference }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:

% \concept{ ~cpt2 }
% Use only one concept per line.

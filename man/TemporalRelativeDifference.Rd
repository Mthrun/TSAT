\name{TemporalRelativeDifference}
\alias{TemporalRelativeDifference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Temporal Relative Difference
}
\description{
Calculates the relative difference between positive Data and and lagged Data
}
\usage{
TemporalRelativeDifference(Data,Lag=1,

na.rm=FALSE,PlotIt=FALSE,Time,Silent=FALSE)
}

\arguments{
  \item{Data}{
numerical matrix of [1:n,1:d], d>=1
}
  \item{Lag}{
  The number of lags (in units of observations): back shifting: positiv value,forward shifting: negative value, default is the difference to yesterday
}
  \item{na.rm}{
 FALSE; do nothing, TRUE: all nan to zero
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
Contrary to linear filters in this cases the range of values lies always between [-2,2]

If vector is given as input, vector is given back. If matrix is given as input, PlotIt is forced to be set as FALSE.
}
\value{
 Reldiff numerical matrix of [1:n,1:d]. For example for \code{Lag=1}: negative value indicate that today is lower than yesterday and positive values that todays value is higher than yesterdays.

}

\author{
Michael thruh
}

\seealso{
\code{\link{RelativeDifferenceBetweenTS}}
}
\examples{
data(ElectricityBRD$Mrd_KWh)
V=TemporalRelativeDifference(ElectricityBRD$Mrd_KWh,
PlotIt=TRUE,Time=(ElectricityBRD$Time))

V=TemporalRelativeDifference(cbind(ElectricityBRD$Mrd_KWh,
ElectricityBRD$Mrd_KWh))
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

\name{FastFourierTransformation}
\alias{FastFourierTransformation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Fourier Transformation
}
\description{
FastFourierTransformation..
}
\usage{
FastFourierTransformation(Timeseries,na.rm=FALSE,PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Timeseries}{][1:n]  Zeitlich geordnete Werte ohne Zeitstempel als vector
}
  \item{na.rm}{  if TRUE removes automatically NaN by spline interpolation
}
  \item{PlotIt}{
}

}
\details{
...
}
\value{
OUTPUT list V with:
  \item{Frequenzspektrum}{
}
  \item{KomplexeFourierKoeffizienten}{
}
  \item{Amplitude}{
}
  \item{Data}{[1:4n] na removed and transformed data
  
}
}
\references{
temporal Data mining, Prof. Dr. Ultsch
}
\author{
Michael Thrun
}
\note{
trick17: data is mirrowed and doubled
}

\examples{
\dontrun{
FastFourierTransformation(timeseries,Periode=1,na.rm=FALSE,PlotIt=FALSE)
}
}
\keyword{FFT}% use one of  RShowDoc("KEYWORDS")
\keyword{Fourier Transformation}% __ONLY ONE__ keyword per line

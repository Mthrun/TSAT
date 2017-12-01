\name{FourierFilter}
\alias{FourierFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Low-pass or band-pass filter Filtering by Fourier
}
\description{
Filter time series by keeping only the largest or first of FFT coefficients.
}
\usage{
FourierFilter(Data, CountOrPercent=0.1, largest=TRUE,

AdaptAmp=FALSE, na.rm = TRUE,PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{[1:n]          real time series or complex fourier coefficients
}
  \item{CountOrPercent}{ if > 1 : keep largest n coefficients
                   if in [0:1] keep largest percentage of coefficients
                  default:  CountOrPercent=0.2  
}
  \item{largest}{
            if ==TRUE(default): keep largest coefficients (BandPass)
                  otherwise:       keep first   coefficients (LowPass)
}
  \item{AdaptAmp}{
           ==TRUE means amplitude and DC component of FilteredData is adapted to fit Data
                   default ==FALSE
}
  \item{na.rm}{
  if TRUE removes automatically NaN by spline interpolation
}
  \item{PlotIt}{
           ==TRUE plots Data versus Fit
                   default ==FALSE
}
}

\value{
 FilteredData[1:n]     FilteredData time series
}
\references{
Prof. Dr. Alfred Ultsch matlab code and temporal data mining, Prof. Dr. ultsch

}
\author{
Michael Thrun
}

\keyword{Fourier Filter}% use one of  RShowDoc("KEYWORDS")
\keyword{Low-pass filter}% __ONLY ONE__ keyword per line
\keyword{Band-pass filter}% __ONLY ONE__ keyword per line
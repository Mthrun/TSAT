\name{FourierAnalysis}
\alias{FourierAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fourier Analysis
}
\description{
Fourier Analysis with a window function equal to 4 times the time series
}
\usage{
FourierAnalysis(Y, SamplingRate, na.rm = FALSE, PlotIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
numerical vector [1:N] of time series
}
  \item{SamplingRate}{
Optional, used if specific frequencies should be given back instead of 1:Nqist frequency
}
  \item{na.rm}{
easy and maybe incorrect imputation of \code{\link{FastFourierTransformation}}
}
  \item{PlotIt}{
TRUE: Plots frequency domain
}
}
\details{
For discrete finite signal defined by a time series.
The DC part is not given back due to the choice of window. The first frequency is "artificial" because it results directly from the window function and thus is also nit given back.
}
\value{
A list with

\item{PowerSpectrum }{Description of 'comp1'}
\item{FFT }{see \code{\link{FastFourierTransformation}}}
\item{ggObject }{object of \code{\pkg{ggplot2}} of \code{PlotIT=TRUE}}
}
\references{
Lectures of MCT
}
\author{
Michael C. Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FastFourierTransformation}}
}
\examples{
data("ElectricityBRD")
#days,hours,mins,secs
SamplingRateInSecs=30.5*24*60*60
FA=FourierAnalysis(ElectricityBRD$Mrd_KWh,

SamplingRate=1/SamplingRateInSecs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FourierAnalysis}% use one of  RShowDoc("KEYWORDS")
\keyword{Fourier}% __ONLY ONE__ keyword per line

\name{WaveletFilter}
\alias{WaveletFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wavelet Filter
}
\description{
Filter time series by keeping only the largest or first n(#) DWT coefficients.
}
\usage{
WaveletFilter(TimeSeries,wavelet="haar",CountOrPercent=0,largest=FALSE, 

PlotIt=FALSE,Fancy=FALSE,

loess.span=0,lowerPeriod=16,upperPeriod=1024,dj = 1/250)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{
[1:n]   time series
}
  \item{wavelet}{
  only if fancy =FALSE can this be set
  
wavelet base to use: 

By default this is set to "la8", the Daubechies orthonormal compactly supported wavelet of length L=8 (Daubechies, 1992), least asymmetric family.

If fancy=TRUE, the Morlet wavelet is used
}

  \item{CountOrPercent}{
 keep largest n coefficients if > 1
 
                  keep largest n# is # <= 1
				  
				  does not work yet!
				  
}
  \item{largest}{
 if Fancv =FALSE, then boolean, whether to use largest or first, default is TRUE
}
  \item{PlotIt}{
 boolean, whether to plot results versus data, default is FALSE
}
  \item{Fancy}{
# If fancy =FALSE, use waveslim package, fast
# if fancy=TURE   use WaveletComp package, in that case only largest are kept, beware: its slow
}
  \item{loess.span}{
loess.span = 0 : if There is no need to detrend this series. 
}
  \item{lowerPeriod}{
This defines the range of periods used in the wavelet transformation.  Only periods of x within this range will be detected.  This range reaches from 16 = 2 4 to 128 = 2 7 .  To define period limits in terms of powers of 2 is convenient (see dj below), but no necessity. 
}
  \item{upperPeriod}{
This defines the range of periods used in the wavelet transformation.  Only periods of x within this range will be detected.  This range reaches from 16 = 2 4 to 128 = 2 7 .  To define period limits in terms of powers of 2 is convenient (see dj below), but no necessity. 

}
  \item{dj}{
 The  period  range  comprises  the  three  “octaves” [2 4 , 2 5 ) , [2 5 , 2 6 ) , [2 6 , 2 7 ) ,  to- gether with the value 2 7 .  Each octave is divided into 250 suboctaves with bounds given in vec- tor my.w$Period on a logarithmic scale.  In other words: my.w$Period has length 751, and all entries of the vector diff(log(my.w$Period)) are equal. Graphically, the argument dj thus determines the resolution along the period axis. }
}
\details{
uses \code{dwt} and \code{idwt} of CRAN package \code{waveslim}

or

uses \code{analyze.wavelet} and \code{reconstruct} of CRAN package \code{WaveletComp}

The the depth of the decomposition cannot be specifieed yet.(This will have to be be a number less than or equal to log(length(x),2).)

}
\value{
List V with

 V$FilteredTS[1:n]                      filtered time series
 
 if fancy FALSE
 
	V$WaveletDecompositionVector[1:n]        wavelet coefficients
 
	V$BookkeepingVector[1:n]               Scaling coefficient vector.
 
 IF Fancy TRUE
 
	V$WaveletDecompositionVector[1:n]        result of \code{analyze.wavelet}
  
	V$BookkeepingVector[1:n]              result of \code{reconstruct}
 
}
\references{
Daubechies, I. (1992) Ten Lectures on Wavelets, CBMS-NSF Regional Conference Series in Applied Mathematics, SIAM: Philadelphia.

Gencay, R., F. Selcuk and B. Whitcher (2001) An Introduction to Wavelets and Other Filtering Methods in Finance and Economics, Academic Press.

Mallat, S. G. (1989) A theory for multiresolution signal decomposition: the wavelet representation, IEEE Transactions on Pattern Analysis and Machine Intelligence, 11, No. 7, 674-693.

Percival, D. B. and A. T. Walden (2000) Wavelet Methods for Time Series Analysis, Cambridge University Press.
}
\author{
Michael Thrun
}
\note{
Droping coeeficients does not work yet

for fancy=TRUE see
\url{http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\keyword{wavelet}% use one of  RShowDoc("KEYWORDS")
\keyword{Filter}% __ONLY ONE__ keyword per line
\keyword{low-pass}% __ONLY ONE__ keyword per line
\keyword{high-pass}% __ONLY ONE__ keyword per line
\keyword{time series}% __ONLY ONE__ keyword per line
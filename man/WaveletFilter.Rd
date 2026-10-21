\name{WaveletFilter}
\alias{WaveletFilter}
\title{
WaveletFilter
}
\description{
The wavelet filter decomposes a signal in its redundant wavelet decomposition
with a wavelet of choice \code{\link[wavelets]{dwt}}.
The wavelet levels can be filtered all together or individually.
The filter can be computed with hard and soft thresholding or with zeroing.
The wavelet decomposition is then reconstructed.
Frequency bands are controlled by the number of decomposition levels.
There more levels are computed, the more fine granular the choices can be.
}
\usage{
WaveletFilter(Data, Filter="haar", NumLevels=2, Boundary="periodic",
Fast=TRUE, PlotIt=FALSE, Threshold="zero", Lambda=0.05, FilterLevels="all")
}
\arguments{
\item{Data}{
Signal filtered with wavelets
}
\item{Filter}{
Character containing name of wavelet
}
\item{NumLevels}{
Integer determining the number of wavelet levels to create.
}
\item{Boundary}{
A character string indicating which boundary method to use.
boundary = "periodic" and boundary = "reflection" are the only supported methods
at this time.
}
\item{Fast}{
A logical flag which, if true, indicates that the pyramid algorithm is computed
with an internal C function. Otherwise, only R code is used in all computations.
}
\item{PlotIt}{
Boolean flag. If TRUE then plots will be generated. Default=F.
}
\item{Threshold}{
Character with filtering strategy. Default: Threshold="zero".
Threshold="zero": sets complete level to zero.
Threshold="hard": performs hard thresholding.
Threshold="soft": performs soft thresholding.
}
\item{Lambda}{
Numeric value. If filtering uses hard or soft thresholding, Lambda indicates the
p-th percentile which is used as threshold. Every value in level below this is
zero (both for hard and soft thresholding). Values greater or equal are treated
accordingly to soft/hard thresholding.
}
\item{FilterLevels}{
Generic: FilterLevels="all" character: chooses all wavelet levels for filtering
FilterLevels[1:n] numeric vector: chooses all wavelet levels which are given in
vector.
}
}
\value{
FilteredData[1:n]    Signal filtered with wavelets
}
\references{
https://cran.r-project.org/web/packages/wavelets/index.html
}
\author{
Q. Stier
}
\examples{
# Load package data; do not read a file from the working directory.
data("TempMelbourneAustralia")
Data = TempMelbourneAustralia$Temp
WaveletFilter(Data, Filter="haar", NumLevels=2, Boundary="periodic", Fast=TRUE,
PlotIt=FALSE, Threshold="zero", Lambda=0.05, FilterLevels="all")

}
\keyword{wavelet}% use one of  RShowDoc("KEYWORDS")
\keyword{waveletfilter}% __ONLY ONE__ keyword per line
\keyword{waveletanalysis}


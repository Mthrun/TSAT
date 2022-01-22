\name{WaveletOutlierDetection}
\alias{WaveletOutlierDetection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WaveletOutlierDetection
}
\description{
Outlier detection based on a wavelet filter for signals.
First, the signal is filtered.
Second, the difference between signal and filtered signal is measured.
If the original signal lies above the filtered signal by a factor of 1.5
(Default), it is recognized as outlier.
Technical details: detection of outliers is pointwise.
Each point of the signal is checked against the filtered signal.
This can be done in two ways:
1. compare data point against moving average (Default)
2. compare data point against data point of filtered signal.
}
\usage{
WaveletOutlierDetection(Data, Factor = 1.5, Strategy="mean",
Order = 7, Filter="haar", NumLevels=2, Boundary="periodic", Fast=T, PlotIt=F,
Threshold="zero", Lambda=0.05, FilterLevels="all")
}
\arguments{
\item{Data}{
Signal filtered with wavelets
}
\item{Factor}{
Numeric defining the factor by which a data point needs to exceed the data in
order to be recognized as outlier.
}
\item{Strategy}{
Character defining the strategy by which single data points compared to data.
Either, a data point is compared to the mean of the filtered data over a certain
time range (moving average which time interval is defined by argument Order)
around the data point or data points are compared to the filtered data points at
their respective time.
}
\item{Order}{
Integer defining the window size computed for the moving average.
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
load("TempMelbourneAustralia")
VarO1 = TempMelbourneAustralia$Temp
VarI1 = forecast::na.interp(VarO1)
Outliers = WaveletOutlierDetection(VarI1, Order = 7)
#plot(Outliers, col = "red", ylim = c(0,10))
#lines(VarI1, col = "green")

}
\keyword{wavelet}% use one of  RShowDoc("KEYWORDS")
\keyword{waveletfilter}% __ONLY ONE__ keyword per line
\keyword{waveletanalysis}


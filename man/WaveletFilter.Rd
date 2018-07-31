\name{WaveletFilter}
\alias{WaveletFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WaveletFilter
}
\description{
WaveletFilter plots timeseries and a reconstruction of this timeseries based on a basis, which is chosen from a wavelet decomposition.
The choice is dependent on a certain filter depth, represented as percentage. The depth is based on a dyadic partitioning because of the way of the algorithm.
Possible filtersteps are: 50, 25, 12.5, 6.25 percents.
Parameters are timeseries, wavelet, percent and plot
timeseries is Data for analyzing
wavelet is String desired kind of wavelet
percent is String the desired filtering depth
plot is Boolean for plotting. Create plot if TRUE. Default value is FALSE
Example: WaveletFilter(timeseries, "haar", "12.5", TRUE)
}
\usage{
WaveletFilter(timeSeries, wavelet = "d6", level = 1, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeSeries}{
%%     ~~Describe \code{x} here~~
}
  \item{wavelet}{
%%     ~~Describe \code{x} here~~
}
  \item{level}{
%%     ~~Describe \code{x} here~~
}
  \item{plot}{
%%     ~~Describe \code{x} here~~
}
}
\details{
to be filled
}
\value{
FilteredData
}
\references{
https://cran.r-project.org/web/packages/waveslim/index.html
}
\author{
Q. Stier
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

library("waveslim")
library("WaveletComp")
x = periodic.series(start.period = 50, length = 128)
x = x + 0.2 * rnorm(128)  # add some noise

WaveletFilter(x, level=2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{wavelet}% use one of  RShowDoc("KEYWORDS")
\keyword{waveletfilter}% __ONLY ONE__ keyword per line
\keyword{waveletanalysis}
\keyword{waveslim}

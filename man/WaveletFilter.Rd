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
WaveletFilter(timeseries, wavelet, percent, plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
x = periodic.series(start.period = 50, length = 1024)
x = x + 0.2 * rnorm(1024)  # add some noise

WaveletFilter(x, percent = "25")

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{wavelet}% use one of  RShowDoc("KEYWORDS")
\keyword{waveletfilter}% __ONLY ONE__ keyword per line
\keyword{waveletanalysis}
\keyword{waveslim}

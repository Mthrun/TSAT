\name{TSDistancesMatrix}
\alias{TSDistancesMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance Matrix for Time Series
}
\description{
Generates a Distance Matrix for Time Series data
}
\usage{
TSDistancesMatrix(TSMatrix,method='euclidean')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TSMatrix}{
[1:n,1:m] matrix of m different Time Series with equal length n
}
  \item{method}{
Distance measure to be used. It must be one of: "euclidean", "manhattan", "minkowski", "infnorm", "ccor", "sts", "dtw", "keogh.lb", "edr", "erp", "lcss", "fourier", "tquest", "dissim", "acf", "pacf", "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", "ar.pic", "cdm", "cid", "cor", "cort", "wav", "int.per", "per", "mindist.sax", "ncd", "pred", "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet"
}
}
\details{
"Shapebased methods [e.g. Euclidean, DTW] compare the overall appearance of the time series. Feature-based methods extract
features that usually describing time independent aspects of the series that are compared with
static distance functions. Model-based methods fit a model to the data and measure the similarity
by comparing the models. Compression-based methods [e.g. cdm] analyze how well two time series can
be compressed alone and together." [Mörchen, 2006]

"For long time series of possibly very different lengths, the shape-based methods do not
give intuitive results, feature- [e.g. fourier] and model-based [e.g. acf] methods  should be considered" [Mörchen, 2006]


For Distances measures description see \pkg{TSdist} CRAN package (here cited below):
\itemize{
\item "euclidean": Euclidean distance. 
\item "manhattan": Manhattan distance. 
\item "minkowski": Minkowski distance. 
\item "infnorm": Infinite norm distance. 
\item "ccor": Distance based on the cross-correlation. 
\item "sts": Short time series distance. 
\item "dtw": Dynamic Time Warping distance. . Uses the \pkg{dtw} package 
\item "lb.keogh": LB_Keogh lower bound for the Dynamic Time Warping distance.
\item "edr": Edit distance for real sequences. 
\item "erp": Edit distance with real penalty. 
\item "lcss": Longest Common Subsequence Matching. 
\item "fourier": Distance based on the Fourier Discrete Transform.
\item "tquest": TQuest distance.
\item "dissim": Dissim distance. 
\item "acf": Autocorrelation-based dissimilarity . Uses the \pkg{TSclust} package 
\item "pacf": Partial autocorrelation-based dissimilarity. Uses the \pkg{TSclust} package
\item "ar.lpc.ceps": Dissimilarity based on LPC cepstral coefficients. Uses the \pkg{TSclust} package
\item "ar.mah": Model-based dissimilarity proposed by Maharaj (1996, 2000). Uses the \pkg{TSclust} package 
\item "ar.pic": Model-based dissimilarity measure proposed by Piccolo (1990)Uses the \pkg{TSclust} package 
\item "cdm": Compression-based dissimilarity measure . Uses the \pkg{TSclust} 
\item "cid": Complexity-invariant distance measure. Uses the \pkg{TSclust} package (see 
\item "cor": Dissimilarities based on Pearson's correlation . Uses the \pkg{TSclust} package 
\item "cort": Dissimilarity index which combines temporal correlation and raw value
behavior. Uses the \pkg{TSclust} package 
\item "wav": Dissimilarity based on wavelet feature extraction . Uses the \pkg{TSclust} package
\item "int.per": Integrated periodogram based dissimilarity. Uses the \pkg{TSclust} package
\item "per": Periodogram based dissimilarity Uses the \pkg{TSclust} package 
\item "mindist.sax": Symbolic Aggregate Aproximation based dissimilarity. Uses the \pkg{TSclust} package 
\item "ncd": Normalized compression based distance . Uses the \pkg{TSclust} package
\item "pred": Dissimilarity measure cased on nonparametric forecasts Uses the \pkg{TSclust} package
\item "spec.glk": Dissimilarity based on the generalized likelihood ratio test Uses the \pkg{TSclust} package
\item "spec.isd": Dissimilarity based on the integrated squared difference between the log-spectr Uses the \pkg{TSclust} package
\item "spec.llr": General spectral dissimilarity measure using local-linear estimation of the log-spectr Uses the \pkg{TSclust} package 
\item "pdc": Permutation Distribution Distance. Uses the \pkg{pdc} package.
\item "frechet": Frechet distance. Uses the \pkg{longitudinalData} package.
}
}
\value{
DistanceMatrix[1:m,1:m]
}

\references{
[Moerchen, 2006]	Moerchen, Fabian. Time series knowledge mining. Goerich & Weiershaeuser, 2006.
}
\author{
Michael Thrun
}

\keyword{time series}% use one of  RShowDoc("KEYWORDS")
\keyword{data mining}% __ONLY ONE__ keyword per line
\keyword{distance}% __ONLY ONE__ keyword per line
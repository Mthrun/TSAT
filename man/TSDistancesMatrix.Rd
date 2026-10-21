\name{TSDistancesMatrix}
\alias{TSDistancesMatrix}
\title{Distance Matrix for Time Series}
\description{
Calculate distances between time series of equal length. Use
\pkg{BIDistances} where possible. Use other CRAN packages for additional
methods. Check the required package before each call.
}
\usage{
TSDistancesMatrix(TSMatrix, method = "euclidean", ..., m = NULL)
}
\arguments{
  \item{TSMatrix}{Numeric matrix with observations in rows and series in
  columns. Supply at least two rows and one column. All values must be finite.
  A numeric data frame is accepted. A numeric vector is treated as one series.
  Row order gives the time order. Observations must have equal time intervals.}
  \item{method}{One method name from the list below. Names are not case
  sensitive. The default is \code{"euclidean"}. The old spelling
  \code{"keogh.lb"} is an alias for \code{"lb.keogh"}.}
  \item{...}{Named parameters for the selected method. Each name must be
  unique. Do not supply data arguments here. See the parameter list below.}
  \item{m}{Embedding dimension for \code{pdc}. Use the full name \code{m =}
  when you supply it. The default \code{NULL} selects the dimension
  automatically. This argument is separate so R does not match its name
  to \code{method}. Other methods do not accept it.}
}
\details{
\strong{Methods from current CRAN packages}
\describe{
  \item{\pkg{BIDistances}}{
  \code{euclidean}: Euclidean distance.
  \code{manhattan}: sum of absolute differences.
  \code{minkowski}: Minkowski distance with exponent \code{p}.
  \code{infnorm}: maximum absolute difference.
  \code{dtw}: dynamic time warping through \code{DTW_Distance}.
  \code{cor}: \eqn{\sqrt{2(1-r)}}, where \eqn{r} is Pearson correlation.
  \code{sts}: Euclidean distance between successive differences.
  \code{fourier}: Euclidean distance between complex Fourier coefficients.}
  \item{\pkg{dtwclust}}{
  \code{lb.keogh}: LB Keogh lower bound with the L2 norm.}
  \item{\pkg{SimilarityMeasures}}{
  \code{edr}: edit distance for real sequences.
  \code{lcss}: one minus the longest common subsequence match count divided
  by the number of observations. No translation search is used.}
  \item{\pkg{jmotif}}{
  \code{mindist.sax}: SAX lower bound. Each series is centered and divided
  by its sample standard deviation. Piecewise aggregate approximation and
  Gaussian breakpoints give the symbols. The squared lookup distances are
  summed, multiplied by \code{nrow(TSMatrix) / w}, and square rooted.}
  \item{\pkg{pdc}}{
  \code{pdc}: permutation distribution distance through \code{pdcDist}.
  The embedding is selected for each pair when parameters are omitted.}
  \item{\pkg{FKmL}}{
  \code{frechet}: discrete maximum Frechet distance between curves with
  coordinates \code{cbind(seq_len(n) * timeScale, values)}.}
  \item{\pkg{stats}}{
  \code{ccor}: cross-correlation distance. This uses the square root of
  \eqn{(1-r_0^2)/\sum_{k<0}r_k^2}. The squared zero-lag correlation is
  rounded to five decimal places, as in the former implementation.}
  \item{\pkg{dtw}}{
  \code{tam}: temporal alignment measure. The DTW path gives the number
  of horizontal, vertical, and diagonal steps. For series length \eqn{n},
  the result is \eqn{1+(H+V-D)/(n-1)}.}
}

\strong{Optional TSclust methods}

The following methods call the corresponding exported function in
\pkg{TSclust}. CRAN archived TSclust on 2026-01-16. These methods work only
when its namespace can be loaded. If it cannot be loaded, the function stops
with a message that identifies the method, package, and archive status.
The other methods do not require TSclust or TSdist.
\describe{
  \item{\code{acf}, \code{pacf}}{Autocorrelation and partial
  autocorrelation distances: \code{diss.ACF} and \code{diss.PACF}.}
  \item{\code{ar.lpc.ceps}}{LPC cepstral distance: \code{diss.AR.LPC.CEPS}.}
  \item{\code{ar.pic}}{Piccolo autoregressive distance: \code{diss.AR.PIC}.}
  \item{\code{ar.mah}, \code{ar.mah.statistic}, \code{ar.mah.pvalue}}{
  Maharaj comparison: \code{diss.AR.MAH}. The first two names return the
  test statistic. The last name returns the p-value.}
  \item{\code{cdm}, \code{ncd}}{Compression distances:
  \code{diss.CDM} and \code{diss.NCD}.}
  \item{\code{cid}}{Complexity-invariant distance: \code{diss.CID}.}
  \item{\code{cort}}{Temporal correlation and value distance:
  \code{diss.CORT}.}
  \item{\code{int.per}, \code{per}}{Integrated periodogram and periodogram
  distances: \code{diss.INT.PER} and \code{diss.PER}.}
  \item{\code{pred}}{Distance between nonparametric forecasts:
  the \code{L1dist} result from \code{diss.PRED}.}
  \item{\code{spec.glk}, \code{spec.isd}, \code{spec.llr}}{
  Spectral comparisons: \code{diss.SPEC.GLK}, \code{diss.SPEC.ISD}, and
  \code{diss.SPEC.LLR}. These use a generalized likelihood ratio,
  integrated squared log-spectrum difference, and local linear
  log-spectrum estimate, respectively.}
}

\strong{Parameters}
\describe{
  \item{\code{p}}{For \code{minkowski}. A number at least one. Default: 2.
  \code{Inf} selects the maximum distance.}
  \item{\code{threads}}{For \code{euclidean}, \code{manhattan},
  \code{minkowski}, and \code{infnorm}. Passed to BIDistances.}
  \item{\code{DistanceFunction}, \code{DistanceFunctionMethod}}{For
  \code{dtw}. Configure the local distance function used by BIDistances.
  Further parameters are passed to that function. These parameters do not
  set the DTW path or alignment window. The default uses Euclidean local
  distances and the default \pkg{dtw} alignment.}
  \item{\code{beta}}{For \code{cor}. Optional finite number at least zero.
  Changes the result to \eqn{\sqrt{((1-r)/(1+r))^{\beta}}}.
  Constant series and other undefined results cause an error.}
  \item{\code{n}}{For \code{fourier}. Number of leading Fourier
  coefficients, including the constant coefficient. Default:
  \code{floor(nrow(TSMatrix) / 2) + 1}. Maximum: the number of rows.
  For \code{spec.isd}, this instead selects the TSclust integration mode.
  The default is 0 for direct integration. Positive values use TSclust
  interpolation, which can return missing values.}
  \item{\code{window.size}}{Required for \code{lb.keogh}. Integer window
  radius from 1 to \code{nrow(TSMatrix) - 1}. This is the dtwclust radius,
  not the full window width used by the former TSdist implementation.}
  \item{\code{epsilon}}{Required for \code{edr} and \code{lcss}.
  Finite nonnegative threshold for a value match. EDR has no time window.}
  \item{\code{sigma}}{For \code{lcss}. Largest permitted index difference
  for a match. Integer from 0 to \code{nrow(TSMatrix) - 1}.
  Default: \code{nrow(TSMatrix) - 1}, which allows all index pairs.}
  \item{\code{w}, \code{alpha}}{For \code{mindist.sax}. Supply \code{w},
  the number of aggregate values, from 1 to the number of rows.
  \code{alpha} is the alphabet size from 2 to 20. Default: 4.
  Constant series are not accepted.}
  \item{\code{m}, \code{t}}{For \code{pdc}. Embedding dimension and time
  delay. Passed to \code{pdcDist}. Both default to automatic selection.}
  \item{\code{timeScale}}{For \code{frechet}. Finite nonnegative time
  coordinate scale. Default: 0.1, as in the former implementation.}
  \item{\code{lag.max}}{For \code{ccor}. Largest correlation lag, from 1
  to \code{nrow(TSMatrix) - 1}. Default: \code{nrow(TSMatrix) - 1}.}
  \item{Other TSclust parameters}{Passed to the selected TSclust function.
  For example, \code{pred} requires forecast horizon \code{h}. Parameter
  \code{B} sets its bootstrap count. Use \code{set.seed} for repeatable
  results. See the help for the selected TSclust function.}
}

\strong{Changes from the former TSdist implementation}

TSdist is no longer called. There is no verified replacement for
\code{erp}, \code{tquest}, or \code{dissim} in the selected current CRAN
packages or TSclust. These names cause an explicit error. The old help
also listed \code{wav}, but TSdist did not support it. TSclust has disabled
\code{diss.DWT}. Thus, \code{wav} also causes an explicit error.

The former LCSS result was a match count. The new LCSS result is a distance
from zero to one. A full match gives zero. SAX uses rounded lookup values
from jmotif. Results can differ slightly from TSclust values.
The former matrix construction assigned some lower triangle
values to the wrong pairs when there were four or more series. Each pair
now has the correct matrix position.
}
\value{
A numeric matrix with one row and one column per input series. Input column
names label both matrix dimensions. The diagonal is zero, except for
\code{ar.mah.pvalue}, where it is one. P-values are not distances; a large
p-value does not prove that two series have the same model.

For \code{lb.keogh}, \code{ccor}, \code{cdm}, \code{ncd}, and \code{tam},
entry \code{[i, j]} compares series \code{i} to series \code{j} in that
order. These results can be asymmetric. Do not convert such a matrix to
\code{dist} unless you have selected an explicit rule to combine the two
directions. Other results are symmetric. Not all methods satisfy the
triangle inequality. An undefined, infinite, or negative result causes
an error.
}
\references{
M\"orchen, F. (2006). Time Series Knowledge Mining.
G\"orich & Weiersh\"auser.

\url{https://CRAN.R-project.org/package=TSclust}
}
\author{Michael Thrun}
\examples{
series <- cbind(a = c(1, 2, 4, 3), b = c(2, 4, 3, 5),
                c = c(4, 3, 2, 1), d = c(1, 3, 5, 7))
if (requireNamespace("BIDistances", quietly = TRUE)) {
  distances <- TSDistancesMatrix(series)
  stopifnot(isTRUE(all.equal(distances, as.matrix(dist(t(series))))))
  TSDistancesMatrix(series, "minkowski", p = 3)
}
if (requireNamespace("SimilarityMeasures", quietly = TRUE)) {
  TSDistancesMatrix(series, "lcss", epsilon = 0.5, sigma = 1)
}
}
\keyword{ts}
\keyword{multivariate}

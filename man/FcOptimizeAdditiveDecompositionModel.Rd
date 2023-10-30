\name{FcOptimizeAdditiveDecompositionModel}
\alias{FcOptimizeAdditiveDecompositionModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with optimized Additive Decomposition Model function using \code{\link[optimx]{opm}}
}
\description{
The optimization is based on \code{L-BFGS-B} for the three parameters alpha beta and gamma of \code{\link{FcAdditiveDecompositionModel}}.
}
\usage{
FcOptimizeAdditiveDecompositionModel(Data, SeasonalLength = 52,

Horizon = 12, Iterations = 100, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n] numerical vector
}
  \item{SeasonalLength}{
see \code{\link{FcAdditiveDecompositionModel}}
}
  \item{Horizon}{
horizon of forecast, autmaticall splits \code{Data} into train and test set
}
  \item{Iterations}{
number of iteratons for the \code{\link[optimx]{opm}} procedure, because it depends on the initial choice of \code{AlphaBetaGamma}.
}
  \item{PlotIt}{
If TRUE, time series and forecast is ploted.
}
}
\details{
Limited-memory BFGS is an optimization algorithm in the family of quasi-Newton methods that approximates the Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm using a limited amount of computer memory. The object function is defined by \code{\link{RootDeviance}} where \code{MRD} is minimized and the initial choice of \code{AlphaBetaGamma} is random. By selecting \code{Iterations} higher than 1 a simulated annealing procedure is used.
After this procudure the optimized parameters \code{AlphaBetaGamma} can be used in \code{\link{FcAdditiveDecompositionModel}} to forecast the future.
}
\value{

  \item{Forecast}{
[1:k] forecast, where \code{Horizon=n-k}
}
  \item{Accurary}{
against the test set, see \code{\link[forecast]{accuracy}}
}
  \item{Train}{
[1:m] trainings data with \code{m+k=n}.
}
  \item{ForecastTrain}{
[1:m] forecast fo trainings data
}
  \item{Test}{
[1:k] testset
}
  \item{AlphaBetaGamma}{
parameters of \code{\link{FcAdditiveDecompositionModel}}
}
}
\references{
Nash JC, and Varadhan R (2011). Unifying Optimization Algorithms to Aid Software System Users: optimx for R., Journal of Statistical Software, 43(9), 1-14., URL http://www.jstatsoft.org/v43/i09/.
}
\author{
Michael Thrun
}
\note{
The optimization of \code{\link[optimx]{opm}} contrary to parameter settings sometimes tries values above 1 or less than zero. This generates warnings, but is catched in \code{\link{FcAdditiveDecompositionModel}}.

}

\seealso{
\code{\link[optimx]{opm}}
}
\examples{
#ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{OptimizeAdditiveDecompositionModel}% use one of  RShowDoc("KEYWORDS")
\concept{time series}
\concept{Additive decomposition model}
\concept{Optimize additive decomposition model}

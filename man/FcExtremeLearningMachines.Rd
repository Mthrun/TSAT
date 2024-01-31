\name{FcExtremeLearningMachines}
\alias{FcExtremeLearningMachines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Extreme Learning Machines
}
\description{
Special case for a multilayer perceptrone feed forward network with backpropagation used for forecasting [Huang et al., 2005] 
}
\usage{
FcExtremeLearningMachines(DataVec, Time, SplitAt,

Predictors, ForecastHorizon, No_HiddenLayers = NULL,

Scaled = TRUE, No_TrainingNetworks = 10,

PlotEvaluation = FALSE, PlotIt = FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataVec}{
[1:n] numerical vector of time series data
}
  \item{Time}{
[1:n] character vector of Time in the length of data
}
  \item{SplitAt}{
Scalar 'k' with k<n, index of row where the \code{DataFrame} is divided into test and train data
}
  \item{Predictors}{
[1:n,1:d] data frame or matrix of d predictores with n values each
}
  \item{No_HiddenLayers}{
Number of hidden layers, see \code{\link[nnfor]{elm}}
}
  \item{Scaled}{
Should the data and regressors be scaled before applying the model?
}
  \item{No_TrainingNetworks}{
Number of Training Networks, see \code{\link[nnfor]{elm}}
}
  \item{PlotEvaluation}{
Plot output of training data regarding and network architecture
}
  \item{PlotIt}{
Simple plot to compare forecasting of future
}
  \item{\dots}{
Further arguments passed on to \code{\link[nnfor]{elm}}
}
}
\details{
The parameters of hidden nodes (not just the weights connecting inputs to hidden nodes) need not be tuned. These hidden nodes can be randomly assigned and never updated (i.e. they are random projection but with nonlinear transforms), or can be inherited from their ancestors without being changed. In most cases, the output weights of hidden nodes are usually learned in a single step. According to their creators, these models are able to produce good generalization performance and learn thousands of times faster than networks trained using backpropagation [Huang et al., 2005].
}
\value{
 \item{Model}{
Model paramters, see example
}
  \item{Forecast}{
Forecast, see example
}
  \item{TrainingData}{
TrainingData, see example
}
  \item{TestData}{
TestData, see example
}
 
  \item{Accuracy}{
ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix
}
}
\references{
[Huang et al., 2005]  Huang, Guang-Bin; Zhu, Qin-Yu; Siew, Chee-Kheong: "Extreme learning machine: theory and applications". Neurocomputing, Vol. 70 (1), pp. 489–501, 2005
}
\author{
Michael Thrun
}
\note{
For an introduction to neural networks see: Ord K., Fildes R., Kourentzes N. (2017) Principles of Business Forecasting 2e. Wessex Press Publishing Co., Chapter 10.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[nnfor]{elm}}
}
\examples{
\donttest{
requireNamespace('ggfortify')
x=fortify(datasets::sunspot.month)
x=ggfortify::fortify(datasets::sunspot.month)
#Example for a bad forecast
results=FcExtremeLearningMachines(DataVector = x$Data,Time = x$Index, SplitAt=2800)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ELM}% use one of  RShowDoc("KEYWORDS")
\keyword{ExtremeLearningMachines}% __ONLY ONE__ keyword per line
\keyword{ANN}
\keyword{TS}
\concept{time series}
\concept{Extreme learning machines}
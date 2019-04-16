\name{ExtremeLearningMachines}
\alias{ExtremeLearningMachines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extreme Learning Machines
}
\description{
 Special case for a multilayer perceptrone feed forward network with backpropagation used for forecasting [Huang et al., 2005] 
}
\usage{
ExtremeLearningMachines(DataVector, Time, SplitDataAt = 10,

FirstPredictor, SecondPredictor,  No_HiddenLayers = NULL,

Scaled = TRUE, No_TrainingNetworks = 10,

PlotEvaluation = FALSE, PlotFuture = TRUE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataVector}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{Time}{
[1:n] time vector
}
  \item{FirstPredictor}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{SecondPredictor}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{SplitDataAt}{
scalar 'k' with k<n, index of row where the \code{DataFrame} is divided into test and train data
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
plot output of training data regarding and netowrk architecture
}
  \item{PlotFuture}{
simple plot to compare forecastingof future
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
model paramters, see example
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
x=ggfortify::fortify(datasets::sunspot.month)
#Example for a bad forecast
results=ExtremeLearningMachines(DataVector = x$Data,Time = x$Index, SplitDataAt=2800)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ELM}% use one of  RShowDoc("KEYWORDS")
\keyword{time series}% __ONLY ONE__ keyword per line
\keyword{ExtremeLearningMachines}% __ONLY ONE__ keyword per line
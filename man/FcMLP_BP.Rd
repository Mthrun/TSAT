\name{FcMLP_BP}
\alias{FcMLP_BP}

\title{
Forecasting with Multilayer Perceptron Feedforward Network with Backpropagation
}
\description{
Foracasting by multilayer perceptron feedforward network with resilient backpropagation with weight backtracking.
}
\usage{
FcMLP_BP(Response, SplitAt, Predictor1,

Predictor2, Time, HiddenLayers = 4, Threshold = 0.05, Repetitions = 10,

ErrorFunction = "sse", PlotEvaluation = FALSE, PlotIt = FALSE)
}

\arguments{
    \item{Response}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{SplitAt}{
Index of row k where the DataVec is divided into test and train data. If not given n is used
}
  \item{Predictor1}{
[1:n] vector with an value of each time j in [1,n]
}
  \item{Predictor2}{
[1:n] vector with an value of each time j in [1,n]
}

  \item{Time}{
[1:n] character vector of Time in the length of data
}
  \item{HiddenLayers}{
Number of hidden layers, see \code{\link[neuralnet]{neuralnet}
}}
  \item{Threshold}{
Threshold for the partial derivatives of \code{ErrorFunction} , please see \code{\link[neuralnet]{neuralnet}}
}
  \item{Repetitions}{
Number of repetitions for the neural network's training.
}
  \item{ErrorFunction}{
Differentiable function that is used for the calculation of the error, string alternatives are 'ce' and 'sse', please see \code{\link[neuralnet]{neuralnet}}
}
  \item{PlotEvaluation}{
Plot output of training for \code{neuralnet}
}
  \item{PlotFuture}{
A logical determining whether or not to plot the forecast in comparison to the validation set.
}
}
\details{
This functions trains a MLP/BP network and then forecasts a new sample of data using the trained ANN. Training and Testdata are splitted up from the arguments using \code{ForecastHorizont}. Testing data is only used to compare against the forecast.

Seems to be good for longterm forcasting. Short term forcasting does not work well.
}
\value{
list with
\item{Forecast}{[k:n], the forecast, of the time interval [k,n] which was not used in the model}
\item{TestSet}{[k:n,1], the part of Response not used in the model}
\item{TrainData}{[1:k,1:3], the part of Response not used in the model}
\item{TestDataOutput}{[output of \code{compute}}
\item{Accuracy}{ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix}
\item{Model}{Output of \code{neuralnet}}

}
\references{
Riedmiller M. and Braun H.: A direct adaptive method for faster backpropagation learning: The RPROP algorithm. Proceedings of the IEEE International Conference on Neural Networks (ICNN), pages 586-591. San Francisco, 1993.
}
\author{
Michael Thrun
}
\note{
This version requires currently two predictors.
}

\seealso{
\code{\link[neuralnet]{neuralnet}}, \code{\link[neuralnet]{compute}}
}
\examples{
#no open acces data available with two predcitors
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ MLP }% use one of  RShowDoc("KEYWORDS")
\keyword{ BP}% __ONLY ONE__ keyword per line
\keyword{ANN}
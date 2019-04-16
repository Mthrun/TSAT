\name{VARIMA}
\alias{VARIMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vector autoregessive moving average model with difference filter
}
\description{
VARMA and VARIMA modelling for multivariate Forecasting
}
\usage{
VARIMA(Data, ARp, i = 1, MAq, ForecastHorizont = 14, PlotIt = TRUE, Time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
matrix [1:n,1:d]
}
  \item{ARp}{
numerical value, see example
}
  \item{i}{
numerical value, either zero: weakly stationary time series or 1 for non stationary
}
  \item{MAq}{
numerical value, see example
}
  \item{ForecastHorizont}{
scalar 'f', forcasting period
}
  \item{PlotIt}{
TRUE: Evaluation plots
}
  \item{Time}{
Optional, for evaluation plots
}
}
\details{
Please read [Tsay, 2013].
}
\value{
List V with
\item{Model}{List with

Model: Model output of \code{\link[MTS]{VARMA}}

OptimizedModel: Further optimized Model of Model using \code{\link[MTS]{refVARMA}}

}
\item{Train}{[1:(N-f),1:d] Training data for building the model}
\item{Test}{[(N-f+1):N,1:d] Evaluation Data of the Model}
\item{Forecast}{[(N-f+1):N,1:d] Prediction of the Model}

}
\references{
[Tsay, 2013]  Tsay, R. S.: Multivariate time series analysis: with R and financial applications, John Wiley & Sons, ISBN: 978-1-118-61790-8, 2013.
}
\author{
Michael Thrun
}
\note{
Wrapper for VARMA
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[MTS]{VARMA}}
}
\examples{

\donttest{
#Defines p and q
MTS::Eccm(Data)

Forecast=VARIMA(Data,p,0,q)#If weakly stationary

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{VARMA}% use one of  RShowDoc("KEYWORDS")
\keyword{VARIMA}% __ONLY ONE__ keyword per line

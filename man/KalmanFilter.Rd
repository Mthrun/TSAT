\name{KalmanFilter}
\alias{KalmanFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kalman Filter
}
\description{
Kalman filtering is also sometimes called “linear quadratic estimation”
}
\usage{
KalmanFilter(Datavector,DLMobject,PlotIt=TRUE,Short=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datavector}{
[1:n], the univariate data set to be analyzed
}
  \item{DLMobject}{
Optional, basically a list with scalar Parameters as defined by dlm::dlm, FF, V,  GG, W,  m0, C0 can be set,  details for Parameter estimation in [Petris, 2010] or in the \code{dlmMLE function} of \code{dlm} package.
}

  \item{PlotIt}{
Default: FALSE, TRUE: Evaluates output of function versus input by plots, works only if Datavector given
}
  \item{Short}{
If PlotIt=TRUE: short or extensive plotting
}
}
\details{
"All filters share a common goal: to let something pass through
while something else does not. An example that many people can relate to is a coffee filter. This
coffee filter will allow the liquid to pass through, while leaving the solid coffee grounds behind.
You can also think about a low-pass filter, which lets low frequencies pass through while
attenuating high frequencies. A Kalman filter also acts as a filter, but its operation is a bit more
complex and harder to understand. A Kalman filter takes in information which is known to have
some error, uncertainty, or noise. The goal of the filter is to take in this imperfect information,
sort out the useful parts of interest, and to reduce the uncertainty or noise."[Rhudy et al., 2017] 

"A Karman Filter can be seen as a Hidden Markov Model based on Gaussian Mixtures" [Ultsch, 2018].
}
\value{
List with
\item{
FilteredData}{[1:n], Filtered Datavector such that noise and uncertainty are reduced.}
\item{FULL}{output of \code{
dlmFilter} of  \code{dlm} package}
\item{ParametersDLM}{DLMobject used in \code{dlmFilter}}
}
\references{
[Rhudy et al., 2017]  Matthew B. Rhudy, Roger A. Salguero and Keaton Holappa: A KALMAN FILTERING TUTORIAL FOR UNDERGRADUATE STUDENTS, International Journal of Computer Science & Engineering Survey (IJCSES) Vol.8, No.1, February, 2017.

[Petris, 2010] Petris, Giovanni: An R Package for Dynamic Linear Models, Journal of Statistical Software,Vol. 36(12), 2010.

[Ultsch, 2018] personal correspondance.
}
\author{
Michael Thrun
}
\note{
 If DLMobject not given the parameters FF, V,  GG, W are set to 1 and the mean 'm0' and standard deviation 'C0' are estimated robustly.
}

\examples{
\donttest{
KalmanFilter(Datavector,PlotIt=TRUE,Short=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{KalmanFilter}% use one of  RShowDoc("KEYWORDS")
\keyword{Kalman}% __ONLY ONE__ keyword per line
\keyword{linear quadratic estimation}
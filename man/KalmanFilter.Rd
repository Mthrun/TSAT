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
KalmanFilter(Datavector,start=0,gam0=1,Fscalar=0.9,gamV=1,G=1,gamW=0.75,PlotIt=TRUE,Short=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datavector}{
%%     ~~Describe \code{TimeSeries} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{gam0}{
%%     ~~Describe \code{gam0} here~~
}
  \item{Fscalar}{
%%     ~~Describe \code{Fscalar} here~~
}
  \item{gamV}{
%%     ~~Describe \code{gamV} here~~
}
  \item{G}{
%%     ~~Describe \code{G} here~~
}
  \item{gamW}{
%%     ~~Describe \code{gamW} here~~
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
}
\value{
List with
\item{
FilteredData}{[1:n], Filtered Datavector such that noise and uncertainty are reduced.}
\item{FULL}{output of \code{
kalman.wge} of packager \code{tswge}}
}
\references{
[Rhudy et al., 2017]  Matthew B. Rhudy, Roger A. Salguero and Keaton Holappa: A KALMAN FILTERING TUTORIAL FOR UNDERGRADUATE STUDENTS, International Journal of Computer Science & Engineering Survey (IJCSES) Vol.8, No.1, February, 2017.
}
\author{
Michael Thrun
}
\note{
in working progress
}


\examples{
\donttest{
require(tswge)
data(table10.1.signal)
data(table10.1.noise)
spn=table10.1.signal+table10.1.noise
KalmanFilter(Datavector=spn,PlotIt=TRUE,Short=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{KalmanFilter}% use one of  RShowDoc("KEYWORDS")
\keyword{Kalman}% __ONLY ONE__ keyword per line
\keyword{linear quadratic estimation}
\name{PlotOptimalAlignment}
\alias{PlotOptimalAlignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Optimal Alignment
}
\description{
Plot Optimal Alignment between two time series
}
\usage{
PlotOptimalAlignment(FirstVector, SecondVector, Type = "threeway", FirstTime, SecondTime, Frequency, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FirstVector}{
[1:n] numerical vector
}
  \item{SecondVector}{
[1:m] numerical vector
}
  \item{Type}{
Optional, Type alignment plot: \code{twoway}, \code{threeway}, \code{density}, \code{alignment}
}
  \item{FirstTime}{
Optional, [1:n] vector, time object of class Date, see \code{\link{ConvertNumerical2TSobject}}
}
  \item{SecondTime}{
Optional, [1:m] vector, Time object of class Date, see \code{\link{ConvertNumerical2TSobject}}
}
  \item{Frequency}{
Optional, either days, weeks, months or quarters or years
}
  \item{\dots}{
Optional, arguments passed on to \code{\link{DTWdistance}}
}
}
\details{
For Plot types see either pictures in [Giorgino, 2009] or \code{\link[dtw]{dtwPlot}} documentation.
}
\value{
Output in invisible mode of \code{\link[dtw]{dtwPlot}}
}
\references{
[Giorgino, 2009]  Giorgino, T.: Computing and visualizing dynamic time warping alignments in R: the dtw package, Journal of statistical Software, Vol. 31(7), pp. 1-24. 2009.

Rabiner, L. R., & Juang, B.-H. (1993). Chapter 4 in Fundamentals of speech recognition. Englewood Cliffs, NJ: Prentice Hall.
}
\author{
Michael Thrun
}


\seealso{
\code{\link{DTWdistance}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PlotOptimalAlignment}% use one of  RShowDoc("KEYWORDS")
\keyword{OptimalAlignment}% __ONLY ONE__ keyword per line

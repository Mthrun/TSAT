\name{SplitPercentageTS}
\alias{SplitPercentageTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split Percentage of Time Series
}
\description{
Splits time series by a given percentage into training test set
}
\usage{
SplitPercentageTS(Datavector, Time, Percentage = 80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datavector}{
[1:n] vector
}
  \item{Time}{
Optional, [1:n] vector
}
  \item{Percentage}{
number between zero and one or one and hundred. Defines \code{k} and \code{m} of output.
}
}
\details{
Percentage is rounded so that Training set is always even-numbered and for the output it is given that \code{n=k+m}
}
\value{
List with
  \item{TrainingSet}{
[1:k] vector, first k entries of \code{Datavector}
}
  \item{TrainingTime}{
Either \code{NULL} or [1:k] vector, first k entries of \code{Time}, if \code{Time} given
}
  \item{TestSet}{
TestSet, [1:m] vector, last m entries of \code{Datavector}
}
  \item{TestTime}{
Either \code{NULL} or [1:m] vector, last m entries of \code{Time}, if \code{Time} given
}
}

\author{
Michael Thrun
}

\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SplitPercentageTS}% use one of  RShowDoc("KEYWORDS")
\keyword{Split}% __ONLY ONE__ keyword per line
\keyword{Training}% 
\keyword{Test}% 
\keyword{Set}% 